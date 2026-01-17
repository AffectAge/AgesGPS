/* =========================================================
   BUILD: ПАРАЛЛЕЛЬНОЕ СТРОИТЕЛЬСТВО
   - Очки строительства из гос-казны: ОчковСтроительства
   - Проекты строятся параллельно
   - Очки распределяются поровну + перераспределение оверфлоу
   - Материалы закупаются через TRADE (append buy orders),
     затем резервируются со склада государства
   - Готовое здание кладём в data.Постройки с cap=10 зданий на ячейку
   ========================================================= */

var BUILD_CFG = {
  CATEGORY: "Постройки",
  SUB_QUEUE: "Очередь строительства",
  SUB_TURN: "Строительство (ход)",
  CELL_BUILDINGS_CAP: 10
};

/* =======================
   A) Append buy orders to TRADE (state as buyer)
   ======================= */

function BUILD_appendConstructionBuyOrders_(ctx) {
  var data = ctx.data;
  if (!data || typeof data !== "object") return;

  // найти очередь
  var qCell = BUILD_findBuildQueueCell_(data);
  if (!qCell) return;
  BUILD_ensureQueueCellShape_(qCell);

  var queue = qCell.ОчередьСтроительства;
  if (!Array.isArray(queue) || !queue.length) return;

  // казна (покупатель)
  var treasury = BUILD_getOrCreateTreasury_(data);
  BUILD_ensureTradeLikeBuyerShape_(treasury);

  // шаблоны
  var TEMPLATES = BUILD_indexTemplates_(data);

  // мы покупаем материалы для "текущего юнита" каждой строки,
  // если ещё не зарезервированы (it._МатериалыЗарезервированы !== true)
  // и если строка не завершена/не заблокирована.
  for (var i = 0; i < queue.length; i++) {
    var it = queue[i];
    if (!it || typeof it !== "object") continue;

    BUILD_ensureQueueItemShape_(it);

    if (it.Статус === "Завершено") continue;
    if (it.Осталось <= 0) { it.Статус = "Завершено"; continue; }

    // если уже зарезервировали материалы на текущий юнит — не покупаем повторно
    if (it._МатериалыЗарезервированы === true) continue;

    var type = String(it.Тип || "").trim();
    var provKey = String(it.Провинция || "").trim();
    if (!type || !provKey) continue;

    var tpl = TEMPLATES[type];
    if (!tpl) continue;

    // материалы: tpl.Стоимость.Товары (как ты описал)
    var needGoods = BUILD_getTemplateNeedGoods_(tpl);
    var goods = Object.keys(needGoods);
    if (!goods.length) continue;

    // рынок провинции (куда импортируем)
    var p = ctx.provByKey ? ctx.provByKey[String(provKey)] : null;
    if (!p) continue;

    var localMarketId = (p.РынокId != null ? String(p.РынокId) : (p.Рынок != null ? String(p.Рынок) : ""));
    localMarketId = String(localMarketId || "").trim();
    if (!localMarketId) continue;

    // инициализировать throughput провинции (строительная закупка тоже "покупатель")
    if (ctx.tpLeftProv && ctx.tpLeftProv[provKey] === undefined) {
      ctx.tpLeftProv[provKey] = Math.floor(Number(p.ПропускнаяСпособность) || 0);
    }

    // для каждого товара формируем buy order на недостачу: сначала пытаемся купить,
    // склад будет использован позже как fallback.
    for (var g = 0; g < goods.length; g++) {
      var good = String(goods[g]);
      var qtyNeed = Math.floor(Number(needGoods[good]) || 0);
      if (qtyNeed <= 0) continue;

      // если на гос-складе уже есть — всё равно "сначала рынок":
      // значит всегда ставим ордер, а остаток будем добирать со склада при резервировании.
      // Но чтобы не гонять лишнее — можем заказать только недостачу к складу 0:
      // здесь делаем именно qtyNeed, чтобы при наличии рынка пополнять склад.
      ctx.orders.buy.push({
        b: treasury,                 // покупатель = казна
        provKey: String(provKey),
        localMarketId: String(localMarketId),
        good: good,
        qtyNeed: qtyNeed,

        // метка (не мешает трейду, но удобно для отладки)
        _kind: "BUILD_MATERIAL",
        _projectId: it.Id || null,
        _buildingType: type
      });
    }
  }
}

/* =======================
   B) Construction points turn (after TRADE)
   ======================= */

function BUILD_runConstructionPointsTurn(data) {
  if (!data || typeof data !== "object") return data;
  ensureNews(data);

  var stateId = getStateIdFromStateData(data);
  if (!stateId) {
    if (typeof pushErrorNotice === "function") pushErrorNotice(data, "STATE_ID_NOT_FOUND", "Идентификатор государства не найден в data['Данные государства'].");
    else BUILD_pushSystemError_(data, "STATE_ID_NOT_FOUND", "Идентификатор государства не найден в data['Данные государства'].");
    return data;
  }

  var qCell = BUILD_findBuildQueueCell_(data);
  if (!qCell) return data;
  BUILD_ensureQueueCellShape_(qCell);

  var queue = qCell.ОчередьСтроительства;
  if (!Array.isArray(queue) || !queue.length) return data;

  // казна (очки + склад)
  var treasury = BUILD_getOrCreateTreasury_(data);
  BUILD_ensureTreasuryShape_(treasury);

  var totalPoints = Math.floor(Number(treasury.ОчковСтроительства) || 0);
  if (totalPoints <= 0) {
    BUILD_pushTurnSummary_(data, { points: 0, active: 0, finished: 0, blocked: 0, waitingMat: 0 });
    return data;
  }

  // Провинции + наши
  var provinces = getAllProvinces(data);
  var ourProvincesMap = {};
  provinces.forEach(function (p) {
    var key = p.Провинция || p.Название || p.id;
    var isOur = key && String(p.Владелец || "") === String(stateId);
    if (isOur) ourProvincesMap[String(key)] = true;
    p._isOur = isOur;
  });

  // Контексты для строгой проверки (размещение/лимиты)
  var STATE_CONTEXT = buildStateContext(data);
  var ACTIVE_BUILDINGS_CONTEXT = buildActiveBuildingsContext(data, ourProvincesMap);

  var TEMPLATES = BUILD_indexTemplates_(data);

  var EXISTING_CTX = BUILD_existingCounts_(data, ourProvincesMap);
  var QUEUE_CTX = BUILDQUEUE_buildContext_(queue);

  var finished = 0, blocked = 0, waitingMat = 0;

  // Список активных "юнитов" (по одной текущей единице на строку)
  var work = [];
  for (var i = 0; i < queue.length; i++) {
    var it = queue[i];
    if (!it || typeof it !== "object") continue;
    BUILD_ensureQueueItemShape_(it);

    if (it.Статус === "Завершено") continue;
    if (it.Осталось <= 0) { it.Статус = "Завершено"; continue; }

    // 1) строгая проверка размещения (на 1 юнит) — если не ок, блокируем строку
    var type = String(it.Тип || "").trim();
    var provName = String(it.Провинция || "").trim();
    var tpl = TEMPLATES[type] || null;
    var prov = findProvince(provinces, provName) || null;

    var decision = BUILD_checkPlacementStrict_(data, {
      raw: "BUILD_POINTS_TURN",
      type: type,
      provinceName: provName,
      qty: 1,
      tpl: tpl,
      prov: prov,
      stateId: stateId,
      STATE_CONTEXT: STATE_CONTEXT,
      ACTIVE_BUILDINGS_CONTEXT: ACTIVE_BUILDINGS_CONTEXT,
      EXISTING_CTX: EXISTING_CTX,
      QUEUE_CTX: QUEUE_CTX
    });

    if (!decision.ok) {
      it.Статус = "Заблокировано";
      it._МатериалыЗарезервированы = false;
      blocked++;

      BUILD_pushProjectNotice_(data, {
        ok: false,
        title: "Проект заблокирован (критерии/лимиты)",
        it: it,
        reasonsParts: decision.reasonsParts || []
      });
      continue;
    }

    // 2) резервируем материалы (рынок уже попытался купить через TRADE; остаток берём со склада)
    var needGoods = tpl ? BUILD_getTemplateNeedGoods_(tpl) : {};
    var reserve = BUILD_tryReserveMaterials_(treasury, needGoods);
    if (!reserve.ok) {
      it.Статус = "ОжидаетМатериалы";
      it._МатериалыЗарезервированы = false;
      waitingMat++;

      BUILD_pushProjectNotice_(data, {
        ok: false,
        title: "Проект ждёт материалы",
        it: it,
        reasonsParts: reserve.reasonsParts || []
      });
      continue;
    }

    it._МатериалыЗарезервированы = true;
    it.Статус = "Строится";

    // 3) очки на юнит
    var needPoints = BUILD_getTemplateNeedPoints_(tpl);
    it.НужноОчков = needPoints;
    if (it.ПрогрессОчков == null) it.ПрогрессОчков = 0;

    work.push(it);
  }

  // Распределение очков с перераспределением оверфлоу
  var pointsLeft = totalPoints;

  while (pointsLeft > 0 && work.length > 0) {
    var n = work.length;

    // "поровну" — базовая доля
    var share = Math.floor(pointsLeft / n);
    if (share <= 0) share = 1;

    var newWork = [];
    for (var k = 0; k < work.length && pointsLeft > 0; k++) {
      var it2 = work[k];

      var needRemain = Math.max(0, Math.floor(Number(it2.НужноОчков) || 0) - Math.floor(Number(it2.ПрогрессОчков) || 0));
      if (needRemain <= 0) {
        // теоретически уже завершён — обработаем ниже
      } else {
        var give = Math.min(share, pointsLeft, needRemain);
        give = Math.floor(give);
        if (give > 0) {
          it2.ПрогрессОчков = Math.floor(Number(it2.ПрогрессОчков) || 0) + give;
          pointsLeft -= give;
        }
      }

      // если добили — завершаем 1 здание, и лишние очки уже остались в pointsLeft (мы их не тратили)
      var doneNow = (Math.floor(Number(it2.ПрогрессОчков) || 0) >= Math.floor(Number(it2.НужноОчков) || 0));
      if (doneNow) {
        // завершение 1 здания
        var created = BUILD_finishOneUnit_(data, it2, TEMPLATES);
        if (created.ok) {
          finished++;

          // контексты лимитов: существующие +1, очередь -1
          BUILDQUEUE_applyToContext_(QUEUE_CTX, String(it2.Тип), String(it2.Провинция), -1); // если твой applyToContext_ не поддерживает -, удали строку
          // безопасно обновим EXISTING_CTX вручную:
          var t = String(it2.Тип), pkey = String(it2.Провинция);
          EXISTING_CTX.Мир[t] = (EXISTING_CTX.Мир[t] || 0) + 1;
          EXISTING_CTX.Государство[t] = (EXISTING_CTX.Государство[t] || 0) + 1;
          if (!EXISTING_CTX.Провинция[pkey]) EXISTING_CTX.Провинция[pkey] = {};
          EXISTING_CTX.Провинция[pkey][t] = (EXISTING_CTX.Провинция[pkey][t] || 0) + 1;

          it2.Осталось -= 1;
          it2.Построено += 1;

          // сброс прогресса на следующий юнит этой строки
          it2.ПрогрессОчков = 0;
          it2._МатериалыЗарезервированы = false;

          if (it2.Осталось <= 0) {
            it2.Статус = "Завершено";
          } else {
            it2.Статус = "Ожидает"; // следующий юнит будет снова пытаться резервировать материалы на следующем ходе
          }

          BUILD_pushProjectNotice_(data, {
            ok: true,
            title: "Здание завершено",
            it: it2,
            reasonsParts: []
          });

        } else {
          it2.Статус = "Заблокировано";
          it2._МатериалыЗарезервированы = false;
          blocked++;

          BUILD_pushProjectNotice_(data, {
            ok: false,
            title: "Не удалось создать здание",
            it: it2,
            reasonsParts: created.reasonsParts || []
          });
        }
      } else {
        // остаётся в работе на следующий цикл распределения
        newWork.push(it2);
      }
    }

    work = newWork;
  }

  // чистим временные
  provinces.forEach(function (p) { delete p._isOur; });

  // удалить завершённые проекты из очереди
BUILD_cleanupFinishedQueueItems_(queue);

// отчёт по провинциям (вариант 3)
BUILD_pushActiveProjectsByProvince_(data, queue, {
  maxProvinces: 12,
  maxItemsPerProvince: 8,
  maxTotalItems: 40
});

BUILD_pushTurnSummary_(data, {
  points: totalPoints,
  active: work.length,
  finished: finished,
  blocked: blocked,
  waitingMat: waitingMat,
  queueLeft: queue.length
});

BUILD_pushTurnSummary_(data, {
  points: totalPoints,
  active: work.length,
  finished: finished,
  blocked: blocked,
  waitingMat: waitingMat,
  queueLeft: queue.length
});

return data;
}

/* =======================
   Helpers: templates
   ======================= */

function BUILD_indexTemplates_(data) {
  var T = {};
  normalizeToArray(data["Шаблоны зданий"]).forEach(function (row) {
    normalizeToArray(row).forEach(function (t) {
      if (t && t.Тип) T[String(t.Тип)] = t;
    });
  });
  return T;
}

// очки на 1 здание: tpl.НужноОчковСтроительства (или tpl.ОчковСтроительства)
function BUILD_getTemplateNeedPoints_(tpl) {
  var v = 0;
  if (tpl) {
    if (tpl.НужноОчковСтроительства != null) v = Number(tpl.НужноОчковСтроительства);
    else if (tpl.ОчковСтроительства != null) v = Number(tpl.ОчковСтроительства);
    else if (tpl.ТребуетсяОчковСтроительства != null) v = Number(tpl.ТребуетсяОчковСтроительства);
  }
  v = Math.floor(v || 0);
  return v > 0 ? v : 100; // дефолт
}

// материалы на 1 здание: tpl.Стоимость.Товары
function BUILD_getTemplateNeedGoods_(tpl) {
  var out = {};
  if (!tpl || !tpl.Стоимость || typeof tpl.Стоимость !== "object") return out;
  var g = tpl.Стоимость.Товары;
  if (!g || typeof g !== "object" || Array.isArray(g)) return out;
  Object.keys(g).forEach(function (k) {
    var q = Math.floor(Number(g[k]) || 0);
    if (q > 0) out[String(k)] = q;
  });
  return out;
}

/* =======================
   Treasury: create if missing
   ======================= */

function BUILD_getOrCreateTreasury_(data) {
  var rows = normalizeToArray(data["Данные государства"]);
  if (!Array.isArray(data["Данные государства"])) data["Данные государства"] = rows;

  // 1) ищем объект с Деньги/Бюджет/Склад/ОчковСтроительства
  for (var i = 0; i < rows.length; i++) {
    var row = normalizeToArray(rows[i]); rows[i] = row;
    for (var j = 0; j < row.length; j++) {
      var cell = row[j];
      if (!cell || typeof cell !== "object" || Array.isArray(cell)) continue;

      if (Object.prototype.hasOwnProperty.call(cell, "Деньги") ||
          Object.prototype.hasOwnProperty.call(cell, "Бюджет") ||
          Object.prototype.hasOwnProperty.call(cell, "Склад") ||
          Object.prototype.hasOwnProperty.call(cell, "ОчковСтроительства")) {
        BUILD_ensureTreasuryShape_(cell);
        return cell;
      }
    }
  }

  // 2) иначе создаём в первой свободной ячейке
  for (var r = 0; r < rows.length; r++) {
    var row2 = normalizeToArray(rows[r]); rows[r] = row2;
    for (var c = 0; c < row2.length; c++) {
      if (row2[c] === "" || row2[c] == null) {
        var created = { Деньги: 0, Бюджет: 0, Склад: {}, ОчковСтроительства: 0 };
        row2[c] = created;
        return created;
      }
    }
  }

  // 3) если нет пустых — добавим строку
  var createdLast = { Деньги: 0, Бюджет: 0, Склад: {}, ОчковСтроительства: 0 };
  data["Данные государства"].push([createdLast]);
  return createdLast;
}

function BUILD_ensureTreasuryShape_(t) {
  if (!t || typeof t !== "object" || Array.isArray(t)) return;

  // синхронизация Бюджет <-> Деньги (Деньги — канонический для трейда)
  if (t.Деньги == null || isNaN(Number(t.Деньги))) t.Деньги = (Number(t.Бюджет) || 0);
  if (t.Бюджет == null || isNaN(Number(t.Бюджет))) t.Бюджет = Number(t.Деньги) || 0;

  if (!t.Склад || typeof t.Склад !== "object" || Array.isArray(t.Склад)) t.Склад = {};
  if (t.ОчковСтроительства == null || isNaN(Number(t.ОчковСтроительства))) t.ОчковСтроительства = 0;
}

// чтобы казна могла участвовать в TRADE_matchFromSellerList_
function BUILD_ensureTradeLikeBuyerShape_(b) {
  BUILD_ensureTreasuryShape_(b);
  if (b.ПрибыльЗаХод == null || isNaN(Number(b.ПрибыльЗаХод))) b.ПрибыльЗаХод = 0;
  if (b.РасходЗаХод == null || isNaN(Number(b.РасходЗаХод))) b.РасходЗаХод = 0;
  if (b.Пошлины == null || isNaN(Number(b.Пошлины))) b.Пошлины = 0;

  // торговля читает buyer.Деньги и buyer.Склад
  // (мы уже гарантировали)
}

/* =======================
   Queue cell/item
   ======================= */

function BUILD_findBuildQueueCell_(data) {
  var rows = normalizeToArray(data["Данные государства"]);
  for (var i = 0; i < rows.length; i++) {
    var row = normalizeToArray(rows[i]);
    for (var j = 0; j < row.length; j++) {
      var cell = row[j];
      if (!cell || typeof cell !== "object" || Array.isArray(cell)) continue;
      if (Array.isArray(cell.ОчередьСтроительства)) return cell;
    }
  }
  return null;
}

function BUILD_ensureQueueCellShape_(cell) {
  if (!cell || typeof cell !== "object" || Array.isArray(cell)) return;
  if (!Array.isArray(cell.ОчередьСтроительства)) cell.ОчередьСтроительства = [];
  if (typeof cell.СчётчикСтроек !== "number") cell.СчётчикСтроек = 0;
}

function BUILD_ensureQueueItemShape_(it) {
  if (it.Осталось == null) it.Осталось = Math.max(0, Math.floor(Number(it.Количество) || 0));
  if (it.Построено == null) it.Построено = 0;
  if (!it.Статус) it.Статус = "Ожидает";
  if (it.ПрогрессОчков == null) it.ПрогрессОчков = 0;
  if (it.НужноОчков == null) it.НужноОчков = 0;
  if (it._МатериалыЗарезервированы == null) it._МатериалыЗарезервированы = false;
}

/* =======================
   Materials reservation (state stock fallback)
   ======================= */

function BUILD_tryReserveMaterials_(treasury, needGoods) {
  var reasons = [];
  var stock = treasury.Склад;

  var keys = Object.keys(needGoods || {});
  for (var i = 0; i < keys.length; i++) {
    var g = keys[i];
    var need = Math.floor(Number(needGoods[g]) || 0);
    if (need <= 0) continue;

    var have = Math.floor(Number(stock[g]) || 0);

    // рынок уже пытался купить; если не купили — смотрим склад
    if (have < need) {
      reasons.push({
        titleParts: makeTitleParts("Материал: ", g),
        exp: (function () {
          var p = [];
          uiPrefix(p, indent(1), false);
          uiText(p, "Нужно: "); uiVal(p, need); uiText(p, ", на складе: "); uiVal(p, have); uiNL(p);
          uiText(p, "На рынке не удалось купить достаточно (клиринг)."); uiNL(p);
          return { ok: false, parts: p };
        })()
      });
      return { ok: false, reasonsParts: reasons };
    }
  }

  // списываем со склада (резерв/потребление)
  for (var j = 0; j < keys.length; j++) {
    var g2 = keys[j];
    var need2 = Math.floor(Number(needGoods[g2]) || 0);
    if (need2 <= 0) continue;
    stock[g2] = Math.floor(Number(stock[g2]) || 0) - need2;
    if (stock[g2] < 0) stock[g2] = 0;
  }

  return { ok: true, reasonsParts: [] };
}

/* =======================
   Finish: create one building + put to data.Постройки (cap=10 per cell)
   ======================= */

function BUILD_finishOneUnit_(data, it, templates) {
  var reasons = [];

  var type = String(it.Тип || "").trim();
  var prov = String(it.Провинция || "").trim();
  if (!type || !prov) {
    reasons.push({
      titleParts: makePlainTitleParts("Данные проекта"),
      exp: (function () {
        var p = [];
        uiPrefix(p, indent(1), false);
        uiText(p, "Нет Тип/Провинция."); uiNL(p);
        return { ok: false, parts: p };
      })()
    });
    return { ok: false, reasonsParts: reasons };
  }

  var tpl = templates[type] || null;
  if (!tpl) {
    reasons.push({
      titleParts: makeTitleParts("Шаблон постройки ", type),
      exp: (function () {
        var p = [];
        uiPrefix(p, indent(1), false);
        uiText(p, "Шаблон не найден."); uiNL(p);
        return { ok: false, parts: p };
      })()
    });
    return { ok: false, reasonsParts: reasons };
  }

  var b = {
    Тип: type,
    Провинция: prov,
    Активно: true,

    // для твоей торговли (без fallback)
    Входы: BUILD_copyPlainObject_(tpl.Входы),
    Выходы: BUILD_copyPlainObject_(tpl.Выходы),

    Склад: {},
    Нехватка: {},
    Деньги: 0,
    ПрибыльЗаХод: 0,
    РасходЗаХод: 0,
    Пошлины: 0,

    ХодСтроительства: (typeof data.Ход === "number" ? data.Ход : null),
    Источник: (it.Id ? String(it.Id) : "CONSTRUCTION")
  };

  if (!Array.isArray(data.Постройки)) data.Постройки = [];
  BUILD_putBuildingWithCellCap_(data.Постройки, b, BUILD_CFG.CELL_BUILDINGS_CAP);

  return { ok: true, building: b, reasonsParts: [] };
}

function BUILD_copyPlainObject_(o) {
  var out = {};
  if (!o || typeof o !== "object" || Array.isArray(o)) return out;
  Object.keys(o).forEach(function (k) { out[String(k)] = o[k]; });
  return out;
}

/* =======================
   Put building into column with cap=10 per cell (1D or 2D)
   ======================= */

function BUILD_putBuildingWithCellCap_(col, b, cap) {
  cap = Math.max(1, Math.floor(Number(cap) || 10));

  // 2D
  if (Array.isArray(col) && col.length && Array.isArray(col[0])) {
    for (var r = 0; r < col.length; r++) {
      var row = col[r];
      if (!Array.isArray(row)) { col[r] = []; row = col[r]; }

      for (var c = 0; c < row.length; c++) {
        var cell = row[c];

        // empty
        if (cell === "" || cell == null) {
          row[c] = [b];
          return;
        }

        // array cell (preferred)
        if (Array.isArray(cell)) {
          if (cell.length < cap) { cell.push(b); return; }
          continue;
        }

        // single object -> convert to array if possible
        if (typeof cell === "object") {
          if (cap >= 2) { row[c] = [cell, b]; return; }
        }
      }
    }

    // no space -> add new row
    col.push([[b]]);
    return;
  }

  // 1D
  for (var i = 0; i < col.length; i++) {
    var cell1 = col[i];

    if (cell1 === "" || cell1 == null) {
      col[i] = [b];
      return;
    }

    if (Array.isArray(cell1)) {
      if (cell1.length < cap) { cell1.push(b); return; }
      continue;
    }

    if (typeof cell1 === "object") {
      if (cap >= 2) { col[i] = [cell1, b]; return; }
    }
  }

  // no space -> append
  col.push([b]);
}

/* =======================
   News
   ======================= */

function BUILD_pushProjectNotice_(data, info) {
  var parts = [];
  var ok = !!info.ok;
  var it = info.it || {};

  uiTitle(parts, info.title || "Строительство", ok ? UI.BORDER : UI.BAD);
  uiTop(parts, ok ? UI.BORDER : UI.BAD);

  uiRow(parts, "Проект", it.Id || "—", UI.VALUE, ok ? UI.BORDER : UI.BAD);
  uiRow(parts, "Здание", it.Тип || "—", UI.VALUE, ok ? UI.BORDER : UI.BAD);
  uiRow(parts, "Провинция", it.Провинция || "—", UI.VALUE, ok ? UI.BORDER : UI.BAD);

  uiRow(parts, "Статус", it.Статус || "—", ok ? UI.OK : UI.BAD, ok ? UI.BORDER : UI.BAD);
  uiRow(parts, "Прогресс", String(it.ПрогрессОчков || 0) + " / " + String(it.НужноОчков || 0), UI.VALUE, ok ? UI.BORDER : UI.BAD);
  uiRow(parts, "Осталось", String(it.Осталось || 0), UI.VALUE, ok ? UI.BORDER : UI.BAD);

  if (!ok) {
    var reasons = info.reasonsParts || [];
    uiRow(parts, "Причины", String(reasons.length), UI.VALUE, UI.BAD);

    reasons.forEach(function (block) {
      uiBlank(parts, UI.BAD);
      if (block.titleParts && block.titleParts.length) parts = parts.concat(block.titleParts);
      else parts = parts.concat(makePlainTitleParts(block.title || "Причина"));
      if (block.exp && block.exp.parts && block.exp.parts.length) parts = parts.concat(block.exp.parts);
    });
  }

  uiBottom(parts, ok ? UI.BORDER : UI.BAD);

  pushBoxNotice(data, {
    category: BUILD_CFG.CATEGORY,
    sub: BUILD_CFG.SUB_QUEUE,
    priority: ok ? 220 : 900,
    parts: parts
  });
}

function BUILD_pushTurnSummary_(data, s) {
  var parts = [];
  uiTitle(parts, "Строительство: распределение очков", UI.BORDER);
  uiTop(parts, UI.BORDER);

  uiRow(parts, "Очков строительства", String(s.points || 0), UI.VALUE, UI.BORDER);
  uiRow(parts, "Завершено зданий", String(s.finished || 0), UI.VALUE, UI.BORDER);
  uiRow(parts, "Заблокировано", String(s.blocked || 0), UI.VALUE, UI.BORDER);
  uiRow(parts, "Ждёт материалов", String(s.waitingMat || 0), UI.VALUE, UI.BORDER);
  uiRow(parts, "В очереди осталось", String(s.queueLeft || 0), UI.VALUE, UI.BORDER);

  uiBottom(parts, UI.BORDER);

  pushBoxNotice(data, {
    category: BUILD_CFG.CATEGORY,
    sub: BUILD_CFG.SUB_TURN,
    priority: 180,
    parts: parts
  });
}

function BUILD_pushSystemError_(data, code, message) {
  var parts = [];
  uiTitle(parts, "Ошибка", UI.BAD);
  uiTop(parts, UI.BAD);
  uiRow(parts, "Код", code, UI.VALUE, UI.BAD);
  uiRow(parts, "Причина", message, UI.VALUE, UI.BAD);
  uiBottom(parts, UI.BAD);

  pushBoxNotice(data, {
    category: "Система",
    sub: "Строительство",
    priority: 999,
    parts: parts
  });
}

function BUILD_cleanupFinishedQueueItems_(queue) {
  if (!Array.isArray(queue) || !queue.length) return;

  // удаляем завершённые и пустые
  for (var i = queue.length - 1; i >= 0; i--) {
    var it = queue[i];
    if (!it || typeof it !== "object") { queue.splice(i, 1); continue; }

    // нормализуем, чтобы Осталось было корректным
    BUILD_ensureQueueItemShape_(it);

    var done = (it.Статус === "Завершено") || (Number(it.Осталось) <= 0);
    if (done) queue.splice(i, 1);
  }
}

/* =========================================================
   BUILD: UI — СТРОЯЩИЕСЯ ЗДАНИЯ ПО ПРОВИНЦИЯМ (ВАРИАНТ 3)
   - 1 сообщение на ход
   - группировка: Провинция -> список проектов
   - показывает: Id, Тип, Кол-во/Построено/Осталось, Прогресс, Статус
   ========================================================= */

function BUILD_pushActiveProjectsByProvince_(data, queue, opts) {
  if (!data || typeof data !== "object") return;
  if (!Array.isArray(queue) || !queue.length) return;

  opts = opts || {};
  var maxProvinces = Math.max(1, Math.floor(Number(opts.maxProvinces) || 12));
  var maxItemsPerProvince = Math.max(1, Math.floor(Number(opts.maxItemsPerProvince) || 8));
  var maxTotalItems = Math.max(1, Math.floor(Number(opts.maxTotalItems) || 40));

  // 1) Собрать активные элементы (не завершены, не пустые)
  var active = [];
  for (var i = 0; i < queue.length; i++) {
    var it = queue[i];
    if (!it || typeof it !== "object") continue;

    // нормализация
    if (typeof BUILD_ensureQueueItemShape_ === "function") BUILD_ensureQueueItemShape_(it);

    var left = Math.floor(Number(it.Осталось) || 0);
    if (it.Статус === "Завершено" || left <= 0) continue;

    active.push(it);
  }

  if (!active.length) return;

  // 2) Группировка по провинциям
  var byProv = {};
  for (var k = 0; k < active.length; k++) {
    var it2 = active[k];
    var prov = String(it2.Провинция || "—").trim() || "—";
    if (!byProv[prov]) byProv[prov] = [];
    byProv[prov].push(it2);
  }

  // 3) Сортировки: провинции по кол-ву проектов, внутри по % прогресса (desc)
  var provKeys = Object.keys(byProv);
  provKeys.sort(function (a, b) {
    var da = byProv[a].length, db = byProv[b].length;
    if (db !== da) return db - da;
    return String(a).localeCompare(String(b));
  });

  for (var pk = 0; pk < provKeys.length; pk++) {
    var list = byProv[provKeys[pk]];
    list.sort(function (x, y) {
      var px = BUILD_progressPct_(x), py = BUILD_progressPct_(y);
      if (py !== px) return py - px;
      return String(x.Id || "").localeCompare(String(y.Id || ""));
    });
  }

  // 4) Рендер сообщения
  var parts = [];
  uiTitle(parts, "Строительство: по провинциям", UI.BORDER);
  uiTop(parts, UI.BORDER);

  uiRow(parts, "Активных строк", String(active.length), UI.VALUE, UI.BORDER);
  uiRow(parts, "Провинций", String(provKeys.length), UI.VALUE, UI.BORDER);

  uiBlank(parts, UI.BORDER);

  var shownTotal = 0;
  var shownProv = 0;

  for (var p = 0; p < provKeys.length; p++) {
    if (shownProv >= maxProvinces) break;
    if (shownTotal >= maxTotalItems) break;

    var provName = provKeys[p];
    var items = byProv[provName] || [];
    if (!items.length) continue;

    // Заголовок провинции
    // (делаем строкой, чтобы выглядело как секция)
    uiText(parts, "┃ "); uiText(parts, String(provName), UI.LABEL); uiText(parts, ": ");
    uiVal(parts, String(items.length)); uiNL(parts);

    var shownHere = 0;
    for (var q = 0; q < items.length; q++) {
      if (shownHere >= maxItemsPerProvince) break;
      if (shownTotal >= maxTotalItems) break;

      var it3 = items[q];

      var id = it3.Id || "—";
      var type = it3.Тип || "—";

      var qty = Math.floor(Number(it3.Количество) || 0);
      var built = Math.floor(Number(it3.Построено) || 0);
      var left = Math.floor(Number(it3.Осталось) || 0);

      var need = Math.floor(Number(it3.НужноОчков) || 0);
      var prog = Math.floor(Number(it3.ПрогрессОчков) || 0);
      var pct = BUILD_progressPct_(it3);

      var status = String(it3.Статус || "—");
      var statusColor =
        (status === "Строится") ? (UI.OK || UI.VALUE) :
        (status === "ОжидаетМатериалы") ? UI.BAD :
        (status === "Заблокировано") ? UI.BAD :
        UI.VALUE;

      // Строка проекта (одна строка, компактная)
      // ┃  ➔ Тип (ID) | Кол-во 3 | Постр 2 | Ост 1 | 45/100 (45%) | Статус
      uiPrefix(parts, indent(1), false);
      uiText(parts, "➔ ");
      uiText(parts, String(type), UI.VALUE);
      uiText(parts, " ("); uiText(parts, String(id), UI.DIM); uiText(parts, ") | ");

      uiText(parts, "Кол-во "); uiVal(parts, String(qty)); uiText(parts, " | ");
      uiText(parts, "Постр "); uiVal(parts, String(built)); uiText(parts, " | ");
      uiText(parts, "Ост "); uiVal(parts, String(left)); uiText(parts, " | ");

      uiText(parts, "Прог "); uiVal(parts, String(prog)); uiText(parts, "/"); uiVal(parts, String(need));
      uiText(parts, " ("); uiVal(parts, String(pct)); uiText(parts, "%) | ");

      uiText(parts, "Статус "); uiText(parts, status, statusColor);

      uiNL(parts);

      shownHere++;
      shownTotal++;
    }

    // Если обрезали внутри провинции — сообщим
    if (items.length > shownHere) {
      uiPrefix(parts, indent(2), false);
      uiText(parts, "… ещё "); uiVal(parts, String(items.length - shownHere));
      uiText(parts, " проект(ов) в этой провинции"); uiNL(parts);
    }

    uiBlank(parts, UI.BORDER);
    shownProv++;
  }

  // Если обрезали провинции — сообщим
  if (provKeys.length > shownProv) {
    uiRow(parts, "Обрезка", "Провинций скрыто: " + String(provKeys.length - shownProv), UI.VALUE, UI.BORDER);
  }
  if (shownTotal < active.length) {
    uiRow(parts, "Обрезка", "Проектов скрыто: " + String(active.length - shownTotal), UI.VALUE, UI.BORDER);
  }

  uiBottom(parts, UI.BORDER);

  pushBoxNotice(data, {
    category: BUILD_CFG.CATEGORY,
    sub: "Строящиеся здания (по провинциям)",
    priority: 170,
    parts: parts
  });
}

function BUILD_progressPct_(it) {
  var need = Math.floor(Number(it && it.НужноОчков) || 0);
  var prog = Math.floor(Number(it && it.ПрогрессОчков) || 0);
  if (need <= 0) return 0;
  var pct = Math.floor((prog * 100) / need);
  if (pct < 0) pct = 0;
  if (pct > 100) pct = 100;
  return pct;
}