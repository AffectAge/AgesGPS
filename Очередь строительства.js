/* =========================================================
   ORDERS: ТЕКСТОВЫЕ ПРИКАЗЫ НА СТРОИТЕЛЬСТВО -> ОЧЕРЕДЬ СТРОИТЕЛЬСТВА
   Google Apps Script (V8)

   ТВОИ УСЛОВИЯ:
   ✅ Приказы = текстовые строки в data.Приказы (1 столбец / 1D или 2D)
   ✅ Шаблон приказа:
      Построить [Кирпичный завод] в провинции [Неизвестная провинция 1] в количестве [3]
   ✅ СТРОГО: если не подходит -> отклонить
   ✅ ОчередьСтроительства хранить в ОТДЕЛЬНОЙ свободной ячейке data["Данные государства"]
      (БЕЗ проверок/использования "Идентификатор государства" для очереди)
   ✅ Текст приказа заменить на:
      "✅ Приказ принят (CON-000001)" или "⛔ Приказ отклонён"
   ✅ Причины — в новостях (твой формат "ячейки" через parts + pushNotice)

   =========================================================
   ЗАВИСИМОСТИ (ДОЛЖНЫ БЫТЬ В ПРОЕКТЕ УЖЕ):
   - normalizeToArray(value)
   - ensureNews(data)
   - pushNotice(data, {category, sub, priority, parts})
   - UI, uiTitle/uiTop/uiRow/uiBlank/uiBottom/uiPrefix/uiText/uiVal/uiNL
   - pushBoxNotice(data, opts)
   - pushErrorNotice(data, code, message)   (если нет — можешь заменить на pushBoxNotice)
   - indent(level)
   - evaluateRule(rule, value)
   - explainRuleParts(rule, value, level, invert)
   - checkProvinceCriteriaParts(province, criteria)
   - checkStateCriteriaParts(stateCtx, criteria)
   - checkStatePropertyCriteriaParts(stateCtx, criteria, title)
   - checkFactionCriteriaParts(stateCtx, criteria)
   - checkBuildingCriteriaParts(rule, ctx, level, invert)
   - getStateIdFromStateData(data)
   - buildStateContext(data)
   - buildActiveBuildingsContext(data, ourProvincesMap)
   - getAllProvinces(data), findProvince(all, key)

   ========================================================= */

/* =======================
   REGEX приказа
   ======================= */

var ORDERS_RE_BUILD = /^\s*Построить\s*\[(.+?)\]\s*в\s*провинции\s*\[(.+?)\]\s*в\s*количестве\s*\[(\d+)\]\s*$/i;

/* =======================
   PUBLIC API
   ======================= */

function ORDERS_processBuildOrders(data) {
  ensureNews(data);

  var stateId = getStateIdFromStateData(data);
  if (!stateId) {
    if (typeof pushErrorNotice === "function") {
      pushErrorNotice(data, "STATE_ID_NOT_FOUND", "Идентификатор государства не найден в data['Данные государства'].");
    } else {
      ORDERS_pushSimpleError_(data, "STATE_ID_NOT_FOUND", "Идентификатор государства не найден в data['Данные государства'].");
    }
    return data;
  }

  // Провинции + наши
  var provinces = getAllProvinces(data);
  var ourProvincesMap = {};
  provinces.forEach(function (p) {
    var key = p.Провинция || p.Название || p.id;
    var isOur = key && String(p.Владелец || "") === String(stateId);
    if (isOur) ourProvincesMap[key] = true;
    p._isOur = isOur;
  });

  // Контексты
  var STATE_CONTEXT = buildStateContext(data);
  var ACTIVE_BUILDINGS_CONTEXT = buildActiveBuildingsContext(data, ourProvincesMap);

  // Шаблоны
  var TEMPLATES = {};
  normalizeToArray(data["Шаблоны зданий"]).forEach(function (row) {
    normalizeToArray(row).forEach(function (t) {
      if (t && t.Тип) TEMPLATES[t.Тип] = t;
    });
  });

  // Очередь строительства: отдельная свободная ячейка в "Данные государства" (без ID)
  var queueCell = STATE_findOrCreateBuildQueueCell_(data);
  STATE_ensureBuildQueueCell_(queueCell);

  // Контексты для лимитов: существующие + очередь
  var EXISTING_CTX = BUILD_existingCounts_(data, ourProvincesMap);
  var QUEUE_CTX = BUILDQUEUE_buildContext_(queueCell.ОчередьСтроительства || []);

  // Обработка приказов: поддержка 1D и 2D
  if (!data.Приказы) data.Приказы = [];

  ORDERS_forEachTextCell_(data.Приказы, function (getText, setText) {
    var txt = getText();
    if (typeof txt !== "string") return;

    // уже обработано
    if (/^\s*(✅|⛔)\s*Приказ\s*/.test(txt)) return;

    var m = txt.match(ORDERS_RE_BUILD);
    if (!m) return;

    var buildType = String(m[1] || "").trim();
    var provName  = String(m[2] || "").trim();
    var qty       = Math.max(1, parseInt(m[3], 10) || 1);

    var tpl  = TEMPLATES[buildType] || null;
    var prov = findProvince(provinces, provName) || null;

    var decision = BUILD_checkPlacementStrict_(data, {
      raw: txt,
      type: buildType,
      provinceName: provName,
      qty: qty,
      tpl: tpl,
      prov: prov,
      stateId: stateId,
      STATE_CONTEXT: STATE_CONTEXT,
      ACTIVE_BUILDINGS_CONTEXT: ACTIVE_BUILDINGS_CONTEXT,
      EXISTING_CTX: EXISTING_CTX,
      QUEUE_CTX: QUEUE_CTX
    });

    if (decision.ok) {
      var conId = STATE_nextConstructionId_(queueCell);

      queueCell.ОчередьСтроительства.push({
        Id: conId,
        Тип: buildType,
        Провинция: provName,
        Количество: qty,
        Статус: "Ожидает",
        ХодСоздания: (typeof data.Ход === "number" ? data.Ход : null)
      });

      // чтобы следующие приказы учитывали лимиты очереди
      BUILDQUEUE_applyToContext_(QUEUE_CTX, buildType, provName, qty);

      setText("✅ Приказ принят (" + conId + ")");

      ORDERS_pushBuildOrderNotice_(data, {
        ok: true,
        id: conId,
        type: buildType,
        province: provName,
        qty: qty,
        reasonsParts: []
      });

    } else {
      setText("⛔ Приказ отклонён");

      ORDERS_pushBuildOrderNotice_(data, {
        ok: false,
        id: null,
        type: buildType,
        province: provName,
        qty: qty,
        reasonsParts: decision.reasonsParts || []
      });
    }
  });

  provinces.forEach(function (p) { delete p._isOur; });
  return data;
}

/* =======================
   Итерация по "ячейкам" приказов (1D/2D)
   ======================= */

function ORDERS_forEachTextCell_(orders, fn) {
  // 2D: [ [..], [..] ]
  if (Array.isArray(orders) && orders.length && Array.isArray(orders[0])) {
    for (var r = 0; r < orders.length; r++) {
      var row = orders[r];
      if (!Array.isArray(row)) continue;
      for (var c = 0; c < row.length; c++) {
        (function (rr, cc) {
          fn(
            function () { return orders[rr][cc]; },
            function (v) { orders[rr][cc] = v; }
          );
        })(r, c);
      }
    }
    return;
  }

  // 1D: [ "строка", "строка", ... ]
  if (Array.isArray(orders)) {
    for (var i = 0; i < orders.length; i++) {
      (function (idx) {
        fn(
          function () { return orders[idx]; },
          function (v) { orders[idx] = v; }
        );
      })(i);
    }
  }
}

/* =======================
   STATE: отдельная свободная ячейка для очереди (БЕЗ ID)
   ======================= */

function STATE_findOrCreateBuildQueueCell_(data) {
  var rows = normalizeToArray(data["Данные государства"]);
  if (!Array.isArray(data["Данные государства"])) data["Данные государства"] = rows;

  // 1) найдём существующую очередь
  for (var i = 0; i < rows.length; i++) {
    var row = normalizeToArray(rows[i]);
    rows[i] = row;

    for (var j = 0; j < row.length; j++) {
      var cell = row[j];
      if (!cell || typeof cell !== "object" || Array.isArray(cell)) continue;
      if (Array.isArray(cell.ОчередьСтроительства)) return cell;
    }
  }

  // 2) найдём свободную ячейку
  for (var r = 0; r < rows.length; r++) {
    var row2 = normalizeToArray(rows[r]);
    rows[r] = row2;

    for (var c = 0; c < row2.length; c++) {
      var v = row2[c];
      if (v === "" || v === null || v === undefined) {
        var created = {
          ОчередьСтроительства: [],
          СчётчикСтроек: 0
        };
        row2[c] = created;
        return created;
      }
    }
  }

  // 3) если свободных ячеек нет — добавим строку
  var createdLast = {
    ОчередьСтроительства: [],
    СчётчикСтроек: 0
  };
  data["Данные государства"].push([createdLast]);
  return createdLast;
}

function STATE_ensureBuildQueueCell_(cell) {
  if (!cell || typeof cell !== "object" || Array.isArray(cell)) return;
  if (!Array.isArray(cell.ОчередьСтроительства)) cell.ОчередьСтроительства = [];
  if (typeof cell.СчётчикСтроек !== "number") cell.СчётчикСтроек = 0;
}

function STATE_nextConstructionId_(queueCell) {
  STATE_ensureBuildQueueCell_(queueCell);
  queueCell.СчётчикСтроек++;
  return "CON-" + String(queueCell.СчётчикСтроек).padStart(6, "0");
}

/* =======================
   Счётчики существующих построек (для лимитов)
   ======================= */

function BUILD_existingCounts_(data, ourProvincesMap) {
  var ctx = {
    Мир: {},         // type -> count
    Государство: {}, // type -> count (наши провинции)
    Провинция: {}    // prov -> {type -> count}
  };

  normalizeToArray(data.Постройки).forEach(function (row) {
    normalizeToArray(row).forEach(function (b) {
      if (!b || typeof b !== "object") return;
      if (!b.Тип || !b.Провинция) return;

      var type = b.Тип;
      var prov = b.Провинция;

      ctx.Мир[type] = (ctx.Мир[type] || 0) + 1;

      if (!ctx.Провинция[prov]) ctx.Провинция[prov] = {};
      ctx.Провинция[prov][type] = (ctx.Провинция[prov][type] || 0) + 1;

      if (ourProvincesMap && ourProvincesMap[prov]) {
        ctx.Государство[type] = (ctx.Государство[type] || 0) + 1;
      }
    });
  });

  return ctx;
}

/* =======================
   Контекст очереди (для лимитов)
   ======================= */

function BUILDQUEUE_buildContext_(queue) {
  var ctx = {
    Мир: {},
    Государство: {},
    Провинция: {}
  };

  queue = Array.isArray(queue) ? queue : [];
  queue.forEach(function (q) {
    if (!q || typeof q !== "object") return;
    var type = q.Тип;
    var prov = q.Провинция;
    var qty = Number(q.Количество || 0) || 0;
    if (!type || !prov || qty <= 0) return;

    ctx.Мир[type] = (ctx.Мир[type] || 0) + qty;
    ctx.Государство[type] = (ctx.Государство[type] || 0) + qty;

    if (!ctx.Провинция[prov]) ctx.Провинция[prov] = {};
    ctx.Провинция[prov][type] = (ctx.Провинция[prov][type] || 0) + qty;
  });

  return ctx;
}

function BUILDQUEUE_applyToContext_(ctx, type, prov, qty) {
  qty = Number(qty || 0) || 0;
  if (!ctx || !type || !prov || qty <= 0) return;

  ctx.Мир[type] = (ctx.Мир[type] || 0) + qty;
  ctx.Государство[type] = (ctx.Государство[type] || 0) + qty;

  if (!ctx.Провинция[prov]) ctx.Провинция[prov] = {};
  ctx.Провинция[prov][type] = (ctx.Провинция[prov][type] || 0) + qty;
}

/* =======================
   STRICT CHECK: критерии + лимиты (иначе отклонение)
   ======================= */

function BUILD_checkPlacementStrict_(data, args) {
  var reasons = [];

  // 1) шаблон
  if (!args.tpl) {
    reasons.push({
      titleParts: makeTitleParts("Шаблон постройки ", args.type),
      exp: (function () {
        var p = [];
        uiPrefix(p, indent(1), false);
        uiText(p, "Ошибка: "); uiVal(p, "Неизвестный тип постройки"); uiNL(p);
        return { ok: false, parts: p };
      })()
    });
    return { ok: false, reasonsParts: reasons };
  }

  // 2) провинция существует
  if (!args.prov) {
    reasons.push({
      titleParts: makeTitleParts("Провинция", args.provinceName),
      exp: (function () {
        var p = [];
        uiPrefix(p, indent(1), false);
        uiText(p, "Ошибка: "); uiVal(p, "Провинция не найдена"); uiNL(p);
        return { ok: false, parts: p };
      })()
    });
    return { ok: false, reasonsParts: reasons };
  }

  // 3) провинция наша
  if (!args.prov._isOur) {
    reasons.push({
      titleParts: makePlainTitleParts("Владелец провинции"),
      exp: (function () {
        var p = [];
        uiPrefix(p, indent(1), false);
        uiText(p, "Нельзя строить в чужой провинции"); uiNL(p);
        return { ok: false, parts: p };
      })()
    });
    return { ok: false, reasonsParts: reasons };
  }

  var tpl = args.tpl;
  var prov = args.prov;
  var provKey = prov.Провинция || prov.Название || prov.id || args.provinceName;

  // 4) ресурсы провинции
  if (tpl.ТребуемыеРесурсы) {
    var provResources = prov.Ресурсы || {};
    for (var res in tpl.ТребуемыеРесурсы) {
      var ruleRes = tpl.ТребуемыеРесурсы[res];
      var valRes = provResources[res];

      if (!evaluateRule(ruleRes, valRes)) {
        reasons.push({
          titleParts: makeTitleParts("Необходим ресурс в провинции: ", res),
          exp: explainRuleParts(ruleRes, valRes, 1)
        });
      }
    }
  }

  // 5) критерии провинции
  var pr = checkProvinceCriteriaParts(prov, tpl.КритерииПровинции);
  if (pr.length) reasons = reasons.concat(pr);

  // 6) критерии государства + свойства
  if (tpl.КритерииГосударства) {
    var sr = checkStateCriteriaParts(args.STATE_CONTEXT, tpl.КритерииГосударства);
    if (sr.length) reasons = reasons.concat(sr);
  }

  if (tpl.КритерииСтабильностиГосударства) {
    reasons = reasons.concat(checkStatePropertyCriteriaParts(args.STATE_CONTEXT, tpl.КритерииСтабильностиГосударства, "Стабильность государства"));
  }
  if (tpl.КритерииКультурГосударства) {
    reasons = reasons.concat(checkStatePropertyCriteriaParts(args.STATE_CONTEXT, tpl.КритерииКультурГосударства, "Признанные культуры"));
  }
  if (tpl.КритерииРелигийГосударства) {
    reasons = reasons.concat(checkStatePropertyCriteriaParts(args.STATE_CONTEXT, tpl.КритерииРелигийГосударства, "Признанные религии"));
  }
  if (tpl.КритерииРасГосударства) {
    reasons = reasons.concat(checkStatePropertyCriteriaParts(args.STATE_CONTEXT, tpl.КритерииРасГосударства, "Признанные расы"));
  }
  if (tpl.КритерииФракцийГосударства) {
    reasons = reasons.concat(checkFactionCriteriaParts(args.STATE_CONTEXT, tpl.КритерииФракцийГосударства));
  }

  // 7) критерии построек по активным
  if (tpl.КритерииПостроек) {
    if (tpl.КритерииПостроек.Провинция) {
      var ctxP = args.ACTIVE_BUILDINGS_CONTEXT.Провинция[provKey] || {};
      var rP = checkBuildingCriteriaParts(tpl.КритерииПостроек.Провинция, ctxP, 1);
      if (!rP.ok) reasons.push({ titleParts: makePlainTitleParts("Критерии построек в провинции"), exp: { ok: false, parts: rP.parts } });
    }
    if (tpl.КритерииПостроек.Государство) {
      var rS = checkBuildingCriteriaParts(tpl.КритерииПостроек.Государство, args.ACTIVE_BUILDINGS_CONTEXT.Государство, 1);
      if (!rS.ok) reasons.push({ titleParts: makePlainTitleParts("Критерии построек в государстве"), exp: { ok: false, parts: rS.parts } });
    }
    if (tpl.КритерииПостроек.Мир) {
      var rW = checkBuildingCriteriaParts(tpl.КритерииПостроек.Мир, args.ACTIVE_BUILDINGS_CONTEXT.Мир, 1);
      if (!rW.ok) reasons.push({ titleParts: makePlainTitleParts("Критерии построек в мире"), exp: { ok: false, parts: rW.parts } });
    }
  }

  // 8) лимиты: существующие + очередь + qty
  BUILD_checkLimitsWithQueue_(tpl, {
    type: args.type,
    provKey: provKey,
    qty: args.qty,
    EXISTING_CTX: args.EXISTING_CTX,
    QUEUE_CTX: args.QUEUE_CTX
  }, reasons);

  return { ok: reasons.length === 0, reasonsParts: reasons };
}

function BUILD_checkLimitsWithQueue_(tpl, ctx, reasons) {
  if (!tpl || !tpl.Лимит) return;

  var type = ctx.type;
  var prov = ctx.provKey;
  var qty = Number(ctx.qty || 0) || 0;

  var exW = (ctx.EXISTING_CTX.Мир[type] || 0);
  var exS = (ctx.EXISTING_CTX.Государство[type] || 0);
  var exP = ((ctx.EXISTING_CTX.Провинция[prov] || {})[type] || 0);

  var qW = (ctx.QUEUE_CTX.Мир[type] || 0);
  var qS = (ctx.QUEUE_CTX.Государство[type] || 0);
  var qP = ((ctx.QUEUE_CTX.Провинция[prov] || {})[type] || 0);

  // Провинция
  if (tpl.Лимит.Провинция) {
    var limP = Number(tpl.Лимит.Провинция) || 0;
    if (limP > 0) {
      var totalP = exP + qP + qty;
      if (totalP > limP) {
        reasons.push({
          titleParts: makePlainTitleParts("Лимит (Провинция)"),
          exp: BUILD_makeLimitExp_("Превышен лимит на провинцию", limP, exP, qP, qty, totalP)
        });
      }
    }
  }

  // Государство
  if (tpl.Лимит.Государство) {
    var limS = Number(tpl.Лимит.Государство) || 0;
    if (limS > 0) {
      var totalS = exS + qS + qty;
      if (totalS > limS) {
        reasons.push({
          titleParts: makePlainTitleParts("Лимит (Государство)"),
          exp: BUILD_makeLimitExp_("Превышен лимит на государство", limS, exS, qS, qty, totalS)
        });
      }
    }
  }

  // Мир
  if (tpl.Лимит.Мир) {
    var limW = Number(tpl.Лимит.Мир) || 0;
    if (limW > 0) {
      var totalW = exW + qW + qty;
      if (totalW > limW) {
        reasons.push({
          titleParts: makePlainTitleParts("Лимит (Мир)"),
          exp: BUILD_makeLimitExp_("Превышен лимит на мир", limW, exW, qW, qty, totalW)
        });
      }
    }
  }
}

function BUILD_makeLimitExp_(caption, limit, existing, queued, inOrder, total) {
  var p = [];

  uiPrefix(p, indent(1), false);
  uiText(p, caption + ": ");
  uiVal(p, limit);
  uiNL(p);

  uiPrefix(p, indent(1), false);
  uiText(p, "Сейчас: ");
  uiVal(p, existing);
  uiText(p, " (существ.) + ");
  uiVal(p, queued);
  uiText(p, " (в очереди) + ");
  uiVal(p, inOrder);
  uiText(p, " (в приказе) = ");
  uiVal(p, total);
  uiNL(p);

  return { ok: false, parts: p };
}

/* =======================
   NEWS: приказ принят/отклонён
   ======================= */

function ORDERS_pushBuildOrderNotice_(data, info) {
  var parts = [];
  var ok = !!info.ok;

  uiTitle(parts, "Приказ на строительство", UI.BORDER);
  uiTop(parts, UI.BORDER);

  uiRow(parts, "Здание", info.type, UI.VALUE, UI.BORDER);
  uiRow(parts, "Провинция", info.province, UI.VALUE, UI.BORDER);
  uiRow(parts, "Количество", info.qty, UI.VALUE, UI.BORDER);

  if (ok) {
    uiRow(parts, "Результат", "Принят", UI.OK, UI.BORDER);
    uiRow(parts, "Проект", info.id, UI.VALUE, UI.BORDER);
  } else {
    uiRow(parts, "Результат", "Отклонён", UI.BAD, UI.BORDER);

    var reasons = info.reasonsParts || [];
    uiRow(parts, "Причины", String(reasons.length), UI.VALUE, UI.BORDER);

    reasons.forEach(function (block) {
      uiBlank(parts, UI.BORDER);

      if (block.titleParts && block.titleParts.length) parts = parts.concat(block.titleParts);
      else parts = parts.concat(makePlainTitleParts(block.title || "Причина"));

      if (block.exp && block.exp.parts && block.exp.parts.length) {
        parts = parts.concat(block.exp.parts);
      }
    });
  }

  uiBottom(parts, UI.BORDER);

  pushBoxNotice(data, {
    category: "Постройки",
    sub: "Приказы на строительство",
    priority: ok ? 200 : 900,
    parts: parts
  });
}

/* =======================
   Fallback error (если pushErrorNotice нет)
   ======================= */

function ORDERS_pushSimpleError_(data, code, message) {
  var parts = [];
  uiTitle(parts, "Ошибка", UI.BAD);
  uiTop(parts, UI.BAD);
  uiRow(parts, "Код", code, UI.VALUE, UI.BAD);
  uiRow(parts, "Причина", message, UI.VALUE, UI.BAD);
  uiBottom(parts, UI.BAD);

  pushBoxNotice(data, {
    category: "Система",
    sub: "Приказы на строительство",
    priority: 999,
    parts: parts
  });
}