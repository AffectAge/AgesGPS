/* =========================================================
   ORDERS: ПРИКАЗЫ НА СНОС ЗДАНИЙ (ВАРИАНТ 1 + ВАРИАНТ A)
   Google Apps Script (V8)

   ✅ Приказы = текстовые строки в data.Приказы (1D или 2D)
   ✅ Шаблон (СТРОГО):
      Снести [Кирпичный завод] в провинции [Неизвестная провинция 1] компании [Acme] корпорации [Lantium Group] в количестве [3]

   ✅ ВАРИАНТ A: СНОС ТОЛЬКО В СВОИХ ПРОВИНЦИЯХ
      Провинция считается "нашей", если:
        findProvince(getAllProvinces(data), prov).Владелец === stateId

   ✅ Если найдено меньше -> ЧАСТИЧНОЕ ВЫПОЛНЕНИЕ (снести сколько есть)
      - если найдено 0 (в наших провинциях) -> отклонить
      - если провинция чужая -> отклонить (даже если совпадения есть)

   ✅ Снос: ЖЁСТКОЕ УДАЛЕНИЕ из data.Постройки
      data.Постройки: 1D массив ячеек, каждая ячейка = [] зданий (cap=10)
      (поддержка редкого случая: "2D внутри ячейки" тоже есть)

   ✅ Новости: pushBoxNotice(data,{category,sub,priority,parts}) + твои UI-хелперы
      (если их нет — модуль не упадёт, но новости будут пропущены)

   ========================================================= */

/* =======================
   CONFIG
   ======================= */

var DEMO_CFG = {
  CATEGORY: "Постройки",
  SUB_ORDERS: "Приказы (снос)",

  ORDER_PREFIX_OK: "✅ Приказ принят",
  ORDER_PREFIX_PARTIAL: "🟨 Приказ принят частично",
  ORDER_PREFIX_BAD: "⛔ Приказ отклонён",

  ID_PREFIX: "DEM"
};

/* =========================================================
   PUBLIC API
   ========================================================= */

/**
 * Главная функция обработки приказов на снос.
 * Вызывай один раз за ход, например перед строительством:
 *   ORDERS_processDemolishOrders(data);
 */
function ORDERS_processDemolishOrders(data) {
  if (!data || typeof data !== "object") return data;
  ensureNews(data);

  // Идентификатор государства (для Варианта A: только свои провинции)
  var stateId = getStateIdFromStateData(data);
  if (!stateId) {
    DEMO_pushSystemError_(data, "STATE_ID_NOT_FOUND",
      "Идентификатор государства не найден в data['Данные государства'].");
    return data;
  }

  var ordersField = data.Приказы;
  if (ordersField == null) return data;

  // системная ячейка в "Данные государства" под счётчик
  var sysCell = DEMO_getOrCreateOrdersCell_(data);
  if (!sysCell) {
    DEMO_pushSystemError_(data, "DEMO_NO_SPACE",
      "Не удалось найти/создать системную ячейку в data['Данные государства'] (добавление строк запрещено).");
    return data;
  }
  if (typeof sysCell.СчётчикСноса !== "number") sysCell.СчётчикСноса = 0;

  // 2D или 1D
  var is2D = Array.isArray(ordersField) && Array.isArray(ordersField[0]);

  if (is2D) {
    for (var r = 0; r < ordersField.length; r++) {
      var row = ordersField[r];
      if (!Array.isArray(row)) continue;
      for (var c = 0; c < row.length; c++) {
        var res = DEMO_tryHandleOneOrder_(data, row[c], sysCell, stateId);
        if (res && res.handled) row[c] = res.newText;
      }
    }
  } else {
    var arr = normalizeToArray(ordersField);
    data.Приказы = arr;
    for (var i = 0; i < arr.length; i++) {
      var res2 = DEMO_tryHandleOneOrder_(data, arr[i], sysCell, stateId);
      if (res2 && res2.handled) arr[i] = res2.newText;
    }
  }

  return data;
}

/* =========================================================
   CORE: handle one order
   ========================================================= */

function DEMO_tryHandleOneOrder_(data, orderText, sysCell, stateId) {
  if (orderText == null) return null;
  var s = String(orderText).trim();
  if (!s) return null;

  // не перерабатывать уже обработанные
  if (s.indexOf(DEMO_CFG.ORDER_PREFIX_OK) === 0) return null;
  if (s.indexOf(DEMO_CFG.ORDER_PREFIX_PARTIAL) === 0) return null;
  if (s.indexOf(DEMO_CFG.ORDER_PREFIX_BAD) === 0) return null;

  // парсим только наш строгий формат
  var parsed = DEMO_parseDemolishOrder_(s);
  if (!parsed.ok) return null;

  var qty = Math.floor(Number(parsed.qty) || 0);
  if (qty <= 0) {
    DEMO_pushOrderNotice_(data, {
      ok: false,
      title: "Приказ на снос отклонён",
      parsed: parsed,
      reasonsParts: DEMO_reasonPartsOneLine_("Некорректное количество: " + String(parsed.qty))
    });
    return {
      handled: true,
      newText: DEMO_CFG.ORDER_PREFIX_BAD + " (" + DEMO_nextId_(sysCell) + ")"
    };
  }

  var result = DEMO_demolishBuildings_(data, {
    stateId: stateId,
    type: parsed.type,
    province: parsed.province,
    company: parsed.company,
    corp: parsed.corp,
    qty: qty
  });

  // Провинция чужая -> запрет
  if (result && result.foreignProvince) {
    DEMO_pushOrderNotice_(data, {
      ok: false,
      title: "Снос запрещён",
      parsed: parsed,
      reasonsParts: DEMO_reasonPartsOneLine_(
        "Провинция не принадлежит вашему государству (Вариант A). " +
        "Совпадений в чужой провинции: " + String(result.foundForeign || 0) + "."
      )
    });
    return {
      handled: true,
      newText: DEMO_CFG.ORDER_PREFIX_BAD + " (" + DEMO_nextId_(sysCell) + ")"
    };
  }

  // 0 совпадений в нашей провинции
  if (!result || result.demolished <= 0) {
    DEMO_pushOrderNotice_(data, {
      ok: false,
      title: "Снос невозможен",
      parsed: parsed,
      reasonsParts: DEMO_reasonPartsNotEnough_(0, qty)
    });
    return {
      handled: true,
      newText: DEMO_CFG.ORDER_PREFIX_BAD + " (" + DEMO_nextId_(sysCell) + ")"
    };
  }

  // Partial
  if (result.demolished < qty) {
    DEMO_pushOrderNotice_(data, {
      ok: true,
      partial: true,
      title: "Снос выполнен частично",
      parsed: parsed,
      demolished: result.demolished,
      requested: qty
    });
    return {
      handled: true,
      newText: DEMO_CFG.ORDER_PREFIX_PARTIAL + " (" + DEMO_nextId_(sysCell) + ")"
    };
  }

  // Full
  DEMO_pushOrderNotice_(data, {
    ok: true,
    partial: false,
    title: "Снос выполнен",
    parsed: parsed,
    demolished: result.demolished,
    requested: qty
  });
  return {
    handled: true,
    newText: DEMO_CFG.ORDER_PREFIX_OK + " (" + DEMO_nextId_(sysCell) + ")"
  };
}

/* =========================================================
   PARSER: strict template
   ========================================================= */

function DEMO_parseDemolishOrder_(s) {
  // Снести [Тип] в провинции [Провинция] компании [Компания] корпорации [Корпорация] в количестве [N]
  if (s.indexOf("Снести ") !== 0) return { ok: false };

  var m = s.match(/^Снести\s*\[([^\]]+)\]\s*в\s*провинции\s*\[([^\]]+)\]\s*компании\s*\[([^\]]*)\]\s*корпорации\s*\[([^\]]*)\]\s*в\s*количестве\s*\[([0-9]+)\]\s*$/);
  if (!m) return { ok: false };

  var type = String(m[1] || "").trim();
  var province = String(m[2] || "").trim();
  var company = String(m[3] || "").trim();
  var corp = String(m[4] || "").trim();
  var qty = String(m[5] || "").trim();

  if (!type || !province) return { ok: false };

  return {
    ok: true,
    raw: s,
    type: type,
    province: province,
    company: company,
    corp: corp,
    qty: qty
  };
}

/* =========================================================
   DEMOLISH ENGINE (Variant A: only our provinces)
   ========================================================= */

function DEMO_demolishBuildings_(data, req) {
  if (!data || typeof data !== "object") return { demolished: 0, foundAll: 0, foundOur: 0, foundForeign: 0 };
  if (!Array.isArray(data.Постройки)) return { demolished: 0, foundAll: 0, foundOur: 0, foundForeign: 0 };

  var col = data.Постройки;

  var stateId = (req && req.stateId != null) ? String(req.stateId) : "";
  var type = String(req.type || "").trim();
  var prov = String(req.province || "").trim();
  var company = String(req.company || "").trim();
  var corp = String(req.corp || "").trim();
  var need = Math.max(0, Math.floor(Number(req.qty) || 0));

  if (!stateId || !type || !prov || need <= 0) {
    return { demolished: 0, foundAll: 0, foundOur: 0, foundForeign: 0 };
  }

  // Провинция должна быть нашей
  var provinces = getAllProvinces(data);
  var provObj = findProvince(provinces, prov);
  var isOurProvince = !!(provObj && String(provObj.Владелец || "") === stateId);

  var refs = [];
  var foundAll = 0, foundForeign = 0, foundOur = 0;

  for (var i = 0; i < col.length; i++) {
    var cell = col[i];
    if (!cell || cell === "") continue;

    // 2D внутри ячейки
    if (Array.isArray(cell) && cell.length && Array.isArray(cell[0])) {
      for (var r = 0; r < cell.length; r++) {
        var sub = cell[r];
        if (!Array.isArray(sub)) continue;
        for (var j = 0; j < sub.length; j++) {
          var b = sub[j];
          if (!DEMO_matchBuilding_(b, type, prov, company, corp)) continue;

          foundAll++;
          if (isOurProvince) {
            foundOur++;
            refs.push({ cell: sub, cellIndex: i, bIndex: j });
          } else {
            foundForeign++;
          }
        }
      }
      continue;
    }

    // обычная 1D ячейка
    if (Array.isArray(cell)) {
      for (var k = 0; k < cell.length; k++) {
        var b2 = cell[k];
        if (!DEMO_matchBuilding_(b2, type, prov, company, corp)) continue;

        foundAll++;
        if (isOurProvince) {
          foundOur++;
          refs.push({ cell: cell, cellIndex: i, bIndex: k });
        } else {
          foundForeign++;
        }
      }
    }
  }

  // чужая провинция — запрещено
  if (!isOurProvince) {
    return { demolished: 0, foundAll: foundAll, foundOur: 0, foundForeign: foundForeign, foreignProvince: true };
  }

  if (!refs.length) {
    return { demolished: 0, foundAll: foundAll, foundOur: foundOur, foundForeign: 0 };
  }

  // удаляем с конца (стабильная сортировка)
  refs.sort(function (a, b) {
    if (b.cellIndex !== a.cellIndex) return b.cellIndex - a.cellIndex;
    return b.bIndex - a.bIndex;
  });

  var demolished = 0;
  for (var z = 0; z < refs.length && demolished < need; z++) {
    var ref = refs[z];
    var arr = ref.cell;
    if (!Array.isArray(arr)) continue;
    if (ref.bIndex < 0 || ref.bIndex >= arr.length) continue;

    arr.splice(ref.bIndex, 1);
    demolished++;
  }

  // мягкая чистка пустых ячеек
  for (var q = 0; q < col.length; q++) {
    if (Array.isArray(col[q]) && col[q].length === 0) col[q] = [];
  }

  return { demolished: demolished, foundAll: foundAll, foundOur: foundOur, foundForeign: 0 };
}

function DEMO_matchBuilding_(b, type, prov, company, corp) {
  if (!b || typeof b !== "object") return false;
  if (String(b.Тип || "").trim() !== type) return false;
  if (String(b.Провинция || "").trim() !== prov) return false;
  if (String(b.Компания || "").trim() !== company) return false;
  if (String(b.Корпорация || "").trim() !== corp) return false;
  return true;
}

/* =========================================================
   STORAGE: counter cell in "Данные государства"
   ========================================================= */

function DEMO_getOrCreateOrdersCell_(data) {
  var rows = normalizeToArray(data["Данные государства"]);
  if (!Array.isArray(data["Данные государства"])) data["Данные государства"] = rows;

  // 1) ищем подходящую объект-ячейку
  for (var i = 0; i < rows.length; i++) {
    var row = normalizeToArray(rows[i]); rows[i] = row;
    for (var j = 0; j < row.length; j++) {
      var cell = row[j];
      if (!cell || typeof cell !== "object" || Array.isArray(cell)) continue;

      // если это ячейка очереди строительства/казны/системная — используем
      if (Array.isArray(cell.ОчередьСтроительства)) return cell;
      if (Object.prototype.hasOwnProperty.call(cell, "Деньги") ||
          Object.prototype.hasOwnProperty.call(cell, "Склад") ||
          Object.prototype.hasOwnProperty.call(cell, "ОчковСтроительства") ||
          Object.prototype.hasOwnProperty.call(cell, "СчётчикСноса")) return cell;
    }
  }

  // 2) иначе создаём объект в первой свободной ячейке (без добавления строк)
  for (var r = 0; r < rows.length; r++) {
    var row2 = normalizeToArray(rows[r]); rows[r] = row2;
    for (var c = 0; c < row2.length; c++) {
      if (row2[c] === "" || row2[c] == null) {
        var created = { СчётчикСноса: 0 };
        row2[c] = created;
        return created;
      }
    }
  }

  return null;
}

function DEMO_nextId_(sysCell) {
  sysCell.СчётчикСноса = Math.floor(Number(sysCell.СчётчикСноса) || 0) + 1;
  var n = sysCell.СчётчикСноса;
  var s = String(n);
  while (s.length < 6) s = "0" + s;
  return DEMO_CFG.ID_PREFIX + "-" + s;
}

/* =========================================================
   NEWS (uses your UI helpers if present)
   ========================================================= */

function DEMO_pushOrderNotice_(data, info) {
  if (typeof pushBoxNotice !== "function") return; // в проекте нет новостей — молча пропускаем
  if (typeof uiTitle !== "function") return;

  var parts = [];
  var ok = !!info.ok;
  var partial = !!info.partial;
  var parsed = info.parsed || {};

  var border = ok ? UI.BORDER : UI.BAD;
  var title = info.title || "Снос";

  uiTitle(parts, title, ok ? UI.BORDER : UI.BAD);
  uiTop(parts, ok ? UI.BORDER : UI.BAD);

  uiRow(parts, "Здание", parsed.type || "—", UI.VALUE, border);
  uiRow(parts, "Провинция", parsed.province || "—", UI.VALUE, border);
  uiRow(parts, "Компания", parsed.company || "—", UI.VALUE, border);
  uiRow(parts, "Корпорация", parsed.corp || "—", UI.VALUE, border);
  uiRow(parts, "Запрошено", String(parsed.qty || "0"), UI.VALUE, border);

  if (ok) {
    uiRow(parts, "Снесено", String(info.demolished || 0), UI.VALUE, border);
    uiRow(parts, "Результат", partial ? "Частичное выполнение" : "Выполнено полностью", UI.VALUE, border);
  } else {
    uiRow(parts, "Результат", "Отклонено", UI.BAD, UI.BAD);

    var reasons = info.reasonsParts || [];
    uiRow(parts, "Причины", String(reasons.length ? reasons.length : 1), UI.VALUE, UI.BAD);

    if (reasons && reasons.length) {
      reasons.forEach(function (block) {
        uiBlank(parts, UI.BAD);
        if (block.titleParts && block.titleParts.length) parts = parts.concat(block.titleParts);
        else parts = parts.concat(makePlainTitleParts(block.title || "Причина"));
        if (block.exp && block.exp.parts && block.exp.parts.length) parts = parts.concat(block.exp.parts);
      });
    }
  }

  uiBottom(parts, ok ? UI.BORDER : UI.BAD);

  pushBoxNotice(data, {
    category: DEMO_CFG.CATEGORY,
    sub: DEMO_CFG.SUB_ORDERS,
    priority: ok ? (partial ? 260 : 230) : 930,
    parts: parts
  });
}

function DEMO_pushSystemError_(data, code, message) {
  if (typeof pushBoxNotice !== "function") return;
  if (typeof uiTitle !== "function") return;

  var parts = [];
  uiTitle(parts, "Ошибка", UI.BAD);
  uiTop(parts, UI.BAD);
  uiRow(parts, "Код", code, UI.VALUE, UI.BAD);
  uiRow(parts, "Причина", message, UI.VALUE, UI.BAD);
  uiBottom(parts, UI.BAD);

  pushBoxNotice(data, {
    category: "Система",
    sub: "Снос",
    priority: 999,
    parts: parts
  });
}

function DEMO_reasonPartsOneLine_(text) {
  return [{
    titleParts: (typeof makePlainTitleParts === "function") ? makePlainTitleParts("Причина") : [],
    exp: (function () {
      var p = [];
      if (typeof uiPrefix === "function") uiPrefix(p, indent(1), false);
      if (typeof uiText === "function") { uiText(p, String(text || "—")); uiNL(p); }
      return { ok: false, parts: p };
    })()
  }];
}

function DEMO_reasonPartsNotEnough_(have, need) {
  return [{
    titleParts: (typeof makePlainTitleParts === "function") ? makePlainTitleParts("Недостаточно зданий") : [],
    exp: (function () {
      var p = [];
      if (typeof uiPrefix === "function") uiPrefix(p, indent(1), false);
      if (typeof uiText === "function") {
        uiText(p, "Найдено: "); uiVal(p, String(have));
        uiText(p, ", требуется: "); uiVal(p, String(need)); uiNL(p);
      }
      return { ok: false, parts: p };
    })()
  }];
}

/* =========================================================
   FALLBACK HELPERS (safe; will not override existing ones)
   ========================================================= */

if (typeof normalizeToArray !== "function") {
  function normalizeToArray(value) {
    if (Array.isArray(value)) return value;
    if (value === null || value === undefined) return [];
    return [value];
  }
}

if (typeof ensureNews !== "function") {
  function ensureNews(data) {
    if (!data) return;
    if (!Array.isArray(data.Новости)) data.Новости = [];
  }
}

/**
 * Fallback: getAllProvinces(data)
 * Ищет data.Провинции или data["Провинции"] (1D/2D).
 */
if (typeof getAllProvinces !== "function") {
  function getAllProvinces(data) {
    var src = data && (data.Провинции || data["Провинции"]);
    var out = [];
    normalizeToArray(src).forEach(function (row) {
      normalizeToArray(row).forEach(function (p) {
        if (p && typeof p === "object" && !Array.isArray(p)) out.push(p);
      });
    });
    return out;
  }
}

/**
 * Fallback: findProvince(list, name)
 * Сопоставляет по p.Провинция или p.Название или p.id.
 */
if (typeof findProvince !== "function") {
  function findProvince(provinces, provinceName) {
    var key = String(provinceName || "").trim();
    if (!key) return null;
    for (var i = 0; i < provinces.length; i++) {
      var p = provinces[i];
      if (!p || typeof p !== "object") continue;
      var k = String(p.Провинция || p.Название || p.id || "").trim();
      if (k === key) return p;
    }
    return null;
  }
}

/**
 * Fallback: getStateIdFromStateData(data)
 * Пытается вытащить Идентификатор государства из data["Данные государства"].
 * Подстрой под свой реальный ключ, если он у тебя другой.
 */
if (typeof getStateIdFromStateData !== "function") {
  function getStateIdFromStateData(data) {
    var rows = normalizeToArray(data && data["Данные государства"]);
    for (var i = 0; i < rows.length; i++) {
      var row = normalizeToArray(rows[i]);
      for (var j = 0; j < row.length; j++) {
        var cell = row[j];
        if (!cell || typeof cell !== "object" || Array.isArray(cell)) continue;
        if (cell["Идентификатор государства"] != null) return String(cell["Идентификатор государства"]);
        if (cell["ГосударствоId"] != null) return String(cell["ГосударствоId"]);
        if (cell["StateId"] != null) return String(cell["StateId"]);
      }
    }
    return "";
  }
}

/* =========================================================
   EXAMPLE ORDER
   =========================================================
   Снести [Дороги] в провинции [Неизвестная провинция 1] компании [Acme] корпорации [Lantium Group] в количестве [3]
*/