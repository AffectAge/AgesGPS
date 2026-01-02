/* =========================================================
   УНИВЕРСАЛЬНЫЙ ДВИЖОК КРИТЕРИЕВ + ЛИМИТЫ С ПРИОРИТЕТОМ
   Google Apps Script (V8)

   ✅ Формат новостей приведён к стилю "ячейки" как в скрипте рынка труда:
      - Заголовок + рамка ┌ └
      - Строки "┃ ➔ ..."
      - Ошибки: красная рамка
   ⚠️ Логика критериев/лимитов НЕ менялась — только формат сообщений.

   Требование: функция pushNotice(data, {category, sub, priority, parts}) должна быть в проекте.
   ========================================================= */

/* =======================
   UI: стиль "ячейки" (как рынок труда)
   ======================= */

var UI = {
  BORDER: "#FF8C00",
  LABEL:  "#CFC7BA",
  VALUE:  "#E6E6FA", // значения по умолчанию
  BAD:    "#E36A6A",
  TEXT:   "#B9B1A4",
  DIM:    "#6E675F",

  TOP: "┌────────────────────────────────────────────────────────┐\n",
  BOT: "└────────────────────────────────────────────────────────┘\n",

  // Лимиты вывода длинных списков
  LIST_MAX_ITEMS: 10,      // 0 = без лимита
  LIST_MAX_CHARS: 64       // 0 = без обрезки
};

function ensureNews(data) {
  if (!data || typeof data !== "object") return;
  if (!Array.isArray(data.Новости)) data.Новости = [];
}

function clipText(s, maxChars) {
  s = String(s == null ? "" : s);
  maxChars = (maxChars === undefined || maxChars === null) ? UI.LIST_MAX_CHARS : maxChars;
  if (maxChars > 0 && s.length > maxChars) return s.slice(0, maxChars - 1) + "…";
  return s;
}

/* =======================
   UI helpers (ячейка)
   ======================= */

function uiTitle(parts, text, borderColor) {
  parts.push({ text: String(text) + "\n", bold: true, color: borderColor || UI.BORDER });
}

function uiTop(parts, borderColor) {
  parts.push({ text: UI.TOP, color: borderColor || UI.BORDER });
}

function uiBottom(parts, borderColor) {
  parts.push({ text: UI.BOT, color: borderColor || UI.BORDER });
}

function uiRow(parts, label, value, valueColor, borderColor) {
  var bc = borderColor || UI.BORDER;
  parts.push({ text: "┃", bold: true, color: bc });
  parts.push({ text: " ➔ " + label + ": ", bold: true, color: UI.LABEL });
  parts.push({ text: String(value) + "\n", bold: true, color: valueColor || UI.VALUE });
}

function uiLine(parts, text, color, bold, borderColor) {
  var bc = borderColor || UI.BORDER;
  parts.push({ text: "┃", bold: true, color: bc });
  parts.push({ text: " " + String(text) + "\n", color: color || UI.TEXT, bold: !!bold });
}

function uiBlank(parts, borderColor) {
  var bc = borderColor || UI.BORDER;
  parts.push({ text: "┃\n", bold: true, color: bc });
}

function pushBoxNotice(data, opts) {
  ensureNews(data);
  pushNotice(data, {
    category: opts.category || "Система",
    sub: opts.sub || "",
    priority: opts.priority || 100,
    parts: opts.parts || []
  });
}

/* =======================
   Ошибка (рамка как в рынке труда)
   ======================= */

function pushErrorNotice(data, code, message) {
  var parts = [];
  uiTitle(parts, "Ошибка", UI.BAD);
  uiTop(parts, UI.BAD);

  uiRow(parts, "Код", code, UI.VALUE, UI.BAD);
  uiRow(parts, "Причина", message, UI.VALUE, UI.BAD);

  uiBottom(parts, UI.BAD);

  pushBoxNotice(data, {
    category: "Система",
    sub: "Проверка критериев",
    priority: 999,
    parts: parts
  });
}

/* =======================
   Базовые утилиты
   ======================= */

function indent(level) { return '  '.repeat(level); }

function normalizeToArray(value) {
  if (Array.isArray(value)) return value;
  if (value === null || value === undefined) return [];
  return [value];
}

function hasValue(value, v) {
  return normalizeToArray(value).indexOf(v) !== -1;
}

function countTrue(arr) {
  return arr.filter(Boolean).length;
}

function getValueByPath(obj, path) {
  return path.split('.').reduce(function (o, k) {
    return o ? o[k] : undefined;
  }, obj);
}

/* =======================
   Контексты
   ======================= */

function buildStateContext(data) {
  var ctx = {};
  var rows = normalizeToArray(data['Данные государства']);

  rows.forEach(function (row) {
    normalizeToArray(row).forEach(function (cell) {
      if (!cell || typeof cell !== 'object') return;

      for (var key in cell) {
        if (cell[key] === null || cell[key] === undefined) continue;
        if (!ctx[key]) ctx[key] = [];

        if (Array.isArray(cell[key])) ctx[key] = ctx[key].concat(cell[key]);
        else ctx[key].push(cell[key]);
      }
    });
  });

  return ctx;
}

function buildActiveBuildingsContext(data, provincesMap) {
  var ctx = { Провинция: {}, Государство: {}, Мир: {} };

  normalizeToArray(data.Постройки).forEach(function (row) {
    normalizeToArray(row).forEach(function (b) {
      if (!b || !b.Тип || !b.Провинция) return;
      if (b.Активно !== true) return;

      var provKey = b.Провинция;
      var isOur = !!provincesMap[provKey];

      ctx.Мир[b.Тип] = (ctx.Мир[b.Тип] || 0) + 1;

      if (!ctx.Провинция[provKey]) ctx.Провинция[provKey] = {};
      ctx.Провинция[provKey][b.Тип] = (ctx.Провинция[provKey][b.Тип] || 0) + 1;

      if (isOur) ctx.Государство[b.Тип] = (ctx.Государство[b.Тип] || 0) + 1;
    });
  });

  return ctx;
}

/* =======================
   Провинции
   ======================= */

function getAllProvinces(data) {
  if (!data.Провинции) return [];
  return normalizeToArray(data.Провинции)
    .reduce(function (a, b) { return a.concat(b); }, [])
    .filter(function (p) { return p && typeof p === 'object'; });
}

function findProvince(all, key) {
  return all.find(function (p) {
    return p.Провинция === key || p.Название === key || p.id === key;
  });
}

/* =======================
   Оценка правил
   ======================= */

function evaluateNumericRule(rule, value) {
  if (typeof value !== 'number') return false;
  if (rule['>'] !== undefined) return value > rule['>'];
  if (rule['<'] !== undefined) return value < rule['<'];
  if (rule['>='] !== undefined) return value >= rule['>='];
  if (rule['<='] !== undefined) return value <= rule['<='];
  if (rule['=='] !== undefined) return value === rule['=='];
  if (rule['!='] !== undefined) return value !== rule['!='];
  if (rule.BETWEEN) return value >= rule.BETWEEN[0] && value <= rule.BETWEEN[1];
  return false;
}

function evaluateRule(rule, value) {
  if (typeof rule === 'string') return normalizeToArray(value).indexOf(rule) !== -1;
  if (Array.isArray(rule)) return rule.some(function (r) { return evaluateRule(r, value); });

  if (typeof rule === 'object' && rule !== null) {
    if (Object.keys(rule).some(function (k) {
      return ['>','<','>=','<=','==','!=','BETWEEN'].indexOf(k) !== -1;
    })) return evaluateNumericRule(rule, value);

    if (rule.AND) return rule.AND.every(function (r) { return evaluateRule(r, value); });
    if (rule.OR)  return rule.OR.some(function (r) { return evaluateRule(r, value); });
    if (rule.NOT) return !evaluateRule(rule.NOT, value);
    if (rule.NAND) return !rule.NAND.every(function (r) { return evaluateRule(r, value); });
    if (rule.NOR)  return !rule.NOR.some(function (r) { return evaluateRule(r, value); });

    if (rule.XOR) {
      var c = 0;
      rule.XOR.forEach(function (r) { if (evaluateRule(r, value)) c++; });
      return c === 1;
    }
  }
  return false;
}

/* =======================
   explainRuleParts (как у тебя, но в "ячейке")
   ======================= */

function uiPrefix(parts, pad, isOk) {
  parts.push({ text: "┃", bold: true, color: UI.BORDER });
  parts.push({ text: pad, color: UI.TEXT });
  // в стиле рынка труда: "хорошо" не подсвечиваем зелёным, оставляем VALUE
  parts.push({ text: "➔ ", bold: true, color: isOk ? UI.VALUE : UI.BAD });
}
function uiText(parts, t) { parts.push({ text: String(t), color: UI.TEXT }); }
function uiVal(parts, v)  { parts.push({ text: String(v), bold: true, color: UI.VALUE }); }
function uiNL(parts)      { parts.push({ text: "\n", color: UI.TEXT }); }

function uiFoundBlockParts(value, pad, isOk) {
  var parts = [];
  pad = pad || "";

  uiPrefix(parts, pad, !!isOk);
  uiText(parts, "найдено:");
  uiNL(parts);

  if (value === undefined || value === null) {
    parts.push({ text: "┃", bold: true, color: UI.BORDER });
    parts.push({ text: pad + "   ", color: UI.TEXT });
    parts.push({ text: "• ", bold: true, color: UI.DIM });
    uiVal(parts, "Отсутствует");
    uiNL(parts);
    return parts;
  }

  if (!Array.isArray(value)) value = [value];

  var arr = value
    .map(function (x) { return x === null || x === undefined ? "" : String(x).trim(); })
    .filter(function (x) { return x !== ""; });

  if (!arr.length) {
    parts.push({ text: "┃", bold: true, color: UI.BORDER });
    parts.push({ text: pad + "   ", color: UI.TEXT });
    parts.push({ text: "• ", bold: true, color: UI.DIM });
    uiVal(parts, "пусто");
    uiNL(parts);
    return parts;
  }

  var maxItems = UI.LIST_MAX_ITEMS || 0;
  var showAll = (maxItems <= 0) || (arr.length <= maxItems);
  var toShow = showAll ? arr : arr.slice(0, maxItems);
  var hidden = arr.length - toShow.length;

  toShow.forEach(function (item) {
    parts.push({ text: "┃", bold: true, color: UI.BORDER });
    parts.push({ text: pad + "   ", color: UI.TEXT });
    parts.push({ text: "• ", bold: true, color: UI.DIM });
    uiVal(parts, clipText(item, UI.LIST_MAX_CHARS));
    uiNL(parts);
  });

  if (!showAll && hidden > 0) {
    parts.push({ text: "┃", bold: true, color: UI.BORDER });
    parts.push({ text: pad + "   ", color: UI.TEXT });
    parts.push({ text: "• ", bold: true, color: UI.DIM });
    uiVal(parts, "… +" + hidden + " ещё");
    uiNL(parts);
  }

  return parts;
}

function explainRuleParts(rule, value, level) {
  level = level || 0;
  var pad = indent(level);
  var parts = [];

  // STRING
  if (typeof rule === "string") {
    var okS = hasValue(value, rule);

    uiPrefix(parts, pad, okS);
    uiText(parts, "требуется: ");
    uiVal(parts, rule);
    uiNL(parts);

    parts = parts.concat(uiFoundBlockParts(normalizeToArray(value), pad, okS));
    return { ok: okS, parts: parts };
  }

  // ARRAY (OR)
  if (Array.isArray(rule)) {
    var resArr = rule.map(function (r) { return explainRuleParts(r, value, level + 1); });
    var okA = resArr.some(function (r) { return r.ok; });
    resArr.forEach(function (r) { parts = parts.concat(r.parts); });
    return { ok: okA, parts: parts };
  }

  // BETWEEN
  if (rule && rule.BETWEEN) {
    var v = value;
    var okB = typeof v === "number" && v >= rule.BETWEEN[0] && v <= rule.BETWEEN[1];

    uiPrefix(parts, pad, okB);
    uiText(parts, "значение BETWEEN ");
    uiVal(parts, rule.BETWEEN[0] + " .. " + rule.BETWEEN[1]);
    uiNL(parts);

    parts = parts.concat(uiFoundBlockParts(v, pad, okB));
    return { ok: okB, parts: parts };
  }

  // OPS
  var ops = [">","<",">=","<=","==","!="];
  for (var i = 0; i < ops.length; i++) {
    var op = ops[i];
    if (rule && rule[op] !== undefined) {
      var okOp = evaluateNumericRule((function(){ var o={}; o[op]=rule[op]; return o; })(), value);

      uiPrefix(parts, pad, okOp);
      uiText(parts, "значение " + op + " ");
      uiVal(parts, rule[op]);
      uiNL(parts);

      parts = parts.concat(uiFoundBlockParts(value, pad, okOp));
      return { ok: okOp, parts: parts };
    }
  }

  // AND / OR / XOR / NOT
  if (rule && rule.AND) {
    parts.push({ text: "┃", bold: true, color: UI.BORDER });
    parts.push({ text: pad + "Логика: AND\n", bold: true, color: UI.LABEL });

    var rA = rule.AND.map(function (r) { return explainRuleParts(r, value, level + 1); });
    var okAnd = rA.every(function (r) { return r.ok; });
    rA.forEach(function (r) { parts = parts.concat(r.parts); });
    return { ok: okAnd, parts: parts };
  }

  if (rule && rule.OR) {
    parts.push({ text: "┃", bold: true, color: UI.BORDER });
    parts.push({ text: pad + "Логика: OR\n", bold: true, color: UI.LABEL });

    var rO = rule.OR.map(function (r) { return explainRuleParts(r, value, level + 1); });
    var okOr = rO.some(function (r) { return r.ok; });
    rO.forEach(function (r) { parts = parts.concat(r.parts); });
    return { ok: okOr, parts: parts };
  }

  if (rule && rule.XOR) {
    parts.push({ text: "┃", bold: true, color: UI.BORDER });
    parts.push({ text: pad + "Логика: XOR\n", bold: true, color: UI.LABEL });

    var rX = rule.XOR.map(function (r) { return explainRuleParts(r, value, level + 1); });
    var cnt = countTrue(rX.map(function (r) { return r.ok; }));
    var okX = (cnt === 1);

    rX.forEach(function (r) { parts = parts.concat(r.parts); });

    uiPrefix(parts, pad, okX);
    uiText(parts, "выполнено ");
    uiVal(parts, cnt);
    uiText(parts, " из ");
    uiVal(parts, rX.length);
    uiNL(parts);

    return { ok: okX, parts: parts };
  }

  if (rule && rule.NOT) {
    parts.push({ text: "┃", bold: true, color: UI.BORDER });
    parts.push({ text: pad + "Логика: NOT\n", bold: true, color: UI.LABEL });

    var rN = explainRuleParts(rule.NOT, value, level + 1);
    parts = parts.concat(rN.parts);
    return { ok: !rN.ok, parts: parts };
  }

  // fallback
  uiPrefix(parts, pad, false);
  uiText(parts, "условие не распознано: ");
  uiVal(parts, JSON.stringify(rule));
  uiNL(parts);

  parts = parts.concat(uiFoundBlockParts(value, pad, false));
  return { ok: false, parts: parts };
}

/* =======================
   Причины как блоки (titleParts + exp.parts)
   ======================= */

function makeTitleParts(prefixLabel, valueText) {
  return [
    { text: "┃", bold: true, color: UI.BORDER },
    { text: " ➔ " + String(prefixLabel), bold: true, color: UI.LABEL },
    { text: String(valueText) + "\n", bold: true, color: UI.VALUE }
  ];
}

function makePlainTitleParts(title) {
  return [
    { text: "┃", bold: true, color: UI.BORDER },
    { text: " ➔ " + String(title) + "\n", bold: true, color: UI.LABEL }
  ];
}

function checkProvinceCriteriaParts(province, criteria) {
  if (!criteria) return [];
  var out = [];
  for (var key in criteria) {
    var value = getValueByPath(province, key);
    if (!evaluateRule(criteria[key], value)) {
      out.push({
        titleParts: makeTitleParts("Критерий провинции", key),
        exp: explainRuleParts(criteria[key], value, 1)
      });
    }
  }
  return out;
}

function checkStateCriteriaParts(stateCtx, criteria) {
  if (!criteria) return [];
  var out = [];
  for (var key in criteria) {
    var value = stateCtx[key] || [];
    if (!evaluateRule(criteria[key], value)) {
      out.push({
        titleParts: makeTitleParts("Критерий государства", key),
        exp: explainRuleParts(criteria[key], value, 1)
      });
    }
  }
  return out;
}

function checkStatePropertyCriteriaParts(stateCtx, criteria, title) {
  if (!criteria) return [];
  var out = [];
  for (var key in criteria) {
    var value = stateCtx[key];
    if (!evaluateRule(criteria[key], value)) {
      out.push({
        titleParts: makeTitleParts(title, key),
        exp: explainRuleParts(criteria[key], value, 1)
      });
    }
  }
  return out;
}

function checkFactionCriteriaParts(stateCtx, criteria) {
  if (!criteria) return [];
  var out = [];
  for (var key in criteria) {
    var value = stateCtx[key] || [];
    if (!evaluateRule(criteria[key], value)) {
      out.push({
        titleParts: makeTitleParts("Фракции государства", key),
        exp: explainRuleParts(criteria[key], value, 1)
      });
    }
  }
  return out;
}

/* =======================
   Критерии построек (активные) -> parts
   ======================= */

function checkBuildingCriteriaParts(rule, ctx, level) {
  level = level || 0;
  var pad = indent(level);
  var parts = [];

  function logicLine(text) {
    parts.push({ text: "┃", bold: true, color: UI.BORDER });
    parts.push({ text: pad + text + "\n", bold: true, color: UI.LABEL });
  }

  function prefix(isOk) {
    parts.push({ text: "┃", bold: true, color: UI.BORDER });
    parts.push({ text: pad, color: UI.TEXT });
    parts.push({ text: "➔ ", bold: true, color: isOk ? UI.VALUE : UI.BAD });
  }

  function t(x) { parts.push({ text: String(x), color: UI.TEXT }); }
  function v(x) { parts.push({ text: String(x), bold: true, color: UI.VALUE }); }
  function nl() { parts.push({ text: "\n", color: UI.TEXT }); }

  if (rule.AND) {
    logicLine("Логика: AND");
    var resA = rule.AND.map(function (r) { return checkBuildingCriteriaParts(r, ctx, level + 1); });
    resA.forEach(function (r) { parts = parts.concat(r.parts); });
    return { ok: resA.every(function (r) { return r.ok; }), parts: parts };
  }

  if (rule.OR) {
    logicLine("Логика: OR");
    var resO = rule.OR.map(function (r) { return checkBuildingCriteriaParts(r, ctx, level + 1); });
    resO.forEach(function (r) { parts = parts.concat(r.parts); });
    return { ok: resO.some(function (r) { return r.ok; }), parts: parts };
  }

  if (rule.NOT) {
    logicLine("Логика: NOT");
    var rN = checkBuildingCriteriaParts(rule.NOT, ctx, level + 1);
    parts = parts.concat(rN.parts);
    return { ok: !rN.ok, parts: parts };
  }

  if (rule.XOR) {
    logicLine("Логика: XOR");
    var resX = rule.XOR.map(function (r) { return checkBuildingCriteriaParts(r, ctx, level + 1); });
    resX.forEach(function (r) { parts = parts.concat(r.parts); });

    var cnt = countTrue(resX.map(function (r) { return r.ok; }));
    var okX = (cnt === 1);

    prefix(okX);
    t("выполнено "); v(cnt); t(" из "); v(resX.length); nl();
    return { ok: okX, parts: parts };
  }

  // базовое: {Тип, Количество}
  var found = ctx[rule.Тип] || 0;
  var ok = evaluateRule(rule.Количество, found);

  prefix(ok);
  t("постройка "); v(rule.Тип); t(" (найдено: "); v(found); t(")"); nl();

  var exp = explainRuleParts(rule.Количество, found, level + 1);
  parts = parts.concat(exp.parts);

  return { ok: ok, parts: parts };
}

/* =======================
   Лимиты
   ======================= */

function applyLimit(list, limit, reason) {
  if (!limit || list.length <= limit) return;

  list.sort(function (a, b) { return a._turnBuilt - b._turnBuilt; });

  for (var i = limit; i < list.length; i++) {
    list[i]._blockedByLimit = true;
    list[i]._reasonsParts = list[i]._reasonsParts || [];

    list[i]._reasonsParts.push({
      titleParts: makePlainTitleParts("Лимит"),
      exp: (function () {
        var p = [];
        uiPrefix(p, indent(1), false);
        uiText(p, "причина: ");
        uiVal(p, reason);
        uiNL(p);
        return { ok: false, parts: p };
      })()
    });
  }
}

/* =======================
   Уведомление по постройке (формат как рынок труда)
   ======================= */

function pushBuildingNotice(data, b, statusOk) {
  var parts = [];
  ensureNews(data);

  // Заголовок + рамка (как в трудовом рынке)
  uiTitle(parts, "Проверка критериев постройки", UI.BORDER);
  uiTop(parts, UI.BORDER);

  uiRow(parts, "Здание", b.Тип, UI.VALUE, UI.BORDER);
  uiRow(parts, "Провинция", b.Провинция, UI.VALUE, UI.BORDER);
  uiRow(parts, "Статус", statusOk ? "Активная" : "Неактивная", statusOk ? UI.VALUE : UI.BAD, UI.BORDER);

  var reasons = b._reasonsParts || [];
  if (!statusOk && reasons.length) {
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
    sub: "Проверка критериев",
    priority: 100,
    parts: parts
  });
}

/* =======================
   ОСНОВНАЯ ФУНКЦИЯ
   ======================= */

function processCriteriaCheck(data) {
  ensureNews(data);

  /* === ПОСТРОЙКИ === */
  var buildings = [];
  var STATE_CONTEXT = buildStateContext(data);

  normalizeToArray(data.Постройки).forEach(function (row) {
    normalizeToArray(row).forEach(function (b) {
      if (b && b.Тип && b.Провинция) {
        var copy = {};
        for (var k in b) copy[k] = b[k];
        copy._originalRef = b;
        buildings.push(copy);
      }
    });
  });

  if (!buildings.length) {
    pushErrorNotice(data, "NO_VALID_BUILDINGS", "Нет валидных построек (нужны поля: Тип, Провинция).");
    return data;
  }

  /* === ГОСУДАРСТВО: stateId === */
  var stateId = null;
  if (STATE_CONTEXT['Идентификатор государства'] && STATE_CONTEXT['Идентификатор государства'].length) {
    stateId = String(STATE_CONTEXT['Идентификатор государства'][0]).trim();
  }
  if (!stateId) {
    pushErrorNotice(
      data,
      "STATE_ID_NOT_FOUND",
      "Идентификатор государства не найден в data['Данные государства'] (ключ: 'Идентификатор государства')."
    );
    return data;
  }

  /* === ПРОВИНЦИИ === */
  var provinces = getAllProvinces(data);

  var ourProvincesMap = {};
  provinces.forEach(function (p) {
    var key = p.Провинция || p.Название || p.id;
    var isOur = key && String(p.Владелец || '') === stateId;
    if (isOur) ourProvincesMap[key] = true;
    p._isOur = isOur;
  });

  /* === АКТИВНЫЕ ПОСТРОЙКИ (контекст) === */
  var ACTIVE_BUILDINGS_CONTEXT = buildActiveBuildingsContext(data, ourProvincesMap);

  /* === ШАБЛОНЫ === */
  var TEMPLATES = {};
  normalizeToArray(data['Шаблоны зданий']).forEach(function (row) {
    normalizeToArray(row).forEach(function (t) {
      if (t && t.Тип) TEMPLATES[t.Тип] = t;
    });
  });

  /* === ХОДЫ === */
  var maxTurn = 0;
  buildings.forEach(function (b) {
    if (typeof b.ХодСтроительства === 'number') maxTurn = Math.max(maxTurn, b.ХодСтроительства);
  });

  /* === ПРОВЕРКА === */
  buildings.forEach(function (b) {
    b._potential = true;
    b._blockedByLimit = false;
    b._reasonsParts = [];

    if (typeof b.ХодСтроительства !== 'number') b.ХодСтроительства = ++maxTurn;
    b._turnBuilt = b.ХодСтроительства;

    var tpl = TEMPLATES[b.Тип];
    var prov = findProvince(provinces, b.Провинция);

    if (!tpl) {
      b._reasonsParts.push({
        titleParts: makeTitleParts("Шаблон постройки", b.Тип),
        exp: (function () {
          var p = [];
          uiPrefix(p, indent(1), false);
          uiText(p, "ошибка: ");
          uiVal(p, "неизвестный тип постройки");
          uiNL(p);
          return { ok: false, parts: p };
        })()
      });
      b._potential = false;
      return;
    }

    if (!prov) {
      b._reasonsParts.push({
        titleParts: makeTitleParts("Провинция", b.Провинция),
        exp: (function () {
          var p = [];
          uiPrefix(p, indent(1), false);
          uiText(p, "ошибка: ");
          uiVal(p, "провинция не найдена");
          uiNL(p);
          return { ok: false, parts: p };
        })()
      });
      b._potential = false;
      return;
    }

    b._isOurProvince = !!prov._isOur;

    // чужая провинция — исключаем молча
    if (!b._isOurProvince) {
      b._potential = false;
      return;
    }

    /* === ПРОЧНОСТЬ === */
    if (tpl.МинимальнаяПрочность !== undefined) {
      var durability = b.Прочность;
      if (!evaluateRule(tpl.МинимальнаяПрочность, durability)) {
        b._reasonsParts.push({
          titleParts: makePlainTitleParts("Прочность постройки"),
          exp: explainRuleParts(tpl.МинимальнаяПрочность, durability, 1)
        });
        b._potential = false;
      }
    }

    /* === РЕСУРСЫ ПРОВИНЦИИ === */
    if (tpl.ТребуемыеРесурсы) {
      var provResources = prov.Ресурсы || {};
      for (var res in tpl.ТребуемыеРесурсы) {
        var ruleRes = tpl.ТребуемыеРесурсы[res];
        var valRes = provResources[res];

        if (!evaluateRule(ruleRes, valRes)) {
          b._reasonsParts.push({
            titleParts: makeTitleParts("Ресурс провинции", res),
            exp: explainRuleParts(ruleRes, valRes, 1)
          });
          b._potential = false;
        }
      }
    }

    /* === КРИТЕРИИ ПРОВИНЦИИ === */
    var pr = checkProvinceCriteriaParts(prov, tpl.КритерииПровинции);
    if (pr.length) { b._reasonsParts = b._reasonsParts.concat(pr); b._potential = false; }

    /* === КРИТЕРИИ ГОСУДАРСТВА === */
    if (tpl.КритерииГосударства) {
      var sr = checkStateCriteriaParts(STATE_CONTEXT, tpl.КритерииГосударства);
      if (sr.length) { b._reasonsParts = b._reasonsParts.concat(sr); b._potential = false; }
    }

    /* === СТАБИЛЬНОСТЬ / КУЛЬТУРЫ / РЕЛИГИИ / РАСЫ === */
    if (tpl.КритерииСтабильностиГосударства) {
      var r1 = checkStatePropertyCriteriaParts(STATE_CONTEXT, tpl.КритерииСтабильностиГосударства, "Стабильность государства");
      if (r1.length) { b._reasonsParts = b._reasonsParts.concat(r1); b._potential = false; }
    }
    if (tpl.КритерииКультурГосударства) {
      var r2 = checkStatePropertyCriteriaParts(STATE_CONTEXT, tpl.КритерииКультурГосударства, "Признанные культуры");
      if (r2.length) { b._reasonsParts = b._reasonsParts.concat(r2); b._potential = false; }
    }
    if (tpl.КритерииРелигийГосударства) {
      var r3 = checkStatePropertyCriteriaParts(STATE_CONTEXT, tpl.КритерииРелигийГосударства, "Признанные религии");
      if (r3.length) { b._reasonsParts = b._reasonsParts.concat(r3); b._potential = false; }
    }
    if (tpl.КритерииРасГосударства) {
      var r4 = checkStatePropertyCriteriaParts(STATE_CONTEXT, tpl.КритерииРасГосударства, "Признанные расы");
      if (r4.length) { b._reasonsParts = b._reasonsParts.concat(r4); b._potential = false; }
    }

    /* === ФРАКЦИИ === */
    if (tpl.КритерииФракцийГосударства) {
      var fr = checkFactionCriteriaParts(STATE_CONTEXT, tpl.КритерииФракцийГосударства);
      if (fr.length) { b._reasonsParts = b._reasonsParts.concat(fr); b._potential = false; }
    }

    /* === КРИТЕРИИ ПОСТРОЕК (по активным) === */
    if (tpl.КритерииПостроек) {
      var fail = false;

      if (tpl.КритерииПостроек.Провинция) {
        var ctxP = ACTIVE_BUILDINGS_CONTEXT.Провинция[b.Провинция] || {};
        var rP = checkBuildingCriteriaParts(tpl.КритерииПостроек.Провинция, ctxP, 1);
        if (!rP.ok) {
          fail = true;
          b._reasonsParts.push({ titleParts: makePlainTitleParts("Критерии построек в провинции"), exp: { ok: false, parts: rP.parts } });
        }
      }

      if (tpl.КритерииПостроек.Государство) {
        var rS = checkBuildingCriteriaParts(tpl.КритерииПостроек.Государство, ACTIVE_BUILDINGS_CONTEXT.Государство, 1);
        if (!rS.ok) {
          fail = true;
          b._reasonsParts.push({ titleParts: makePlainTitleParts("Критерии построек в государстве"), exp: { ok: false, parts: rS.parts } });
        }
      }

      if (tpl.КритерииПостроек.Мир) {
        var rW = checkBuildingCriteriaParts(tpl.КритерииПостроек.Мир, ACTIVE_BUILDINGS_CONTEXT.Мир, 1);
        if (!rW.ok) {
          fail = true;
          b._reasonsParts.push({ titleParts: makePlainTitleParts("Критерии построек в мире"), exp: { ok: false, parts: rW.parts } });
        }
      }

      if (fail) b._potential = false;
    }
  });

  /* === ЛИМИТЫ === */
  for (var type in TEMPLATES) {
    var t = TEMPLATES[type];
    if (!t.Лимит) continue;

    if (t.Лимит.Провинция) {
      var map = {};
      buildings.forEach(function (b) {
        if (b._potential && b.Тип === type) {
          if (!map[b.Провинция]) map[b.Провинция] = [];
          map[b.Провинция].push(b);
        }
      });
      for (var p in map) {
        applyLimit(map[p], t.Лимит.Провинция, "Превышен лимит на провинцию (" + t.Лимит.Провинция + ")");
      }
    }

    if (t.Лимит.Государство) {
      applyLimit(
        buildings.filter(function (b) { return b._potential && b.Тип === type && b._isOurProvince; }),
        t.Лимит.Государство,
        "Превышен лимит на государство (" + t.Лимит.Государство + ")"
      );
    }
  }

  /* === ИТОГ: Активно + pushNotice === */
  buildings.forEach(function (b) {
    var o = b._originalRef;
    var statusOk = !!b._isOurProvince && !!b._potential && !b._blockedByLimit;
    o.Активно = statusOk;
    pushBuildingNotice(data, b, statusOk);
  });

  provinces.forEach(function (p) { delete p._isOur; });

  return data;
}