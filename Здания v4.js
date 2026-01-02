/* =========================================================
   УНИВЕРСАЛЬНЫЙ ДВИЖОК КРИТЕРИЕВ + ЛИМИТЫ С ПРИОРИТЕТОМ
   Google Apps Script (V8)
   ЦЕЛЬ: все сообщения о критериях — в стиле твоего pushNotice (рамка + "┃ ➔" + цвета)
   ========================================================= */

/* =======================
   UI: стиль сообщений (как в примере)
   ======================= */

var UI = {
  BORDER: "#FF8C00",
  LABEL:  "#CFC7BA",
  VALUE:  "#E6E6FA",
  OK:     "#6EE06E",
  BAD:    "#E36A6A",
  TEXT:   "#B9B1A4",
  DIM:    "#6E675F",
  TOP:    "┌────────────────────────────────────────────────────────┐\n",
  BOT:    "└────────────────────────────────────────────────────────┘\n"
};

function uiTitle(parts, text) {
  parts.push({ text: String(text) + "\n", bold: true, color: UI.BORDER });
}

function uiTop(parts) {
  parts.push({ text: UI.TOP, color: UI.BORDER });
}

function uiBottom(parts) {
  parts.push({ text: UI.BOT, color: UI.BORDER });
}

function uiRow(parts, label, value, valueColor) {
  parts.push({ text: "┃", bold: true, color: UI.BORDER });
  parts.push({ text: " ➔ " + label + ": ", bold: true, color: UI.LABEL });
  parts.push({ text: String(value) + "\n", bold: true, color: valueColor || UI.VALUE });
}

function uiLine(parts, text, color, bold) {
  parts.push({ text: "┃", bold: true, color: UI.BORDER });
  parts.push({ text: " " + String(text) + "\n", color: color || UI.TEXT, bold: !!bold });
}

function uiBlank(parts) {
  parts.push({ text: "┃\n", color: UI.BORDER, bold: true });
}

function uiDivider(parts) {
  parts.push({ text: "┃", bold: true, color: UI.BORDER });
  parts.push({ text: "──────────────────────────────────────────────────────────\n", color: UI.DIM });
}

function pushBoxNotice(data, opts) {
  pushNotice(data, {
    category: opts.category || "Система",
    sub: opts.sub || "",
    priority: opts.priority || 100,
    parts: opts.parts || []
  });
}

/* =======================
   ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
   ======================= */

function indent(level) {
  return '  '.repeat(level);
}

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
   STATE CONTEXT
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

        if (Array.isArray(cell[key])) {
          ctx[key] = ctx[key].concat(cell[key]);
        } else if (typeof cell[key] === 'object') {
          ctx[key].push(cell[key]);
        } else {
          ctx[key].push(cell[key]);
        }
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
   ПРОВИНЦИИ
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
   RULE EVAL
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
  if (typeof rule === 'string') {
    return normalizeToArray(value).indexOf(rule) !== -1;
  }
  if (Array.isArray(rule)) {
    return rule.some(function (r) { return evaluateRule(r, value); });
  }
  if (typeof rule === 'object' && rule !== null) {
    if (Object.keys(rule).some(function (k) {
      return ['>','<','>=','<=','==','!=','BETWEEN'].indexOf(k) !== -1;
    })) {
      return evaluateNumericRule(rule, value);
    }
    if (rule.AND) return rule.AND.every(function (r) { return evaluateRule(r, value); });
    if (rule.OR) return rule.OR.some(function (r) { return evaluateRule(r, value); });
    if (rule.NOT) return !evaluateRule(rule.NOT, value);
    if (rule.NAND) return !rule.NAND.every(function (r) { return evaluateRule(r, value); });
    if (rule.NOR) return !rule.NOR.some(function (r) { return evaluateRule(r, value); });
    if (rule.XOR) {
      var c = 0;
      rule.XOR.forEach(function (r) { if (evaluateRule(r, value)) c++; });
      return c === 1;
    }
  }
  return false;
}

/* =======================
   explainRuleParts: возвращает parts с КРАСНОЙ/ЗЕЛЁНОЙ "➔"
   ВАЖНО: Никаких эмодзи, только unicode, но цвет управляется через parts
   ======================= */

function explainRuleParts(rule, value, level) {
  level = level || 0;
  var pad = indent(level);
  var parts = [];

  function addLine(isOk, text) {
    parts.push({ text: "┃", bold: true, color: UI.BORDER });
    parts.push({ text: pad, color: UI.TEXT });
    parts.push({ text: "➔ ", bold: true, color: isOk ? UI.OK : UI.BAD });
    parts.push({ text: String(text) + "\n", color: UI.TEXT });
  }

  // STRING
  if (typeof rule === 'string') {
    var okS = hasValue(value, rule);
    addLine(okS, rule);
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
    var okB = typeof v === 'number' && v >= rule.BETWEEN[0] && v <= rule.BETWEEN[1];
    addLine(okB, "значение BETWEEN " + rule.BETWEEN[0] + " .. " + rule.BETWEEN[1] +
                 " (найдено: " + (v === undefined ? "Отсутствует" : v) + ")");
    return { ok: okB, parts: parts };
  }

  // OPS
  var ops = ['>','<','>=','<=','==','!='];
  for (var i = 0; i < ops.length; i++) {
    var op = ops[i];
    if (rule && rule[op] !== undefined) {
      var okOp = typeof value === 'number' && eval(value + op + rule[op]);
      addLine(okOp, "значение " + op + " " + rule[op] +
                    " (найдено: " + (value === undefined ? "Отсутствует" : value) + ")");
      return { ok: okOp, parts: parts };
    }
  }

  // AND
  if (rule && rule.AND) {
    parts.push({ text: "┃", bold: true, color: UI.BORDER });
    parts.push({ text: pad + "Логика: AND\n", bold: true, color: UI.LABEL });

    var rA = rule.AND.map(function (r) { return explainRuleParts(r, value, level + 1); });
    var okAnd = rA.every(function (r) { return r.ok; });
    rA.forEach(function (r) { parts = parts.concat(r.parts); });
    return { ok: okAnd, parts: parts };
  }

  // OR
  if (rule && rule.OR) {
    parts.push({ text: "┃", bold: true, color: UI.BORDER });
    parts.push({ text: pad + "Логика: OR\n", bold: true, color: UI.LABEL });

    var rO = rule.OR.map(function (r) { return explainRuleParts(r, value, level + 1); });
    var okOr = rO.some(function (r) { return r.ok; });
    rO.forEach(function (r) { parts = parts.concat(r.parts); });
    return { ok: okOr, parts: parts };
  }

  // XOR
  if (rule && rule.XOR) {
    parts.push({ text: "┃", bold: true, color: UI.BORDER });
    parts.push({ text: pad + "Логика: XOR\n", bold: true, color: UI.LABEL });

    var rX = rule.XOR.map(function (r) { return explainRuleParts(r, value, level + 1); });
    var cnt = countTrue(rX.map(function (r) { return r.ok; }));
    var okX = cnt === 1;

    rX.forEach(function (r) { parts = parts.concat(r.parts); });

    addLine(okX, "выполнено " + cnt + " из " + rX.length);
    return { ok: okX, parts: parts };
  }

  // NOT
  if (rule && rule.NOT) {
    parts.push({ text: "┃", bold: true, color: UI.BORDER });
    parts.push({ text: pad + "Логика: NOT\n", bold: true, color: UI.LABEL });

    var rN = explainRuleParts(rule.NOT, value, level + 1);
    parts = parts.concat(rN.parts);
    return { ok: !rN.ok, parts: parts };
  }

  // fallback
  addLine(false, "условие не распознано: " + JSON.stringify(rule));
  return { ok: false, parts: parts };
}

/* =======================
   CHECKS: теперь возвращают reasons как parts
   ======================= */

function checkProvinceCriteriaParts(province, criteria) {
  if (!criteria) return [];
  var out = [];

  for (var key in criteria) {
    var value = getValueByPath(province, key);
    if (!evaluateRule(criteria[key], value)) {
      out.push({ title: "Критерий провинции: " + key, exp: explainRuleParts(criteria[key], value, 1) });
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
      out.push({ title: "Критерий государства: " + key, exp: explainRuleParts(criteria[key], value, 1) });
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
      out.push({ title: title + ": " + key, exp: explainRuleParts(criteria[key], value, 1) });
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
      out.push({ title: "Фракции государства: " + key, exp: explainRuleParts(criteria[key], value, 1) });
    }
  }
  return out;
}

/* =======================
   BUILDING CRITERIA (постройки) -> parts
   ======================= */

function checkBuildingCriteriaParts(rule, ctx, level) {
  level = level || 0;
  var pad = indent(level);
  var parts = [];

  function logicLine(text) {
    parts.push({ text: "┃", bold: true, color: UI.BORDER });
    parts.push({ text: pad + text + "\n", bold: true, color: UI.LABEL });
  }

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
    var okX = cnt === 1;

    parts.push({ text: "┃", bold: true, color: UI.BORDER });
    parts.push({ text: pad, color: UI.TEXT });
    parts.push({ text: "➔ ", bold: true, color: okX ? UI.OK : UI.BAD });
    parts.push({ text: "выполнено " + cnt + " из " + resX.length + "\n", color: UI.TEXT });

    return { ok: okX, parts: parts };
  }

  // Базовое правило: {Тип, Количество}
  var found = ctx[rule.Тип] || 0;
  var ok = evaluateRule(rule.Количество, found);

  parts.push({ text: "┃", bold: true, color: UI.BORDER });
  parts.push({ text: pad, color: UI.TEXT });
  parts.push({ text: "➔ ", bold: true, color: ok ? UI.OK : UI.BAD });
  parts.push({ text: rule.Тип + " (найдено: " + found + ")\n", color: UI.TEXT });

  // Детализация количества — через explainRuleParts (тоже цветные ➔)
  var exp = explainRuleParts(rule.Количество, found, level + 1);
  parts = parts.concat(exp.parts);

  return { ok: ok, parts: parts };
}

/* =======================
   ЛИМИТЫ
   ======================= */

function applyLimit(list, limit, reason) {
  if (!limit || list.length <= limit) return;
  list.sort(function (a, b) { return a._turnBuilt - b._turnBuilt; });
  for (var i = limit; i < list.length; i++) {
    list[i]._blockedByLimit = true;
    list[i]._reasonsParts = list[i]._reasonsParts || [];
    list[i]._reasonsParts.push({
      title: "Лимит",
      exp: { ok: false, parts: (function () {
        var p = [];
        // одна строка лимита в стиле ➔
        p.push({ text: "┃", bold: true, color: UI.BORDER });
        p.push({ text: indent(1), color: UI.TEXT });
        p.push({ text: "➔ ", bold: true, color: UI.BAD });
        p.push({ text: String(reason) + "\n", color: UI.TEXT });
        return p;
      })() }
    });
  }
}

/* =======================
   PUSH NOTICE: постройка (СТРОГО как в твоём примере)
   ======================= */

function pushBuildingNotice(data, b, statusOk) {
  var parts = [];

  uiTitle(parts, b.Тип + " в " + b.Провинция);
  uiTop(parts);

  uiRow(parts, "Статус", statusOk ? "работает" : "не работает", statusOk ? UI.OK : UI.BAD);
  uiRow(parts, "Провинция наша", String(!!b._isOurProvince), UI.VALUE);

  // Причины из parts
  var reasons = (b._reasonsParts || []);
  if (!statusOk && reasons.length) {
    uiDivider(parts);
    uiLine(parts, "Причины:", UI.LABEL, true);

    reasons.forEach(function (block) {
      uiBlank(parts);
      uiLine(parts, block.title, UI.LABEL, true);

      if (block.exp && block.exp.parts && block.exp.parts.length) {
        // block.exp.parts уже содержит "┃" и переносы
        parts = parts.concat(block.exp.parts);
      }
    });
  }

  uiBottom(parts);

  pushBoxNotice(data, {
    category: "Постройки",
    sub: "Проверка критериев",
    priority: 100,
    parts: parts
  });
}

function pushErrorNotice(data, code, message) {
  var parts = [];
  uiTitle(parts, "Ошибка");
  uiTop(parts);
  uiRow(parts, "Код", code, UI.BAD);
  uiDivider(parts);
  uiLine(parts, message, UI.BAD, true);
  uiBottom(parts);

  pushBoxNotice(data, {
    category: "Система",
    sub: "Проверка критериев",
    priority: 250,
    parts: parts
  });
}

/* =======================
   ОСНОВНАЯ ФУНКЦИЯ
   ======================= */

function processCriteriaCheck(data) {
  data.Новости = data.Новости || [];

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
    pushErrorNotice(data, "NO_VALID_BUILDINGS", "Нет валидных построек (нужны поля Тип и Провинция).");
    return data;
  }

  /* === ГОСУДАРСТВО: извлекаем stateId корректно === */
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

  /* === АКТИВНЫЕ ПОСТРОЙКИ КОНТЕКСТ === */
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
        title: "Шаблон постройки",
        exp: explainRuleParts("Шаблон существует", null, 1) // будет NO
      });
      b._potential = false;
      return;
    }
    if (!prov) {
      b._reasonsParts.push({
        title: "Провинция",
        exp: { ok: false, parts: (function () {
          var p = [];
          p.push({ text: "┃", bold: true, color: UI.BORDER });
          p.push({ text: indent(1), color: UI.TEXT });
          p.push({ text: "➔ ", bold: true, color: UI.BAD });
          p.push({ text: "провинция не найдена\n", color: UI.TEXT });
          return p;
        })() }
      });
      b._potential = false;
      return;
    }

    b._isOurProvince = !!prov._isOur;

    // Чужая провинция: исключаем
    if (!b._isOurProvince) {
      b._potential = false;
      return;
    }

    /* === ПРОЧНОСТЬ === */
    if (tpl.МинимальнаяПрочность !== undefined) {
      var durability = b.Прочность;
      if (!evaluateRule(tpl.МинимальнаяПрочность, durability)) {
        b._reasonsParts.push({
          title: "Прочность постройки",
          exp: explainRuleParts(tpl.МинимальнаяПрочность, durability, 1)
        });
        b._potential = false;
      }
    }

    /* === РЕСУРСЫ ПРОВИНЦИИ === */
    if (tpl.ТребуемыеРесурсы) {
      var provResources = prov.Ресурсы || {};
      for (var res in tpl.ТребуемыеРесурсы) {
        var rule = tpl.ТребуемыеРесурсы[res];
        var value = provResources[res];

        if (!evaluateRule(rule, value)) {
          b._reasonsParts.push({
            title: "Ресурс провинции: " + res,
            exp: explainRuleParts(rule, value, 1)
          });
          b._potential = false;
        }
      }
    }

    /* === КРИТЕРИИ ПРОВИНЦИИ === */
    var pr = checkProvinceCriteriaParts(prov, tpl.КритерииПровинции);
    if (pr.length) {
      b._reasonsParts = b._reasonsParts.concat(pr);
      b._potential = false;
    }

    /* === КРИТЕРИИ ГОСУДАРСТВА === */
    if (tpl.КритерииГосударства) {
      var sr = checkStateCriteriaParts(STATE_CONTEXT, tpl.КритерииГосударства);
      if (sr.length) {
        b._reasonsParts = b._reasonsParts.concat(sr);
        b._potential = false;
      }
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

    /* === КРИТЕРИИ ПОСТРОЕК (по активным зданиям) === */
    if (tpl.КритерииПостроек) {
      var fail = false;

      if (tpl.КритерииПостроек.Провинция) {
        var ctxP = ACTIVE_BUILDINGS_CONTEXT.Провинция[b.Провинция] || {};
        var rP = checkBuildingCriteriaParts(tpl.КритерииПостроек.Провинция, ctxP, 1);
        if (!rP.ok) {
          fail = true;
          b._reasonsParts.push({ title: "Критерии построек в провинции", exp: { ok: false, parts: rP.parts } });
        }
      }

      if (tpl.КритерииПостроек.Государство) {
        var rS = checkBuildingCriteriaParts(tpl.КритерииПостроек.Государство, ACTIVE_BUILDINGS_CONTEXT.Государство, 1);
        if (!rS.ok) {
          fail = true;
          b._reasonsParts.push({ title: "Критерии построек в государстве", exp: { ok: false, parts: rS.parts } });
        }
      }

      if (tpl.КритерииПостроек.Мир) {
        var rW = checkBuildingCriteriaParts(tpl.КритерииПостроек.Мир, ACTIVE_BUILDINGS_CONTEXT.Мир, 1);
        if (!rW.ok) {
          fail = true;
          b._reasonsParts.push({ title: "Критерии построек в мире", exp: { ok: false, parts: rW.parts } });
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

  /* === ИТОГ: выставляем Активно + пушим карточки === */
  buildings.forEach(function (b) {
    var o = b._originalRef;
    var statusOk = !!b._isOurProvince && !!b._potential && !b._blockedByLimit;

    o.Активно = statusOk;

    // Сообщение в стиле твоего примера
    pushBuildingNotice(data, b, statusOk);
  });

  provinces.forEach(function (p) { delete p._isOur; });

  return data;
}