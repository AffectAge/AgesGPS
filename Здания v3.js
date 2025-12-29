/* =========================================================
   УНИВЕРСАЛЬНЫЙ ДВИЖОК КРИТЕРИЕВ + ЛИМИТЫ С ПРИОРИТЕТОМ
   Google Apps Script
   ========================================================= */

/* =======================
   ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
   ======================= */

function indent(level) {
  return '  '.repeat(level);
}

function hasValue(value, v) {
  return normalizeToArray(value).indexOf(v) !== -1;
}

function countTrue(arr) {
  return arr.filter(Boolean).length;
}

function buildStateContext(data) {
  var ctx = {};

  var rows = normalizeToArray(data['Данные государства']);

  rows.forEach(function (row) {
    normalizeToArray(row).forEach(function (cell) {

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

function buildBuildingsContext(buildings) {
  var ctx = {
    Провинция: {},
    Государство: {},
    Мир: {}
  };

  buildings.forEach(function (b) {
    if (!b._potential) return;

    // 🌍 Мир
    ctx.Мир[b.Тип] = (ctx.Мир[b.Тип] || 0) + 1;

    // 🏘 Провинция
    if (!ctx.Провинция[b.Провинция])
      ctx.Провинция[b.Провинция] = {};
    ctx.Провинция[b.Провинция][b.Тип] =
      (ctx.Провинция[b.Провинция][b.Тип] || 0) + 1;

    // 🏛 Государство
    if (b._isOurProvince) {
      ctx.Государство[b.Тип] =
        (ctx.Государство[b.Тип] || 0) + 1;
    }
  });

  return ctx;
}

function buildActiveBuildingsContext(data, provincesMap, stateId) {
  var ctx = {
    Провинция: {},
    Государство: {},
    Мир: {}
  };

  // provincesMap — это объект { "Название провинции": true } для наших провинций
  normalizeToArray(data.Постройки).forEach(function (row) {
    normalizeToArray(row).forEach(function (b) {
      if (!b || !b.Тип || !b.Провинция) return;
      if (b.Активно !== true) return; // только уже активные

      var provKey = b.Провинция;
      var isOur = provincesMap[provKey];

      // 🌍 Мир — считаем все активные постройки в мире
      ctx.Мир[b.Тип] = (ctx.Мир[b.Тип] || 0) + 1;

      // 🏘 Провинция — считаем все активные в этой конкретной провинции (независимо от владельца)
      if (!ctx.Провинция[provKey]) ctx.Провинция[provKey] = {};
      ctx.Провинция[provKey][b.Тип] = (ctx.Провинция[provKey][b.Тип] || 0) + 1;

      // 🏛 Государство — только если провинция принадлежит нам
      if (isOur) {
        ctx.Государство[b.Тип] = (ctx.Государство[b.Тип] || 0) + 1;
      }
    });
  });

  return ctx;
}

function normalizeToArray(value) {
  if (Array.isArray(value)) return value;
  if (value === null || value === undefined) return [];
  return [value];
}

function getValueByPath(obj, path) {
  return path.split('.').reduce(function (o, k) {
    return o ? o[k] : undefined;
  }, obj);
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
   ПРАВИЛА
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
    return rule.some(function (r) {
      return evaluateRule(r, value);
    });
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
      rule.XOR.forEach(function (r) {
        if (evaluateRule(r, value)) c++;
      });
      return c === 1;
    }
  }
  return false;
}

function explainRule(rule, value) {
  if (typeof rule === 'string') {
    return 'требуется "' + rule + '", найдено: [' +
      (normalizeToArray(value).join(', ') || 'пусто') + ']';
  }
  if (Array.isArray(rule)) {
    return rule.map(function (r) {
      return explainRule(r, value);
    }).join(' или ');
  }
  if (rule.BETWEEN) {
    return 'значение ' + value + ' должно быть между ' +
      rule.BETWEEN[0] + ' и ' + rule.BETWEEN[1];
  }
  if (rule.AND) return 'все: (' + rule.AND.map(function (r) {
    return explainRule(r, value);
  }).join('; ') + ')';
  if (rule.OR) return 'хотя бы одно: (' + rule.OR.map(function (r) {
    return explainRule(r, value);
  }).join('; ') + ')';
  if (rule.NOT) return 'не (' + explainRule(rule.NOT, value) + ')';
  return 'не выполнено условие: ' + JSON.stringify(rule) +
       ', найдено: [' + normalizeToArray(value).join(', ') + ']';
}

function explainRuleTable(rule, value, level) {
  level = level || 0;
  var pad = indent(level);
  var lines = [];

  /* === СТРОКА === */
  if (typeof rule === 'string') {
    var ok = hasValue(value, rule);
    lines.push(
      pad + (ok ? '✅️ ' : '⏹️ ') + rule
    );
    return { ok: ok, lines: lines };
  }

  /* === МАССИВ === */
  if (Array.isArray(rule)) {
    var results = rule.map(function (r) {
      return explainRuleTable(r, value, level + 1);
    });

    results.forEach(function (r) {
      lines = lines.concat(r.lines);
    });

    return {
      ok: results.some(function (r) { return r.ok; }),
      lines: lines
    };
  }

  /* === ЧИСЛОВЫЕ === */
  if (rule.BETWEEN) {
    var v = value;
    var ok = typeof v === 'number' &&
      v >= rule.BETWEEN[0] &&
      v <= rule.BETWEEN[1];

    lines.push(
      pad + (ok ? '✅️ ' : '⛔️ ') +
      'Значение между ' + rule.BETWEEN[0] + ' и ' + rule.BETWEEN[1] +
      ' (Найдено: ' + (v === undefined ? 'Отсутствует' : v) + ')'
    );
    return { ok: ok, lines: lines };
  }

  var ops = ['>','<','>=','<=','==','!='];
  for (var i = 0; i < ops.length; i++) {
    var op = ops[i];
    if (rule[op] !== undefined) {
      var ok = typeof value === 'number' &&
        eval(value + op + rule[op]);

      lines.push(
        pad + (ok ? '✅️ ' : '⛔️ ') +
        'значение ' + op + ' ' + rule[op] +
        ' (Найдено: ' + (value === undefined ? 'Отсутствует' : value) + ')'
      );
      return { ok: ok, lines: lines };
    }
  }

  /* === AND === */
  if (rule.AND) {
    lines.push(pad + 'Логика: AND (все условия обязательны)');
    var results = rule.AND.map(function (r) {
      return explainRuleTable(r, value, level + 1);
    });

    results.forEach(function (r) {
      lines = lines.concat(r.lines);
    });

    return {
      ok: results.every(function (r) { return r.ok; }),
      lines: lines
    };
  }

  /* === OR === */
  if (rule.OR) {
    lines.push(pad + 'Логика: OR (достаточно одного)');
    var results = rule.OR.map(function (r) {
      return explainRuleTable(r, value, level + 1);
    });

    results.forEach(function (r) {
      lines = lines.concat(r.lines);
    });

    return {
      ok: results.some(function (r) { return r.ok; }),
      lines: lines
    };
  }

  /* === XOR === */
  if (rule.XOR) {
    lines.push(pad + 'Логика: XOR (ровно одно условие)');
    var results = rule.XOR.map(function (r) {
      return explainRuleTable(r, value, level + 1);
    });

    results.forEach(function (r) {
      lines = lines.concat(r.lines);
    });

    var cnt = countTrue(results.map(function (r) { return r.ok; }));

    return {
      ok: cnt === 1,
      lines: lines.concat([
        pad + '→ выполнено ' + cnt + ' из ' + results.length
      ])
    };
  }

  /* === NOT === */
  if (rule.NOT) {
    lines.push(pad + 'НЕ должно выполняться:');
    var r = explainRuleTable(rule.NOT, value, level + 1);
    lines = lines.concat(r.lines);

    return {
      ok: !r.ok,
      lines: lines
    };
  }

  /* === FALLBACK === */
  lines.push(
    pad + '❌ условие не распознано'
  );
  return { ok: false, lines: lines };
}

function checkProvinceCriteria(province, criteria) {
  if (!criteria) return [];
  var reasons = [];

  for (var key in criteria) {
    var value = getValueByPath(province, key);
    if (!evaluateRule(criteria[key], value)) {
      var exp = explainRuleTable(criteria[key], value);
      reasons.push(
        '\n' +
        '🏠 ' + key +
        '\n➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️' + '\n' +
        exp.lines.join('\n') + '\n'
      );
    }
  }

  return reasons;
}

function checkStateCriteria(stateCtx, criteria) {
  if (!criteria) return [];
  var reasons = [];

  for (var key in criteria) {
    var value = stateCtx[key] || [];
    if (!evaluateRule(criteria[key], value)) {
      var exp = explainRuleTable(criteria[key], value);
      reasons.push(
        '\n' +
        '🏛 ' + key +
        '\n➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️' + '\n' +
        exp.lines.join('\n') + '\n'
      );
    }
  }

  return reasons;
}

function checkFactionCriteria(stateCtx, criteria) {
  if (!criteria) return [];
  var reasons = [];

  for (var key in criteria) {
    var value = stateCtx[key] || [];

    if (!evaluateRule(criteria[key], value)) {
      var exp = explainRuleTable(criteria[key], value);
      reasons.push(
        '\n' +
        '🏴 Фракции государства\n' +
        '➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️\n' +
        exp.lines.join('\n') + '\n'
      );
    }
  }

  return reasons;
}

function checkBuildingCriteria(rule, ctx, level, title) {
  level = level || 0;
  var pad = indent(level);
  var lines = [];

  /* === ЛОГИКА === */
  if (rule.AND) {
    lines.push(pad + 'Логика: AND');
    var res = rule.AND.map(r =>
      checkBuildingCriteria(r, ctx, level + 1)
    );
    res.forEach(r => lines = lines.concat(r.lines));
    return {
      ok: res.every(r => r.ok),
      lines: lines
    };
  }

  if (rule.OR) {
    lines.push(pad + 'Логика: OR');
    var res = rule.OR.map(r =>
      checkBuildingCriteria(r, ctx, level + 1)
    );
    res.forEach(r => lines = lines.concat(r.lines));
    return {
      ok: res.some(r => r.ok),
      lines: lines
    };
  }

  if (rule.NOT) {
    lines.push(pad + 'НЕ должно выполняться:');
    var r = checkBuildingCriteria(rule.NOT, ctx, level + 1);
    lines = lines.concat(r.lines);
    return { ok: !r.ok, lines: lines };
  }

  if (rule.XOR) {
    lines.push(pad + 'Логика: XOR');
    var res = rule.XOR.map(r =>
      checkBuildingCriteria(r, ctx, level + 1)
    );
    res.forEach(r => lines = lines.concat(r.lines));
    var cnt = countTrue(res.map(r => r.ok));
    return {
      ok: cnt === 1,
      lines: lines.concat([
        pad + '→ выполнено ' + cnt + ' из ' + res.length
      ])
    };
  }

  /* === БАЗОВОЕ ПРАВИЛО === */
  var found = ctx[rule.Тип] || 0;
  var ok = evaluateRule(rule.Количество, found);

  var exp = explainRuleTable(rule.Количество, found, level + 1);
  lines.push(
    pad + (ok ? '✅️ ' : '⛔️ ') +
    rule.Тип + ' (найдено: ' + found + ')'
  );
  lines = lines.concat(exp.lines);

  return { ok: ok, lines: lines };
}

/* =======================
   ЛИМИТЫ
   ======================= */

function applyLimit(list, limit, reason) {
  if (!limit || list.length <= limit) return;
  list.sort(function (a, b) {
    return a._turnBuilt - b._turnBuilt;
  });
  for (var i = limit; i < list.length; i++) {
    list[i]._blockedByLimit = true;
    list[i]._reasons.push(reason);
  }
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
    data.Новости.push('Ошибка: нет валидных построек');
    return data;
  }
  
  /* === ГОСУДАРСТВО === */
  var stateId = null;
  if (Array.isArray(data['Идентификатор данных государства']) &&
      Array.isArray(data['Данные государства'])) {
    var keys = data['Идентификатор данных государства'];
    var vals = data['Данные государства'];
    var idx = keys.indexOf('Идентификатор государства');
    if (idx !== -1) stateId = String(vals[idx]).trim();
  }

  if (!stateId) {
    data.Новости.push('Ошибка: идентификатор государства не найден');
    return data;
  }
  
  

  /* === ПРОВИНЦИИ === */
  var provinces = getAllProvinces(data);

  // Создаём быстрый lookup: какие провинции наши
  var ourProvincesMap = {};
  provinces.forEach(function (p) {
    var key = p.Провинция || p.Название || p.id;
    if (key && String(p.Владелец || '') === stateId) {
      ourProvincesMap[key] = true;
    }
    // Заодно помечаем для удобства (как было раньше)
    p._isOur = String(p.Владелец || '') === stateId;
  });

  /* === АКТИВНЫЕ ПОСТРОЙКИ КОНТЕКСТ === */
  var ACTIVE_BUILDINGS_CONTEXT = buildActiveBuildingsContext(data, ourProvincesMap, stateId);

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
    if (typeof b.ХодСтроительства === 'number') {
      maxTurn = Math.max(maxTurn, b.ХодСтроительства);
    }
  });

  /* === ПРОВЕРКА === */
  buildings.forEach(function (b) {

    b._reasons = [];
    b._potential = true;
    b._blockedByLimit = false;

    if (typeof b.ХодСтроительства !== 'number') {
      b.ХодСтроительства = ++maxTurn;
    }
    b._turnBuilt = b.ХодСтроительства;

    var tpl = TEMPLATES[b.Тип];
    var prov = findProvince(provinces, b.Провинция);

    if (!tpl) {
      b._reasons.push('Неизвестный тип постройки');
      b._potential = false;
      return;
    }
    if (!prov) {
      b._reasons.push('Провинция не найдена');
      b._potential = false;
      return;
    }

    b._isOurProvince = prov._isOur;

// ⛔️ Чужая провинция — полностью исключаем из логики
if (!b._isOurProvince) {
  b._potential = false;
  return;
}

/* === ПРОЧНОСТЬ === */
if (tpl.МинимальнаяПрочность !== undefined) {
  var durability = b.Прочность;

  if (!evaluateRule(tpl.МинимальнаяПрочность, durability)) {
    var exp = explainRuleTable(tpl.МинимальнаяПрочность, durability);
    b._reasons.push(
      '\n' +
      '🛠 Прочность постройки\n' +
      '➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️\n' +
      exp.lines.join('\n') + '\n'
    );
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
      var exp = explainRuleTable(rule, value);

      b._reasons.push(
        '\n' +
        '⛏ Требуемый ресурс: ' + res + '\n' +
        '➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️\n'  +
        exp.lines.join('\n') + '\n'
      );

      b._potential = false;
    }
  }
}

    var pr = checkProvinceCriteria(prov, tpl.КритерииПровинции);
    if (pr.length) {
      b._reasons = b._reasons.concat(pr);
      b._potential = false;
    }

    if (b._isOurProvince && tpl.КритерииГосударства) {
      var sr = checkStateCriteria(STATE_CONTEXT, tpl.КритерииГосударства);
      if (sr.length) {
        b._reasons = b._reasons.concat(sr);
        b._potential = false;
      }
    }
    
    /* === ФРАКЦИИ ГОСУДАРСТВА === */
if (b._isOurProvince && tpl.КритерииФракцийГосударства) {
  var fr = checkFactionCriteria(
    STATE_CONTEXT,
    tpl.КритерииФракцийГосударства
  );

  if (fr.length) {
    b._reasons = b._reasons.concat(fr);
    b._potential = false;
  }
}
    
      if (tpl.КритерииПостроек) {

  var fail = false;

  if (tpl.КритерииПостроек.Провинция) {
    var ctx = ACTIVE_BUILDINGS_CONTEXT.Провинция[b.Провинция] || {};
    var r = checkBuildingCriteria(
      tpl.КритерииПостроек.Провинция,
      ctx
    );

    if (!r.ok) {
      fail = true;
      b._reasons.push(
        '\n🏘 Критерии построек в провинции\n' +
        '➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️\n' +
        r.lines.join('\n')
      );
    }
  }

  if (tpl.КритерииПостроек.Государство && b._isOurProvince) {
    var r = checkBuildingCriteria(
      tpl.КритерииПостроек.Государство,
      ACTIVE_BUILDINGS_CONTEXT.Государство
    );

    if (!r.ok) {
      fail = true;
      b._reasons.push(
        '\n🏛 Критерии построек в государстве\n' +
        '➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️\n' +
        r.lines.join('\n')
      );
    }
  }

  if (tpl.КритерииПостроек.Мир) {
    var r = checkBuildingCriteria(
      tpl.КритерииПостроек.Мир,
      ACTIVE_BUILDINGS_CONTEXT.Мир
    );

    if (!r.ok) {
      fail = true;
      b._reasons.push(
        '\n🌍 Критерии построек в мире\n' +
        '➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️\n' +
        r.lines.join('\n')
      );
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
        applyLimit(map[p], t.Лимит.Провинция,
          'Превышен лимит на провинцию (' + t.Лимит.Провинция + ')');
      }
    }

    if (t.Лимит.Государство) {
      applyLimit(buildings.filter(function (b) {
        return b._potential && b.Тип === type && b._isOurProvince;
      }), t.Лимит.Государство,
      'Превышен лимит на государство (' + t.Лимит.Государство + ')');
    }
  }

  /* === ИТОГ === */
buildings.forEach(function (b) {
  var o = b._originalRef;

  // Начало рамки для постройки
  var header = '🧱🧱🧱🧱🧱🧱🧱 Постройка 🧱🧱🧱🧱🧱🧱🧱\n' +
               '' + b.Тип + ' в ' + b.Провинция + '' +
               '';

  if (!b._isOurProvince || !b._potential || b._blockedByLimit) {
    o.Активно = false;
    if (b._reasons.length) {
      data.Новости.push(
        header + '\n' +
        b._reasons.join('\n') + '\n' +
        '\n'
      );
    }
  } else {
    o.Активно = true;
    data.Новости.push(
      header + '' +
      ' работает' +
      '\n'
    );
  }
});

  provinces.forEach(function (p) { delete p._isOur; });

  return data;
}