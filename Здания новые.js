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
      pad + (ok ? '☑ ' : '☐ ') + rule
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
      pad + (ok ? '☑ ' : '❌ ') +
      'значение между ' + rule.BETWEEN[0] + ' и ' + rule.BETWEEN[1] +
      ' (найдено: ' + (v === undefined ? 'отсутствует' : v) + ')'
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
        pad + (ok ? '☑ ' : '❌ ') +
        'значение ' + op + ' ' + rule[op] +
        ' (найдено: ' + (value === undefined ? 'отсутствует' : value) + ')'
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
    if (!evaluateRule(criteria[key], getValueByPath(province, key))) {
      reasons.push('Параметр "' + key + '": ' +
        explainRule(criteria[key], getValueByPath(province, key)));
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
        'Государственный параметр "' + key + '":\n' +
        exp.lines.join('\n')
      );
    }
  }

  return reasons;
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

  /* === ШАБЛОНЫ === */
  var TEMPLATES = {};
  normalizeToArray(data['Шаблоны зданий']).forEach(function (row) {
    normalizeToArray(row).forEach(function (t) {
      if (t && t.Тип) TEMPLATES[t.Тип] = t;
    });
  });

  /* === ПРОВИНЦИИ === */
  var provinces = getAllProvinces(data);
  provinces.forEach(function (p) {
    p._isOur = String(p.Владелец || '') === stateId;
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
    if (!b._isOurProvince || !b._potential || b._blockedByLimit) {
      o.Активно = false;
      if (b._reasons.length) {
        data.Новости.push('"' + b.Тип + '" в ' + b.Провинция +
          ' остановлена: ' + b._reasons.join('; '));
      }
    } else {
      o.Активно = true;
      data.Новости.push('"' + b.Тип + '" в ' + b.Провинция + ' работает');
    }
  });

  provinces.forEach(function (p) { delete p._isOur; });

  return data;
}