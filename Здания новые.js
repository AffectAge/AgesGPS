/* =========================================================
   УНИВЕРСАЛЬНЫЙ ДВИЖОК КРИТЕРИЕВ + ЛИМИТЫ С ПРИОРИТЕТОМ
   (Google Apps Script, V8)
   Версия: полная поддержка массивов построек в ячейках
   Исправлено: дублирование сообщений при одинаковых постройках
   Добавлено: критерий соседства с опцией только наши/все провинции и полной логикой (AND/OR/NOT и т.д.)
   Добавлено: критерии государства (законы, технологии, культурные институты, религиозные догмы)
   Улучшения: исправлены опечатки в строках объяснений; добавлено детальное объяснение ошибок для ТребуемыеСоседи;
              улучшена читаемость (добавлены комментарии, рефакторинг функций); добавлена обработка отсутствующих полей;
              оптимизирована фильтрация соседей; учтены только потенциальные постройки в подсчётах требований
   ========================================================= */

/* =======================
   ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
   ======================= */

function normalizeToArray(value) {
  if (Array.isArray(value)) return value;
  if (value === null || value === undefined) return [];
  return [value];
}

function getValueByPath(obj, path) {
  return path.split('.').reduce((o, k) => (o ? o[k] : undefined), obj);
}

/* =======================
   ПРОВИНЦИИ
   ======================= */

function getAllProvinces(data) {
  if (!data.Провинции) return [];
  return normalizeToArray(data.Провинции).flat(Infinity).filter(p => p && typeof p === 'object');
}

function findProvince(all, key) {
  return all.find(p =>
    p.Провинция === key ||
    p.Название === key ||
    p.id === key
  );
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
    return normalizeToArray(value).includes(rule);
  }
  if (Array.isArray(rule)) {
    return rule.some(r => evaluateRule(r, value)); // Массив по умолчанию OR
  }

  if (typeof rule === 'object' && !Array.isArray(rule)) {
    if (Object.keys(rule).some(k => ['>','<','>=','<=','==','!=','BETWEEN'].includes(k))) {
      return evaluateNumericRule(rule, value);
    }
    if (rule.AND) return rule.AND.every(r => evaluateRule(r, value));
    if (rule.OR) return rule.OR.some(r => evaluateRule(r, value));
    if (rule.NOT) return !evaluateRule(rule.NOT, value);
    if (rule.NAND) return !rule.NAND.every(r => evaluateRule(r, value));
    if (rule.NOR) return !rule.NOR.some(r => evaluateRule(r, value));
    if (rule.XOR) return rule.XOR.filter(r => evaluateRule(r, value)).length === 1;
  }
  return false;
}

function explainRule(rule, value) {
  if (typeof rule === 'string') {
    return `требуется "\( {rule}", найдено: [ \){normalizeToArray(value).join(', ') || 'пусто'}]`;
  }
  if (Array.isArray(rule)) {
    return rule.map(r => explainRule(r, value)).join(' или ');
  }
  if (rule.BETWEEN) {
    return `значение ${value} должно быть между ${rule.BETWEEN[0]} и ${rule.BETWEEN[1]}`;
  }
  if (rule.AND) return `все: (${rule.AND.map(r => explainRule(r, value)).join('; ')})`;
  if (rule.OR) return `хотя бы одно: (${rule.OR.map(r => explainRule(r, value)).join('; ')})`;
  if (rule.NOT) return `не (${explainRule(rule.NOT, value)})`;
  // Аналогично для NAND, NOR, XOR можно расширить, но для простоты используем общее
  return 'условие не выполнено';
}

function checkProvinceCriteria(province, criteria) {
  if (!criteria) return [];
  var reasons = [];
  for (var key in criteria) {
    var rule = criteria[key];
    var value = getValueByPath(province, key);
    if (!evaluateRule(rule, value)) {
      reasons.push(`Параметр "${key}": ${explainRule(rule, value)}`);
    }
  }
  return reasons;
}

function checkStateCriteria(data, criteria) {
  if (!criteria) return [];
  var reasons = [];
  for (var key in criteria) {
    var rule = criteria[key];
    var value = data[key] || []; // Предполагаем, что это массивы, как Законы, Технологии и т.д.
    if (!evaluateRule(rule, value)) {
      reasons.push(`Государственный параметр "${key}": ${explainRule(rule, value)}`);
    }
  }
  return reasons;
}

/* =======================
   ЛИМИТЫ С ПРИОРИТЕТОМ
   ======================= */

function applyLimit(candidates, limit, reason) {
  if (!limit || candidates.length <= limit) return;

  candidates
    .sort((a, b) => a._turnBuilt - b._turnBuilt) // старые важнее
    .forEach((item, idx) => {
      if (idx >= limit) {
        item._blockedByLimit = true;
        item._reasons.push(reason);
      }
    });
}

/* =======================
   ОСНОВНАЯ ФУНКЦИЯ
   ======================= */

function processCriteriaCheck(data) {

  // Инициализация Новости
  data.Новости = data.Новости || [];

  // === НОРМАЛИЗАЦИЯ ПОСТРОЕК ===
  if (!data.Постройки) {
    data.Новости.push('Ошибка: отсутствует список построек.');
    return data;
  }

  // Собираем все валидные постройки с прямой ссылкой на оригинал
  var buildings = [];
  normalizeToArray(data.Постройки)
    .flat(Infinity)
    .forEach(item => {
      if (item && typeof item === 'object' && item.Тип && item.Провинция) {
        var copy = { ...item };
        copy._originalRef = item; // ссылка на оригинальный объект в data
        buildings.push(copy);
      }
    });

  if (buildings.length === 0) {
    data.Новости.push('Ошибка: не найдено ни одной валидной постройки (нужны поля Тип и Провинция).');
    return data;
  }

  /* === ГОСУДАРСТВО === */
  var stateId;
  if (Array.isArray(data['Идентификатор данных государства']) &&
      Array.isArray(data['Данные государства'])) {
    var k = data['Идентификатор данных государства'];
    var v = data['Данные государства'];
    var i = k.indexOf('Идентификатор государства');
    if (i !== -1) stateId = String(v[i]).trim();
  }

  if (!stateId) {
    data.Новости.push('Идентификатор государства не найден.');
    return data;
  }

  /* === ШАБЛОНЫ === */
  var TEMPLATES = {};
  normalizeToArray(data['Шаблоны зданий'] || [])
    .flat(Infinity)
    .forEach(t => {
      if (t?.Тип) TEMPLATES[t.Тип] = t;
    });

  /* === ПРОВИНЦИИ === */
  var allProvinces = getAllProvinces(data);
  // Добавляем флаг _isOur для провинций
  allProvinces.forEach(p => {
    p._isOur = String(p.Владелец || '').trim() === stateId; // Добавлена обработка отсутствия Владелец
  });

  /* === ХОД ПОСТРОЙКИ === */
  var maxTurn = buildings.reduce(
    (m, b) => typeof b.ХодСтроительства === 'number' ? Math.max(m, b.ХодСтроительства) : m,
    0
  );

  /* =======================
     ПЕРВЫЙ ПРОХОД — ЛОКАЛЬНЫЕ УСЛОВИЯ
     ======================= */

  buildings.forEach(b => {
    b._reasons = [];
    b._potential = true;
    b._blockedByLimit = false;

    if (typeof b.ХодСтроительства !== 'number') {
      b.ХодСтроительства = ++maxTurn;
    }
    b._turnBuilt = b.ХодСтроительства;

    var template = TEMPLATES[b.Тип];
    var province = findProvince(allProvinces, b.Провинция);

    if (!template) {
      b._reasons.push(`Неизвестный тип постройки "${b.Тип}"`);
      b._potential = false;
      return;
    }

    if (!province) {
      b._reasons.push('Провинция не найдена');
      b._potential = false;
      return;
    }

    /* Флаг принадлежности */
    b._isOurProvince = province._isOur;

    /* Критерии провинции */
    var crit = checkProvinceCriteria(province, template.КритерииПровинции);
    if (crit.length) {
      b._reasons.push(...crit);
      b._potential = false;
    }

    /* Критерии государства (законы, технологии и т.д.) */
    if (b._isOurProvince && template.КритерииГосударства) {
      var stateCrit = checkStateCriteria(data, template.КритерииГосударства);
      if (stateCrit.length) {
        b._reasons.push(...stateCrit);
        b._potential = false;
      }
    }

    /* Требуемые постройки (в той же провинции) */
    if (template.ТребуемыеПостройки) {
      var req = template.ТребуемыеПостройки;
      var count = buildings.filter(x =>
        x !== b &&
        x.Провинция === b.Провинция &&
        x.Тип === req.Тип &&
        x._potential // Учёт только потенциальных построек (улучшение)
      ).length;

      if (count < (req.Минимум || 0)) { // Обработка отсутствия Минимум
        b._reasons.push(
          `Требуется минимум \( {req.Минимум || 0} " \){req.Тип}" в провинции, найдено ${count}`
        );
        b._potential = false;
      }
    }

    /* Требуемые постройки в соседних провинциях — с полной логикой и детальным объяснением */
    if (template.ТребуемыеСоседи) {
      var neighbors = normalizeToArray(province.Соседи || []).map(n => findProvince(allProvinces, n)).filter(p => p);
      
      // Функция подсчёта построек по типу в соседях с фильтром ТолькоНаши
      function countInNeighbors(type, onlyOur) {
        var filtered = onlyOur ? neighbors.filter(p => p._isOur) : neighbors;
        return filtered.reduce((sum, neigh) => {
          return sum + buildings.filter(x =>
            x.Провинция === neigh.Провинция && x.Тип === type && x._potential // Учёт только потенциальных
          ).length;
        }, 0);
      }

      // Функция оценки одного правила
      function evaluateNeighborRule(rule) {
        if (typeof rule === 'object' && rule.Тип !== undefined) {
          var count = countInNeighbors(rule.Тип, !!rule.ТолькоНаши);
          var min = rule.Минимум !== undefined ? rule.Минимум : 1;
          return count >= min;
        }
        if (rule.AND) return rule.AND.every(evaluateNeighborRule);
        if (rule.OR) return rule.OR.some(evaluateNeighborRule);
        if (rule.NOT) return !evaluateNeighborRule(rule.NOT);
        if (rule.NAND) return !rule.NAND.every(evaluateNeighborRule);
        if (rule.NOR) return !rule.NOR.some(evaluateNeighborRule);
        if (rule.XOR) return rule.XOR.filter(evaluateNeighborRule).length === 1;
        return false;
      }

      // Функция объяснения одного правила (аналог explainRule)
      function explainNeighborRule(rule, prefix = '') {
        if (typeof rule === 'object' && rule.Тип !== undefined) {
          var count = countInNeighbors(rule.Тип, !!rule.ТолькоНаши);
          var min = rule.Минимум !== undefined ? rule.Минимум : 1;
          var onlyOurStr = rule.ТолькоНаши ? ' (только наши)' : '';
          return `${prefix}требуется минимум \( {min} " \){rule.Тип}" в соседних провинциях${onlyOurStr}, найдено ${count}`;
        }
        if (rule.AND) return `\( {prefix}все: ( \){rule.AND.map(r => explainNeighborRule(r)).join('; ')})`;
        if (rule.OR) return `\( {prefix}хотя бы одно: ( \){rule.OR.map(r => explainNeighborRule(r)).join('; ')})`;
        if (rule.NOT) return `\( {prefix}не ( \){explainNeighborRule(rule.NOT)})`;
        // Аналогично для NAND, NOR, XOR
        if (rule.NAND) return `\( {prefix}не все: ( \){rule.NAND.map(r => explainNeighborRule(r)).join('; ')})`;
        if (rule.NOR) return `\( {prefix}ни одно: ( \){rule.NOR.map(r => explainNeighborRule(r)).join('; ')})`;
        if (rule.XOR) return `\( {prefix}ровно одно: ( \){rule.XOR.map(r => explainNeighborRule(r)).join('; ')})`;
        return `${prefix}условие не выполнено`;
      }

      // Основная проверка
      var rules = normalizeToArray(template.ТребуемыеСоседи);
      var unsatisfied = rules.filter(r => !evaluateNeighborRule(r));
      if (unsatisfied.length > 0) {
        var neighborReasons = unsatisfied.map(r => explainNeighborRule(r, 'Соседи: '));
        b._reasons.push(...neighborReasons);
        b._potential = false;
      }
    }

  });

  /* =======================
     ВТОРОЙ ПРОХОД — ЛИМИТЫ
     ======================= */

  Object.keys(TEMPLATES).forEach(type => {
    var t = TEMPLATES[type];
    if (!t.Лимит) return;

    /* Лимит на провинцию */
    if (t.Лимит.Провинция) {
      var byProv = {};
      buildings.forEach(b => {
        if (b._potential && b.Тип === type) {
          var prov = b.Провинция;
          if (!byProv[prov]) byProv[prov] = [];
          byProv[prov].push(b);
        }
      });
      Object.values(byProv).forEach(list =>
        applyLimit(list, t.Лимит.Провинция,
          `Превышен лимит на провинцию (${t.Лимит.Провинция})`)
      );
    }

    /* Лимит на государство (только наши провинции) */
    if (t.Лимит.Государство) {
      var stateList = buildings.filter(b =>
        b._potential && b.Тип === type && b._isOurProvince
      );
      applyLimit(stateList, t.Лимит.Государство,
        `Превышен лимит на государство (${t.Лимит.Государство})`);
    }

    /* Глобальный лимит (весь мир) */
    if (t.Лимит.Мир) {
      var worldList = buildings.filter(b =>
        b._potential && b.Тип === type
      );
      applyLimit(worldList, t.Лимит.Мир,
        `Превышен глобальный лимит (${t.Лимит.Мир})`);
    }
  });

  /* =======================
     ТРЕТИЙ ПРОХОД — ИТОГ + ЗАПИСЬ В ОРИГИНАЛЫ
     ======================= */

  buildings.forEach(b => {
    var original = b._originalRef;

    if (!b._isOurProvince) {
      original.Активно = false;
    } else if (!b._potential || b._blockedByLimit) {
      original.Активно = false;
      if (b._reasons && b._reasons.length > 0) {
        data.Новости.push(
          `"${b.Тип}" в ${b.Провинция} остановлена: ${b._reasons.join('; ')}`
        );
      }
    } else {
      original.Активно = true;
      data.Новости.push(
        `"${b.Тип}" в ${b.Провинция} работает`
      );
    }

    // Очистка временных полей из копии
    delete b._reasons;
    delete b._potential;
    delete b._blockedByLimit;
    delete b._turnBuilt;
    delete b._isOurProvince;
    delete b._originalRef;
  });

  // Очистка временных полей из провинций
  allProvinces.forEach(p => {
    delete p._isOur;
  });

  return data;
}