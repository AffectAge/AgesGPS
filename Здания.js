/* =========================================================
   УНИВЕРСАЛЬНЫЙ ДВИЖОК КРИТЕРИЕВ + ЛИМИТЫ С ПРИОРИТЕТОМ
   (Google Apps Script, V8)
   Версия: полная поддержка массивов построек в ячейках
   Исправлено: дублирование сообщений при одинаковых постройках
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
  if (rule.BETWEEN) {
    return `значение ${value} должно быть между ${rule.BETWEEN[0]} и ${rule.BETWEEN[1]}`;
  }
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
    b._isOurProvince = String(province.Владелец).trim() === stateId;

    /* Критерии провинции */
    var crit = checkProvinceCriteria(province, template.КритерииПровинции);
    if (crit.length) {
      b._reasons.push(...crit);
      b._potential = false;
    }

    /* Требуемые постройки */
    if (template.ТребуемыеПостройки) {
      var req = template.ТребуемыеПостройки;
      var count = buildings.filter(x =>
        x !== b &&
        x.Провинция === b.Провинция &&
        x.Тип === req.Тип
      ).length;

      if (count < req.Минимум) {
        b._reasons.push(
          `Требуется минимум \( {req.Минимум} " \){req.Тип}" в провинции, найдено ${count}`
        );
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

  return data;
}