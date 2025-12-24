/* =========================================================
   УНИВЕРСАЛЬНЫЙ ДВИЖОК КРИТЕРИЕВ ДЛЯ ПРОИЗВОДСТВА
   (Google Apps Script, V8)
   ========================================================= */

/* =======================
   ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
   ======================= */

function normalizeToArray(value) {
  if (Array.isArray(value)) return value;
  if (value === undefined || value === null) return [];
  return [value];
}

function getValueByPath(obj, path) {
  return path.split('.').reduce(function (o, k) {
    return o && o[k];
  }, obj);
}

/* =======================
   РАБОТА С ПРОВИНЦИЯМИ
   ======================= */

/**
 * Разворачивает data.Провинции:
 * [ [], [p1,p2], null, [p3] ] -> [p1,p2,p3]
 */
function getAllProvinces(provincesData) {
  if (!Array.isArray(provincesData)) return [];

  return provincesData
    .flat()
    .filter(function (p) {
      return p && typeof p === 'object';
    });
}

/**
 * Находит конкретную провинцию для здания
 * item.Провинция может быть:
 *  - названием
 *  - id
 */
function findProvinceForBuilding(allProvinces, provinceKey) {
  return allProvinces.find(function (p) {
    return p.Провинция === provinceKey ||
           p.Название === provinceKey ||
           p.id === provinceKey;
  });
}

/* =======================
   ЧИСЛОВЫЕ ОПЕРАТОРЫ
   ======================= */

function evaluateNumericRule(rule, value) {
  if (typeof value !== 'number') return false;

  if (rule['>'] !== undefined) return value > rule['>'];
  if (rule['<'] !== undefined) return value < rule['<'];
  if (rule['>='] !== undefined) return value >= rule['>='];
  if (rule['<='] !== undefined) return value <= rule['<='];
  if (rule['=='] !== undefined) return value === rule['=='];
  if (rule['!='] !== undefined) return value !== rule['!='];

  if (rule.BETWEEN) {
    return value >= rule.BETWEEN[0] && value <= rule.BETWEEN[1];
  }

  return false;
}

/* =======================
   ЛОГИЧЕСКИЙ ИНТЕРПРЕТАТОР
   ======================= */

function evaluateRule(rule, provinceValue) {

  if (
    typeof rule === 'object' &&
    !Array.isArray(rule) &&
    Object.keys(rule).some(function (k) {
      return ['>', '<', '>=', '<=', '==', '!=', 'BETWEEN'].indexOf(k) !== -1;
    })
  ) {
    return evaluateNumericRule(rule, provinceValue);
  }

  if (typeof rule === 'string') {
    return normalizeToArray(provinceValue).indexOf(rule) !== -1;
  }

  if (rule.AND) return rule.AND.every(function (r) { return evaluateRule(r, provinceValue); });
  if (rule.OR) return rule.OR.some(function (r) { return evaluateRule(r, provinceValue); });
  if (rule.NOT) return !evaluateRule(rule.NOT, provinceValue);
  if (rule.NAND) return !rule.NAND.every(function (r) { return evaluateRule(r, provinceValue); });
  if (rule.NOR) return !rule.NOR.some(function (r) { return evaluateRule(r, provinceValue); });

  if (rule.XOR) {
    var count = rule.XOR.filter(function (r) {
      return evaluateRule(r, provinceValue);
    }).length;
    return count === 1;
  }

  return false;
}

/* =======================
   ОБЪЯСНЕНИЕ ПРОВАЛА
   ======================= */

function explainRule(rule, provinceValue) {
  var value = provinceValue;
  var has = normalizeToArray(provinceValue);

  if (typeof rule === 'string') {
    return 'требуется "' + rule + '", но есть [' + (has.join(', ') || 'пусто') + ']';
  }

  if (typeof rule === 'object' && !Array.isArray(rule)) {

    if (rule['>'] !== undefined) return 'значение ' + value + ' должно быть > ' + rule['>'];
    if (rule['<'] !== undefined) return 'значение ' + value + ' должно быть < ' + rule['<'];
    if (rule['>='] !== undefined) return 'значение ' + value + ' должно быть ≥ ' + rule['>='];
    if (rule['<='] !== undefined) return 'значение ' + value + ' должно быть ≤ ' + rule['<='];
    if (rule['=='] !== undefined) return 'значение ' + value + ' должно быть = ' + rule['=='];
    if (rule['!='] !== undefined) return 'значение ' + value + ' не должно быть = ' + rule['!='];

    if (rule.BETWEEN) {
      return 'значение ' + value + ' должно быть между ' + rule.BETWEEN[0] + ' и ' + rule.BETWEEN[1];
    }

    if (rule.AND) {
      return 'не выполнены условия: ' + rule.AND
        .filter(function (r) { return !evaluateRule(r, provinceValue); })
        .map(function (r) { return explainRule(r, provinceValue); })
        .join('; ');
    }

    if (rule.OR) return 'ни одно из условий OR не выполнено';
    if (rule.NOT) return 'условие должно быть ложным';
  }

  return 'неизвестное правило';
}

/* =======================
   ПРОВЕРКА КРИТЕРИЕВ
   ======================= */

function checkProvinceCriteria(province, criteria) {
  if (!criteria) return { passes: true, reasons: [] };

  var reasons = [];

  for (var key in criteria) {
    var rule = criteria[key];
    var value = getValueByPath(province, key);

    if (!evaluateRule(rule, value)) {
      reasons.push('"' + key + '": ' + explainRule(rule, value));
    }
  }

  return { passes: reasons.length === 0, reasons: reasons };
}

/* =======================
   ОСНОВНАЯ ФУНКЦИЯ
   ======================= */

function processCriteriaCheck(data) {

  if (!data.Новости) data.Новости = [];

  if (!Array.isArray(data.Постройки)) {
    data.Новости.push('Ошибка: список "Постройки" отсутствует.');
    return;
  }

  var allProvinces = getAllProvinces(data.Провинции);

  if (allProvinces.length === 0) {
    data.Новости.push('Ошибка: провинции отсутствуют.');
    return;
  }

  var skipped = 0;

  data.Постройки.forEach(function (item) {

    if (!item || typeof item !== 'object') {
      skipped++;
      return;
    }

    if (item.Активно === false) return;
    if (!item.Уровень || item.Уровень < 1) item.Уровень = 1;

    var template = BUILDING_TEMPLATES[item.Тип];
    var reasons = [];
    var canWork = true;

    if (!template) {
      canWork = false;
      reasons.push('неизвестный тип постройки');
    }

    var province = findProvinceForBuilding(allProvinces, item.Провинция);

    if (!province) {
      canWork = false;
      reasons.push('провинция постройки не найдена');
    }

    if (canWork && template.КритерииПровинции) {
      var check = checkProvinceCriteria(province, template.КритерииПровинции);
      if (!check.passes) {
        canWork = false;
        reasons = reasons.concat(check.reasons);
      }
    }

    if (!canWork) {
      item.Активно = false;
      data.Новости.push(
        'Постройка "' + item.Тип +
        '" остановлена (провинция "' + item.Провинция + '"): ' +
        reasons.join('; ')
      );
    } else {
      data.Новости.push(
        'Постройка "' + item.Тип +
        '" (ур. ' + item.Уровень +
        ', провинция "' + item.Провинция + '") работает'
      );
    }
  });

  if (skipped > 0) {
    data.Новости.push('Пропущено некорректных записей: ' + skipped);
  }
}