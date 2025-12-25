/* =========================================================
УНИВЕРСАЛЬНЫЙ ДВИЖОК КРИТЕРИЕВ ДЛЯ ПРОИЗВОДСТВА
(Google Apps Script, V8) — ПОЛНАЯ АКТУАЛЬНАЯ ВЕРСИЯ
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

function getAllProvinces(provincesData) {
  if (!Array.isArray(provincesData)) return [];

  return provincesData
    .flat()
    .filter(function (p) {
      return p && typeof p === 'object';
    });
}

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
    return 'требуется наличие "' + rule + '", но в провинции найдено [' + (has.join(', ') || 'пусто') + ']. Чтобы исправить: добавьте "' + rule + '" в соответствующий параметр провинции.';
  }

  if (typeof rule === 'object' && !Array.isArray(rule)) {
    if (rule['>'] !== undefined) return 'значение ' + value + ' должно быть > ' + rule['>'] + '. Чтобы исправить: увеличьте значение до > ' + rule['>'] + '.';
    if (rule['<'] !== undefined) return 'значение ' + value + ' должно быть < ' + rule['<'] + '. Чтобы исправить: уменьшите значение до < ' + rule['<'] + '.';
    if (rule['>='] !== undefined) return 'значение ' + value + ' должно быть ≥ ' + rule['>='] + '. Чтобы исправить: увеличьте значение до ≥ ' + rule['>='] + '.';
    if (rule['<='] !== undefined) return 'значение ' + value + ' должно быть ≤ ' + rule['<='] + '. Чтобы исправить: уменьшите значение до ≤ ' + rule['<='] + '.';
    if (rule['=='] !== undefined) return 'значение ' + value + ' должно быть = ' + rule['=='] + '. Чтобы исправить: установите значение равным ' + rule['=='] + '.';
    if (rule['!='] !== undefined) return 'значение ' + value + ' не должно быть = ' + rule['!='] + '. Чтобы исправить: измените значение на любое, кроме ' + rule['!='] + '.';

    if (rule.BETWEEN) {  
      return 'значение ' + value + ' должно быть между ' + rule.BETWEEN[0] + ' и ' + rule.BETWEEN[1] + '. Чтобы исправить: скорректируйте значение в диапазон [' + rule.BETWEEN[0] + ', ' + rule.BETWEEN[1] + '].';  
    }  

    if (rule.AND) {  
      return 'не выполнены условия AND: ' + rule.AND  
        .filter(function (r) { return !evaluateRule(r, provinceValue); })  
        .map(function (r) { return explainRule(r, provinceValue); })  
        .join('; ');  
    }  

    if (rule.OR) return 'ни одно из условий OR не выполнено. Чтобы исправить: выполните хотя бы одно из: ' + rule.OR.map(function (r) { return explainRule(r, provinceValue); }).join('; ');  
    if (rule.NOT) return 'условие должно быть ложным, но оно истинно. Чтобы исправить: инвертируйте условие NOT: ' + explainRule(rule.NOT, provinceValue);
  }

  return 'неизвестное правило. Чтобы исправить: проверьте структуру правила.';
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
      reasons.push('Параметр "' + key + '": ' + explainRule(rule, value));  
    }
  }

  return { passes: reasons.length === 0, reasons: reasons };
}

function processCriteriaCheck(data) {

  if (!data.Новости) data.Новости = [];

  if (!Array.isArray(data.Постройки)) {
    data.Новости.push('Ошибка: список "Постройки" отсутствует.');
    return data;
  }

  /* =======================
     ВАЛИДНЫЕ ПОСТРОЙКИ
     ======================= */
  var validBuildings = data.Постройки.filter(p =>
    p && typeof p === 'object'
  );

  /* =======================
     ИДЕНТИФИКАТОР ГОСУДАРСТВА
     ======================= */
  var stateId;

  if (Array.isArray(data['Данные государства']) &&
      Array.isArray(data['Идентификатор данных государства'])) {

    var keys = data['Идентификатор данных государства'];
    var values = data['Данные государства'];

    var idx = keys.indexOf('Идентификатор государства');
    if (idx !== -1 && values[idx] !== null && values[idx] !== undefined && String(values[idx]).trim() !== '') {
      stateId = String(values[idx]).trim();
    }
  }

  data.Новости.push('Используемый идентификатор государства: ' + (stateId ?? '(не задан)'));

  if (!stateId) {
    data.Новости.push('Идентификатор государства не найден. Обработка остановлена.');
    return data;
  }

  /* =======================
     ШАБЛОНЫ ЗДАНИЙ
     ======================= */
  var BUILDING_TEMPLATES = {};
  (data['Шаблоны зданий'] || []).forEach(t => {
    if (t && t.Тип) BUILDING_TEMPLATES[t.Тип] = t;
  });

  /* =======================
     ПРОВИНЦИИ
     ======================= */
  var allProvinces = getAllProvinces(data.Провинции);
  if (allProvinces.length === 0) {
    data.Новости.push('Ошибка: провинции отсутствуют.');
    return data;
  }

  /* =======================
     СТАТИСТИКА
     ======================= */
  var ourProvinces = allProvinces.filter(p =>
    p && String(p.Владелец).trim() === stateId
  );

  data.Новости.push('Всего провинций: ' + allProvinces.length);
  data.Новости.push('Провинций нашего государства: ' + ourProvinces.length);

  var buildingsInOurProvinces = 0;

  /* =======================
     ОСНОВНОЙ ЦИКЛ
     ======================= */
  validBuildings.forEach(item => {

    if (!item.Провинция || !item.Тип) return;

    if (!item.Уровень || item.Уровень < 1) item.Уровень = 1;

    var province = findProvinceForBuilding(allProvinces, item.Провинция);

    var isOurProvince = province &&
      province.Владелец !== undefined &&
      province.Владелец !== null &&
      String(province.Владелец).trim() === stateId;

    if (!isOurProvince) return;

    buildingsInOurProvinces++;

    var template = BUILDING_TEMPLATES[item.Тип];
    var reasons = [];

    if (!template) reasons.push('Неизвестный тип постройки "' + item.Тип + '". Чтобы исправить: добавьте шаблон для "' + item.Тип + '" в "Шаблоны зданий".');

    if (item.ПрогрессСтроительства !== undefined && item.ПрогрессСтроительства < 100) {
      reasons.push('Строительство не завершено (прогресс: ' + item.ПрогрессСтроительства + '%). Чтобы исправить: завершите строительство до 100%.');
    }

    /* === КРИТЕРИИ ПРОВИНЦИИ === */
    if (template?.КритерииПровинции && province) {
      var check = checkProvinceCriteria(province, template.КритерииПровинции);
      if (!check.passes) reasons.push(...check.reasons);
    }

    /* === ТРЕБУЕМЫЕ ПОСТРОЙКИ === */
    if (template?.ТребуемыеПостройки) {
      var req = template.ТребуемыеПостройки;
      var count = validBuildings.filter(p =>
        p.Провинция === item.Провинция &&
        p.Тип === req.Тип &&
        p.Активно &&
        p !== item
      ).length;

      if (count < req.Минимум) {
        reasons.push(`Требуется минимум \( {req.Минимум} активных построек типа " \){req.Тип}" в той же провинции, но найдено только \( {count}. Чтобы исправить: постройте и активируйте дополнительные " \){req.Тип}".`);
      }
    }

    /* === ЛИМИТЫ === */
    if (template?.Лимит) {

      if (template.Лимит.Провинция !== undefined) {
        var provCount = validBuildings.filter(p =>
          p.Провинция === item.Провинция &&
          p.Тип === item.Тип &&
          p.Активно &&
          p !== item
        ).length + 1;

        if (provCount > template.Лимит.Провинция) {
          reasons.push(`Превышен лимит на провинцию для "${item.Тип}": текущее количество ${provCount} > максимум ${template.Лимит.Провинция}. Чтобы исправить: деактивируйте или удалите лишние постройки этого типа в провинции.`);
        }
      }

      if (template.Лимит.Государство !== undefined) {
        var stateCount = validBuildings.filter(p => {
          var pp = findProvinceForBuilding(allProvinces, p.Провинция);
          return pp &&
            String(pp.Владелец).trim() === stateId &&
            p.Тип === item.Тип &&
            p.Активно &&
            p !== item;
        }).length + 1;

        if (stateCount > template.Лимит.Государство) {
          reasons.push(`Превышен лимит на государство для "${item.Тип}": текущее количество ${stateCount} > максимум ${template.Лимит.Государство}. Чтобы исправить: деактивируйте или удалите лишние постройки этого типа в государстве.`);
        }
      }
    }

    /* === ЛОЯЛЬНОСТЬ === */
    if (province.Лояльность !== undefined && province.Лояльность < 50) {
      reasons.push(`Низкая лояльность провинции: ${province.Лояльность} < 50. Чтобы исправить: повысьте лояльность провинции до 50 или выше.`);
    }

    /* === СРОК РАБОТЫ === */
    if (template?.СрокРаботы > 0) {
      if (item.ОставшийсяСрок === undefined) {
        item.ОставшийсяСрок = template.СрокРаботы;
      } else if (item.Активно) {
        item.ОставшийсяСрок--;
      }

      if (item.ОставшийсяСрок <= 0) {
        reasons.push(`Срок работы истёк: оставшийся срок ${item.ОставшийсяСрок} <= 0. Чтобы исправить: обновите или перестройте здание для сброса срока.`);
      }
    }

    /* === АВАРИИ === */
    if (template?.РискАварии && template.РискАварии[item.Уровень] !== undefined) {
      var risk = template.РискАварии[item.Уровень];
      if (Math.random() < risk) {
        reasons.push(`Произошла авария (риск для уровня ${item.Уровень}: ${ (risk * 100).toFixed(1) }%). Чтобы исправить: отремонтируйте здание или снизьте риск через улучшения.`);
      }
    }

    /* === РЕЗУЛЬТАТ === */
    if (reasons.length) {
      item.Активно = false;
      data.Новости.push(`"${item.Тип}" в провинции ${item.Провинция} остановлена: ${reasons.join('; ')}`);
    } else {
      item.Активно = true;
      data.Новости.push(`"${item.Тип}" в провинции ${item.Провинция} работает успешно!`);
    }

    /* === СИНЕРГИЯ === */
    if (item.Активно && template?.Синергия) {
      var syn = template.Синергия;
      var synCount = validBuildings.filter(p =>
        p.Провинция === item.Провинция &&
        p.Тип === syn.Тип &&
        p.Активно &&
        p !== item
      ).length;

      if (synCount >= (syn.Минимум || 1)) {
        data.Новости.push(`"${item.Тип}" в провинции ${item.Провинция} получает бонус синергии: ${syn.Бонус} (благодаря \( {synCount} " \){syn.Тип}")`);
      }
    }

  });

  data.Новости.push(`Всего построек в наших провинциях: ${buildingsInOurProvinces}`);
  return data;
}