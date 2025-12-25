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
ОСНОВНАЯ ФУНКЦИЯ — ПРОВЕРКА ПОСТРОЕК
======================= */

function processCriteriaCheck(data) {

  if (!data.Новости) data.Новости = [];

  if (!Array.isArray(data.Постройки)) {
    data.Новости.push('Ошибка: список "Постройки" отсутствует.');
    return data;
  }

  // === ПОЛУЧЕНИЕ ИДЕНТИФИКАТОРА ГОСУДАРСТВА ИЗ МАССИВОВ "Данные государства" ===
  var stateId = undefined;

  if (Array.isArray(data['Данные государства']) && 
      Array.isArray(data['Идентификатор данных государства'])) {
    
    const idents = data['Идентификатор данных государства'];
    const values = data['Данные государства'];
    
    const index = idents.indexOf('Идентификатор государства');
    if (index !== -1 && values[index] !== undefined && values[index] !== null && String(values[index]).trim() !== '') {
      stateId = values[index];
    }
  }

  // Приводим к строке для надёжного сравнения
  var stateIdStr = stateId !== undefined ? String(stateId).trim() : '(не задан)';

  data.Новости.push(`🔍 Используемый идентификатор государства: ${stateIdStr}`);

  if (stateId === undefined) {
    data.Новости.push('⚠️ Идентификатор государства не найден в данных листа. Дальнейшая обработка невозможна.');
    return data;
  }

  // === РАБОТА С ШАБЛОНАМИ ===
  let templatesArray = data['Шаблоны зданий'] || [];

  var BUILDING_TEMPLATES = {};
  templatesArray.forEach(function(t) {
    if (t && t.Тип) {
      BUILDING_TEMPLATES[t.Тип] = t;
    }
  });

  // Создание дефолтного шаблона
  if (Object.keys(BUILDING_TEMPLATES).length === 0) {
    if (templatesArray.length === 0 || templatesArray[0] == null || templatesArray[0] === '' || typeof templatesArray[0] !== 'object') {
      var defaultTemplate = {
        Тип: 'Центр управления',
        КритерииПровинции: {}
      };

      BUILDING_TEMPLATES['Центр управления'] = defaultTemplate;  
      templatesArray[0] = defaultTemplate;  
      data['Шаблоны зданий'] = templatesArray;  

      data.Новости.push('Создан дефолтный шаблон "Центр управления" в первой свободной строке');  
    } else {  
      data.Новости.push('Шаблоны зданий уже содержат данные. Дефолтный шаблон не создан.');
    }
  }

  var allProvinces = getAllProvinces(data.Провинции);

  if (allProvinces.length === 0) {
    data.Новости.push('Ошибка: провинции отсутствуют.');
    return data;
  }

  // === ПОДСЧЁТ НАШИХ ПРОВИНЦИЙ ===
  var ourProvincesCount = allProvinces.filter(p => 
    p.Владелец !== undefined && 
    p.Владелец !== null && 
    String(p.Владелец).trim() === stateIdStr
  ).length;

  data.Новости.push(`📍 Всего провинций в данных: ${allProvinces.length}`);
  data.Новости.push(`🏰 Провинций нашего государства: ${ourProvincesCount}`);

  var buildingsInOurProvinces = 0;
  var skipped = 0;

  data.Постройки.forEach(function (item) {

    if (!item || typeof item !== 'object') {  
      skipped++;  
      return;  
    }  

    if (!item.Уровень || item.Уровень < 1) item.Уровень = 1;  

    var provinceName = item.Провинция || '(не указана)';  
    var province = findProvinceForBuilding(allProvinces, item.Провинция);

    // === ПРОВЕРКА: ПРИНАДЛЕЖИТ ЛИ ПРОВИНЦИЯ НАМ? ===
    var isOurProvince = province && 
                        province.Владелец !== undefined && 
                        province.Владелец !== null &&
                        String(province.Владелец).trim() === stateIdStr;

    if (isOurProvince) {
      buildingsInOurProvinces++;
    }

    // Если НЕ наша провинция — полностью игнорируем постройку
    if (!isOurProvince) {
      return;
    }

    // === ОБРАБОТКА ТОЛЬКО НАШИХ ПОСТРОЕК ===
    var template = BUILDING_TEMPLATES[item.Тип];  
    var reasons = [];  

    if (!template) {  
      reasons.push('неизвестный тип постройки "' + item.Тип + '"');  
    }  

    if (!province) {  
      reasons.push('провинция постройки не найдена');  
    }  

    if (item.ПрогрессСтроительства !== undefined && item.ПрогрессСтроительства < 100) {  
      reasons.push('строительство не завершено (' + item.ПрогрессСтроительства + '%)');  
    }  

    if (template && template.КритерииПровинции && province) {  
      var check = checkProvinceCriteria(province, template.КритерииПровинции);  
      if (!check.passes) {  
        reasons = reasons.concat(check.reasons);  
      }  
    }  

    if (template && template.ТребуемыеПостройки && province) {  
      var req = template.ТребуемыеПостройки;  
      var count = data.Постройки.filter(p => p.Провинция === item.Провинция && p.Тип === req.Тип && p.Активно && p !== item).length;  
      if (count < req.Минимум) {  
        reasons.push('требуется минимум ' + req.Минимум + ' активных "' + req.Тип + '" в провинции');  
      }  
    }  

    if (template && template.Лимит) {  
      if (template.Лимит.Провинция !== undefined) {  
        var provCount = data.Постройки.filter(p => p.Провинция === item.Провинция && p.Тип === item.Тип && p.Активно && p !== item).length + 1;  
        if (provCount > template.Лимит.Провинция) {  
          reasons.push('превышен лимит на провинцию (' + template.Лимит.Провинция + ')');  
        }  
      }  

      if (template.Лимит.Государство !== undefined) {  
        var stateCount = data.Постройки.filter(p => {  
          var pProv = findProvinceForBuilding(allProvinces, p.Провинция);  
          return pProv && 
                 pProv.Владелец !== undefined && 
                 pProv.Владелец !== null &&
                 String(pProv.Владелец).trim() === stateIdStr && 
                 p.Тип === item.Тип && 
                 p.Активно && 
                 p !== item;  
        }).length + 1;  
        if (stateCount > template.Лимит.Государство) {  
          reasons.push('превышен лимит на государство (' + template.Лимит.Государство + ')');  
        }  
      }  
    }  

    if (province && province.Лояльность !== undefined && province.Лояльность < 50) {  
      reasons.push('низкая лояльность провинции (' + province.Лояльность + ')');  
    }  

    if (template && template.СрокРаботы > 0) {  
      if (item.ОставшийсяСрок === undefined) item.ОставшийсяСрок = template.СрокРаботы;  
      if (item.ОставшийсяСрок <= 0) {  
        reasons.push('срок работы истёк');  
      } else if (item.Активно) {  
        item.ОставшийсяСрок--;  
        if (item.ОставшийсяСрок <= 0) {  
          reasons.push('срок работы истёк в этом ходу');  
        }  
      }  
    }  

    if (template && template.РискАварии && template.РискАварии[item.Уровень] !== undefined) {  
      var chance = template.РискАварии[item.Уровень];  
      if (Math.random() < chance) {  
        reasons.push('авария на уровне ' + item.Уровень + ' (шанс ' + (chance * 100).toFixed(1) + '%)');  
      }  
    }  

    var level = item.Уровень || 1;  

    if (reasons.length > 0) {  
      item.Активно = false;  
      data.Новости.push('Постройка "' + item.Тип + '" (ур. ' + level + ', провинция "' + provinceName + '") остановлена: ' + reasons.join('; '));  
    } else {  
      item.Активно = true;  
      data.Новости.push('Постройка "' + item.Тип + '" (ур. ' + level + ', провинция "' + provinceName + '") работает');  
    }  

    if (item.Активно && template && template.Синергия) {  
      var syn = template.Синергия;  
      var synCount = data.Постройки.filter(p => p.Провинция === item.Провинция && p.Тип === syn.Тип && p.Активно && p !== item).length;  
      var min = syn.Минимум || 1;  
      if (synCount >= min) {  
        data.Новости.push('Постройка "' + item.Тип + '" получает бонус "' + syn.Бонус + '" благодаря ' + synCount + ' "' + syn.Тип + '"');  
      }  
    }
  });

  data.Новости.push(`🏗️ Построек в провинциях нашего государства: ${buildingsInOurProvinces}`);

  if (skipped > 0) {
    data.Новости.push('Пропущено некорректных записей: ' + skipped);
  }

  return data;
}