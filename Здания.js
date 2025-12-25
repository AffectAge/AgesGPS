/* =========================================================
   УНИВЕРСАЛЬНЫЙ ДВИЖОК КРИТЕРИЕВ ДЛЯ ПРОИЗВОДСТВА
   (Google Apps Script, V8)
   Актуальная версия — декабрь 2025
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
  return provincesData.flat().filter(p => p && typeof p === 'object');
}

function findProvinceForBuilding(allProvinces, provinceKey) {
  return allProvinces.find(p =>
    p.Провинция === provinceKey ||
    p.Название === provinceKey ||
    p.id === provinceKey
  );
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
    Object.keys(rule).some(k =>
      ['>', '<', '>=', '<=', '==', '!=', 'BETWEEN'].includes(k)
    )
  ) {
    return evaluateNumericRule(rule, provinceValue);
  }

  if (typeof rule === 'string') {
    return normalizeToArray(provinceValue).includes(rule);
  }

  if (rule.AND) return rule.AND.every(r => evaluateRule(r, provinceValue));
  if (rule.OR) return rule.OR.some(r => evaluateRule(r, provinceValue));
  if (rule.NOT) return !evaluateRule(rule.NOT, provinceValue);
  if (rule.NAND) return !rule.NAND.every(r => evaluateRule(r, provinceValue));
  if (rule.NOR) return !rule.NOR.some(r => evaluateRule(r, provinceValue));

  if (rule.XOR) {
    return rule.XOR.filter(r => evaluateRule(r, provinceValue)).length === 1;
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
    return `требуется "${rule}", но найдено [${has.join(', ') || 'пусто'}]`;
  }

  if (typeof rule === 'object') {
    if (rule['>'] !== undefined) return `значение ${value} должно быть > ${rule['>']}`;
    if (rule['<'] !== undefined) return `значение ${value} должно быть < ${rule['<']}`;
    if (rule['>='] !== undefined) return `значение ${value} должно быть ≥ ${rule['>=']}`;
    if (rule['<='] !== undefined) return `значение ${value} должно быть ≤ ${rule['<=']}`;
    if (rule['=='] !== undefined) return `значение ${value} должно быть = ${rule['==']}`;
    if (rule['!='] !== undefined) return `значение ${value} не должно быть = ${rule['!=']}`;

    if (rule.BETWEEN) {
      return `значение ${value} должно быть между ${rule.BETWEEN[0]} и ${rule.BETWEEN[1]}`;
    }

    if (rule.AND) {
      return rule.AND
        .filter(r => !evaluateRule(r, provinceValue))
        .map(r => explainRule(r, provinceValue))
        .join('; ');
    }

    if (rule.OR) {
      return 'не выполнено ни одно из условий OR';
    }

    if (rule.NOT) {
      return 'условие NOT не должно выполняться';
    }
  }

  return 'неизвестное правило';
}

/* =======================
   ПРОВЕРКА КРИТЕРИЕВ ПРОВИНЦИИ
   ======================= */

function checkProvinceCriteria(province, criteria) {
  if (!criteria) return { passes: true, reasons: [] };

  var reasons = [];

  for (var key in criteria) {
    var rule = criteria[key];
    var value = getValueByPath(province, key);

    if (!evaluateRule(rule, value)) {
      reasons.push(`Параметр "${key}": ${explainRule(rule, value)}`);
    }
  }

  return { passes: reasons.length === 0, reasons };
}

/* =======================
   ОСНОВНАЯ ФУНКЦИЯ
   ======================= */

function processCriteriaCheck(data) {

  if (!data.Новости) data.Новости = [];
  if (!Array.isArray(data.Постройки)) {
    data.Новости.push('Ошибка: отсутствует список построек.');
    return data;
  }

  /* === ИДЕНТИФИКАТОР ГОСУДАРСТВА === */
  var stateId;
  if (Array.isArray(data['Идентификатор данных государства']) &&
      Array.isArray(data['Данные государства'])) {

    var k = data['Идентификатор данных государства'];
    var v = data['Данные государства'];
    var idx = k.indexOf('Идентификатор государства');

    if (idx !== -1 && v[idx] !== undefined && v[idx] !== null) {
      stateId = String(v[idx]).trim();
    }
  }

  if (!stateId) {
    data.Новости.push('Идентификатор государства не найден.');
    return data;
  }

  /* === ШАБЛОНЫ === */
  var BUILDING_TEMPLATES = {};
  (data['Шаблоны зданий'] || []).forEach(t => {
    if (t?.Тип) BUILDING_TEMPLATES[t.Тип] = t;
  });

  /* === ПРОВИНЦИИ === */
  var allProvinces = getAllProvinces(data.Провинции);
  var ourProvinces = allProvinces.filter(p => String(p.Владелец).trim() === stateId);

  data.Новости.push(`Всего провинций: ${allProvinces.length}`);
  data.Новости.push(`Провинций нашего государства: ${ourProvinces.length}`);

  var buildings = data.Постройки.filter(p => p && typeof p === 'object');

  /* =======================
     ПЕРВЫЙ ПРОХОД
     ======================= */

  buildings.forEach(item => {

    if (!item.Провинция || !item.Тип) return;
    if (!item.Уровень || item.Уровень < 1) item.Уровень = 1;

    var province = findProvinceForBuilding(allProvinces, item.Провинция);
    if (!province) return;

    var template = BUILDING_TEMPLATES[item.Тип];
    var reasons = [];

    if (!template) {
      reasons.push(`Неизвестный тип постройки "${item.Тип}"`);
    }

    if (template?.КритерииПровинции) {
      var check = checkProvinceCriteria(province, template.КритерииПровинции);
      if (!check.passes) reasons.push(...check.reasons);
    }

    /* === ТРЕБУЕМЫЕ ПОСТРОЙКИ === */
    if (template?.ТребуемыеПостройки) {
      var req = template.ТребуемыеПостройки;
      var count = buildings.filter(p =>
        p.Провинция === item.Провинция &&
        p.Тип === req.Тип &&
        p !== item
      ).length;

      if (count < req.Минимум) {
        reasons.push(
          `Требуется минимум ${req.Минимум} построек типа "${req.Тип}" ` +
          `в провинции, но найдено ${count}.`
        );
      }
    }

    /* === ЛИМИТЫ === */
    if (template?.Лимит) {

      /* Провинция */
      if (template.Лимит.Провинция !== undefined) {
        var provCount = buildings.filter(p =>
          p.Провинция === item.Провинция &&
          p.Тип === item.Тип
        ).length;

        if (provCount > template.Лимит.Провинция) {
          reasons.push(`Превышен лимит на провинцию для "${item.Тип}"`);
        } else if (provCount === template.Лимит.Провинция) {
          data.Новости.push(
            `Достигнут лимит на провинцию: "${item.Тип}" в ${item.Провинция}`
          );
        }
      }

      /* Государство */
      if (template.Лимит.Государство !== undefined) {
        var stateCount = buildings.filter(p => {
          var pp = findProvinceForBuilding(allProvinces, p.Провинция);
          return pp &&
            String(pp.Владелец).trim() === stateId &&
            p.Тип === item.Тип;
        }).length;

        if (stateCount > template.Лимит.Государство) {
          reasons.push(`Превышен лимит на государство для "${item.Тип}"`);
        } else if (stateCount === template.Лимит.Государство) {
          data.Новости.push(
            `Достигнут лимит на государство: "${item.Тип}"`
          );
        }
      }
    }

    item._failReasons = reasons;
    item._canWork = reasons.length === 0;
  });

  /* =======================
     ВТОРОЙ ПРОХОД — ЛИМИТ МИРА
     ======================= */

  var byType = {};
  buildings.forEach(b => {
    if (b._canWork) {
      if (!byType[b.Тип]) byType[b.Тип] = [];
      byType[b.Тип].push(b);
    }
  });

  for (var type in byType) {
    var template = BUILDING_TEMPLATES[type];
    if (!template?.Лимит?.Мир) continue;

    var list = byType[type];
    var limit = template.Лимит.Мир;

    if (list.length === limit) {
      data.Новости.push(
        `Достигнут глобальный лимит на мир для "${type}"`
      );
    }

    list.forEach((b, i) => {
      if (i < limit) {
        b.Активно = true;
      } else {
        b.Активно = false;
        b._failReasons.push('Превышен глобальный лимит на мир');
      }
    });
  }

  /* =======================
     ТРЕТИЙ ПРОХОД — ИТОГ
     ======================= */

  buildings.forEach(item => {
    if (item._failReasons?.length) {
      item.Активно = false;
      data.Новости.push(
        `"${item.Тип}" в ${item.Провинция} остановлена: ${item._failReasons.join('; ')}`
      );
    } else if (item.Активно !== false) {
      item.Активно = true;
      data.Новости.push(
        `"${item.Тип}" в ${item.Провинция} работает`
      );
    }

    delete item._failReasons;
    delete item._canWork;
  });

  return data;
}