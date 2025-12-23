/* =========================================================
   УНИВЕРСАЛЬНЫЙ ДВИЖОК КРИТЕРИЕВ + ПРОИЗВОДСТВО
   Архитектура Victoria-подобной стратегии
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
  return path.split('.').reduce((o, k) => o?.[k], obj);
}

/* =======================
   ЧИСЛОВЫЕ ОПЕРАТОРЫ
   ======================= */

function evaluateNumericRule(rule, value) {
  if (typeof value !== "number") return false;

  if (rule[">"] !== undefined) return value > rule[">"];
  if (rule["<"] !== undefined) return value < rule["<"];
  if (rule[">="] !== undefined) return value >= rule[">="];
  if (rule["<="] !== undefined) return value <= rule["<="];
  if (rule["=="] !== undefined) return value === rule["=="];
  if (rule["!="] !== undefined) return value !== rule["!="];

  if (rule.BETWEEN) {
    const [min, max] = rule.BETWEEN;
    return value >= min && value <= max;
  }

  return false;
}

/* =======================
   ЛОГИЧЕСКИЙ ИНТЕРПРЕТАТОР
   ======================= */

function evaluateRule(rule, provinceValue) {

  // числовые условия
  if (
    typeof rule === "object" &&
    !Array.isArray(rule) &&
    Object.keys(rule).some(k =>
      [">", "<", ">=", "<=", "==", "!=", "BETWEEN"].includes(k)
    )
  ) {
    return evaluateNumericRule(rule, provinceValue);
  }

  // строковое значение
  if (typeof rule === "string") {
    return normalizeToArray(provinceValue).includes(rule);
  }

  if (rule.AND) {
    return rule.AND.every(r => evaluateRule(r, provinceValue));
  }

  if (rule.OR) {
    return rule.OR.some(r => evaluateRule(r, provinceValue));
  }

  if (rule.NOT) {
    return !evaluateRule(rule.NOT, provinceValue);
  }

  if (rule.NAND) {
    return !rule.NAND.every(r => evaluateRule(r, provinceValue));
  }

  if (rule.NOR) {
    return !rule.NOR.some(r => evaluateRule(r, provinceValue));
  }

  if (rule.XOR) {
    let count = 0;
    for (const r of rule.XOR) {
      if (evaluateRule(r, provinceValue)) count++;
    }
    return count === 1;
  }

  return false;
}

/* =======================
   ПРОВЕРКА КРИТЕРИЕВ
   ======================= */

function checkProvinceCriteria(province, criteria) {
  if (!criteria) return true;

  for (const key in criteria) {
    const rule = criteria[key];
    const value = getValueByPath(province, key);

    if (!evaluateRule(rule, value)) {
      return false;
    }
  }

  return true;
}

/* =======================
   ПРОИЗВОДСТВО (ХОД)
   ======================= */

function processProduction(data) {

  for (const building of data.Постройки) {
    if (!building.Активно) continue;

    const template = BUILDING_TEMPLATES[building.Тип];
    if (!template) continue;

    const province = data.Провинции[building.Провинция][0];
    const resources = province.Ресурсы[0];

    // критерии
    if (!checkProvinceCriteria(province, template.КритерииПровинции)) {
      building.Активно = false;
      data.Новости.push(`${province.Провинция}: ${building.Тип} остановлена (критерии)`);
      continue;
    }

    // эффективность
    const eff = calculateEfficiency(province, template.Эффективность);

    // входные ресурсы
    for (const r in template.Вход) {
      const need = template.Вход[r] * building.Уровень * eff;
      if ((resources[r] || 0) < need) {
        building.Активно = false;
        data.Новости.push(`${province.Провинция}: ${building.Тип} остановлена (ресурсы)`);
        continue;
      }
    }

    // списание входов
    for (const r in template.Вход) {
      resources[r] -= template.Вход[r] * building.Уровень * eff;
    }

    // производство
    for (const p in template.Выход) {
      if (!resources[p]) resources[p] = 0;
      resources[p] += template.Выход[p] * building.Уровень * eff;
    }

    data.Новости.push(
      `${province.Провинция}: ${building.Тип} произвела ресурсы (эфф. ${eff.toFixed(2)})`
    );
  }
}