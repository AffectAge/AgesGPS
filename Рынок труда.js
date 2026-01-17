/* =========================================================
   РЫНОК ТРУДА (БАЗОВЫЙ) — ГОТОВАЯ ВЕРСИЯ (ТВОЙ ФОРМАТ "ЯЧЕЙКИ")
   Google Apps Script
)
   ========================================================= */


/* =======================
   ВСПОМОГАТЕЛЬНЫЕ
   ======================= */
   
   function getPopQuantity_(pop) {
  if (!pop || typeof pop !== "object") return 0;

  var base = pop["Основные данные"];

  // ожидаемый формат: массив объектов
  if (Array.isArray(base)) {
    for (var i = 0; i < base.length; i++) {
      var row = base[i];
      if (row && typeof row === "object" && typeof row.Количество === "number") {
        return Math.max(0, Math.floor(row.Количество));
      }
    }
    return 0;
  }

  // если внезапно пришло не массивом — считаем отсутствующим
  return 0;
}

function normalizeToArray(value) {
  if (Array.isArray(value)) return value;
  if (value === null || value === undefined) return [];
  return [value];
}

function ensureNews(data) {
  if (!Array.isArray(data.Новости)) data.Новости = [];
}

function clamp01(x) {
  x = Number(x);
  if (isNaN(x)) return 0;
  return Math.min(1, Math.max(0, x));
}

function ensure2DArrayField(data, fieldName) {
  if (!Array.isArray(data[fieldName])) data[fieldName] = [];
  for (var i = 0; i < data[fieldName].length; i++) {
    if (!Array.isArray(data[fieldName][i])) data[fieldName][i] = [];
  }
  if (data[fieldName].length === 0) data[fieldName] = [[]];
}


/* =======================
   ТРУД: рабочие места по уровню + шаблоны
   ======================= */

function computeRequiredWorkersByLevel(base, level, growthCoef) {
  base = Number(base) || 0;
  level = Math.max(1, Math.floor(Number(level) || 1));
  growthCoef = Number(growthCoef);
  if (isNaN(growthCoef)) growthCoef = 0.8;

  var slots = base * (1 + growthCoef * (level - 1));
  return Math.max(0, Math.floor(slots));
}

function buildTemplatesMap(data) {
  var map = {};
  normalizeToArray(data["Шаблоны зданий"]).forEach(function (row) {
    normalizeToArray(row).forEach(function (t) {
      if (t && t.Тип) map[t.Тип] = t;
    });
  });
  return map;
}

function getBuildingWorkSlots(data, templatesMap, building) {
  if (!building || typeof building !== "object") return 0;

  // override: в здании явно задано число
  if (typeof building["Рабочие места"] === "number") {
    return Math.max(0, Math.floor(building["Рабочие места"]));
  }

  // по шаблону
  var tpl = templatesMap && building.Тип ? templatesMap[building.Тип] : null;
  if (!tpl || !tpl.Труд) return 0;

  var base = tpl.Труд.База;
  var k = tpl.Труд.ПриростЗаУровень;

  var lvl = (building.Уровень !== undefined)
    ? building.Уровень
    : (building["Ур."] !== undefined ? building["Ур."] : 1);

  return computeRequiredWorkersByLevel(base, lvl, k);
}


/* =======================
   ГОСУДАРСТВО: читаем из "ячейки" data["Данные государства"]
   ======================= */

function getStateParamFromCell(data, key) {
  var root = data ? data["Данные государства"] : null;
  if (root === null || root === undefined) return undefined;

  var flat = [];
  normalizeToArray(root).forEach(function (row) {
    normalizeToArray(row).forEach(function (cell) {
      flat.push(cell);
    });
  });

  for (var i = 0; i < flat.length; i++) {
    var obj = flat[i];
    if (obj && typeof obj === "object" && !Array.isArray(obj)) {
      if (Object.prototype.hasOwnProperty.call(obj, key)) return obj[key];
    }
  }

  return undefined;
}

function getStateIdSafe(data) {
  ensureNews(data);

  var v = getStateParamFromCell(data, "Идентификатор государства");
  if (v === undefined || v === null || String(v).trim() === "") {
    pushNotice(data, {
      category: "Рынок труда",
      sub: "Ошибка",
      priority: 999,
      parts: [
        { text: "Ошибка данных государства\n", bold: true, color: "#E36A6A" },
        { text: "┌────────────────────────────────────────────────────────┐\n", color: "#E36A6A" },

        { text: "┃", bold: true, color: "#E36A6A" },
        { text: " ➔ Причина: ", bold: true, color: "#CFC7BA" },
        { text: "не найден параметр \"Идентификатор государства\"\n", bold: true, color: "#E6E6FA" },

        { text: "┃", bold: true, color: "#E36A6A" },
        { text: " ➔ Источник: ", bold: true, color: "#CFC7BA" },
        { text: "data[\"Данные государства\"] (ячейка JSON)\n", bold: true, color: "#E6E6FA" },

        { text: "┃", bold: true, color: "#E36A6A" },
        { text: " ➔ Что сделать: ", bold: true, color: "#CFC7BA" },
        { text: "добавь {\"Идентификатор государства\": <id>} в \"Данные государства\"\n", bold: true, color: "#E6E6FA" },

        { text: "└────────────────────────────────────────────────────────┘\n", color: "#E36A6A" }
      ]
    });
    return null;
  }

  return String(v).trim();
}

function getWorkforceCoefficientSafe(data) {
  ensureNews(data);

  var v = getStateParamFromCell(data, "Коэффициент рабочей силы");
  if (v === undefined || v === null || v === "") {
    pushNotice(data, {
      category: "Рынок труда",
      sub: "Предупреждение",
      priority: 400,
      parts: [
        { text: "Параметр не найден\n", bold: true, color: "#FF8C00" },
        { text: "┌────────────────────────────────────────────────────────┐\n", color: "#FF8C00" },

        { text: "┃", bold: true, color: "#FF8C00" },
        { text: " ➔ Параметр: ", bold: true, color: "#CFC7BA" },
        { text: "Коэффициент рабочей силы\n", bold: true, color: "#E6E6FA" },

        { text: "┃", bold: true, color: "#FF8C00" },
        { text: " ➔ Источник: ", bold: true, color: "#CFC7BA" },
        { text: "data[\"Данные государства\"]\n", bold: true, color: "#E6E6FA" },

        { text: "┃", bold: true, color: "#FF8C00" },
        { text: " ➔ Принято: ", bold: true, color: "#CFC7BA" },
        { text: "0\n", bold: true, color: "#E36A6A" },

        { text: "└────────────────────────────────────────────────────────┘\n", color: "#FF8C00" }
      ]
    });
    return 0;
  }

  var num = Number(v);
  if (isNaN(num)) {
    pushNotice(data, {
      category: "Рынок труда",
      sub: "Предупреждение",
      priority: 400,
      parts: [
        { text: "Некорректный параметр\n", bold: true, color: "#FF8C00" },
        { text: "┌────────────────────────────────────────────────────────┐\n", color: "#FF8C00" },

        { text: "┃", bold: true, color: "#FF8C00" },
        { text: " ➔ Параметр: ", bold: true, color: "#CFC7BA" },
        { text: "Коэффициент рабочей силы\n", bold: true, color: "#E6E6FA" },

        { text: "┃", bold: true, color: "#FF8C00" },
        { text: " ➔ Значение: ", bold: true, color: "#CFC7BA" },
        { text: String(v) + "\n", bold: true, color: "#E36A6A" },

        { text: "┃", bold: true, color: "#FF8C00" },
        { text: " ➔ Принято: ", bold: true, color: "#CFC7BA" },
        { text: "0\n", bold: true, color: "#E36A6A" },

        { text: "└────────────────────────────────────────────────────────┘\n", color: "#FF8C00" }
      ]
    });
    return 0;
  }

  var clamped = clamp01(num);
  if (clamped !== num) {
    pushNotice(data, {
      category: "Рынок труда",
      sub: "Предупреждение",
      priority: 300,
      parts: [
        { text: "Параметр обрезан до диапазона\n", bold: true, color: "#FF8C00" },
        { text: "┌────────────────────────────────────────────────────────┐\n", color: "#FF8C00" },

        { text: "┃", bold: true, color: "#FF8C00" },
        { text: " ➔ Параметр: ", bold: true, color: "#CFC7BA" },
        { text: "Коэффициент рабочей силы\n", bold: true, color: "#E6E6FA" },

        { text: "┃", bold: true, color: "#FF8C00" },
        { text: " ➔ Было: ", bold: true, color: "#CFC7BA" },
        { text: String(num) + "\n", bold: true, color: "#E36A6A" },

        { text: "┃", bold: true, color: "#FF8C00" },
        { text: " ➔ Стало: ", bold: true, color: "#CFC7BA" },
        { text: String(clamped) + "\n", bold: true, color: "#E6E6FA" },

        { text: "└────────────────────────────────────────────────────────┘\n", color: "#FF8C00" }
      ]
    });
  }

  return clamped;
}


/* =======================
   ПРОВИНЦИИ: только наши
   ======================= */

function getAllProvincesFlat(data) {
  if (!Array.isArray(data.Провинции)) return [];
  return normalizeToArray(data.Провинции)
    .reduce(function (acc, row) { return acc.concat(normalizeToArray(row)); }, [])
    .filter(function (p) { return p && typeof p === "object" && p.Провинция; });
}

function buildOurProvincesMap(data, stateId) {
  var map = {};
  var sid = String(stateId || "");
  getAllProvincesFlat(data).forEach(function (p) {
    if (String(p.Владелец || "") === sid) {
      map[p.Провинция] = true;
    }
  });
  return map;
}


/* =======================
   НАСЕЛЕНИЕ / POP
   ======================= */
function calculatePopulationTotal(data, provinceName) {
  if (!Array.isArray(data.Население)) return 0;

  var total = 0;
  var rows = normalizeToArray(data.Население);

  for (var i = 0; i < rows.length; i++) {
    var row = normalizeToArray(rows[i]);
    for (var j = 0; j < row.length; j++) {
      var pop = row[j];
      if (!pop || typeof pop !== "object") continue;
      if (pop.Провинция !== provinceName) continue;

      total += getPopQuantity_(pop);
    }
  }

  return Math.max(0, Math.floor(total));
}

function calculateWorkforceFromPopulation(data, provinceName, workforceCoef) {
  var popTotal = calculatePopulationTotal(data, provinceName);
  if (!workforceCoef || workforceCoef <= 0) return 0;
  return Math.max(0, Math.floor(popTotal * workforceCoef));
}


/* =======================
   ПОСТРОЙКИ: спрос (только активные)
   ======================= */

function calculateLaborDemand(data, provinceName, templatesMap) {
  if (!Array.isArray(data.Постройки)) return 0;

  var demand = 0;
  var rows = normalizeToArray(data.Постройки);

  for (var i = 0; i < rows.length; i++) {
    var row = normalizeToArray(rows[i]);
    for (var j = 0; j < row.length; j++) {
      var b = row[j];
      if (!b || typeof b !== "object") continue;
      if (b.Провинция !== provinceName) continue;
      if (b.Активно !== true) continue;

      var slots = getBuildingWorkSlots(data, templatesMap, b);
      if (slots <= 0) continue;

      demand += slots;
    }
  }

  return Math.max(0, Math.floor(demand));
}


/* =======================
   РЫНОК ТРУДА: хранение
   ======================= */

function normalizeLaborMarket(data) {
  ensure2DArrayField(data, "Рынок труда");
}

function getLaborMarketByProvince(data, provinceName) {
  if (!Array.isArray(data["Рынок труда"])) return null;
  var flat = data["Рынок труда"].reduce(function (acc, row) {
    return acc.concat(normalizeToArray(row));
  }, []);
  for (var i = 0; i < flat.length; i++) {
    var r = flat[i];
    if (r && r.Провинция === provinceName) return r;
  }
  return null;
}

function upsertLaborMarketEntry(data, provinceName, population, workforce, demand, occupancyShareNullable) {
  normalizeLaborMarket(data);

  var entry = getLaborMarketByProvince(data, provinceName);

  if (!entry) {
    entry = {
      Провинция: provinceName,
      "Население": 0,
      "Рабочая сила": 0,
      "Спрос": 0,
      "Доля занятости": 0,
      "Занятые": 0,
      "Безработные": 0,
      "Безработица": 0,
      "Дефицит": 0,
      "Дефицит %": 0
    };
    data["Рынок труда"][0].push(entry);
  }

  entry["Население"] = population;
  entry["Рабочая сила"] = workforce;
  entry["Спрос"] = demand;

  // Доля занятости (доля заполнения рабочих мест)
  var occ = null;
  if (occupancyShareNullable !== null && occupancyShareNullable !== undefined) {
    occ = clamp01(occupancyShareNullable);
  } else {
    occ = (demand > 0) ? clamp01(workforce / demand) : 0;
  }
  entry["Доля занятости"] = occ;

  // Безработица (по рабочей силе)
  var employed = Math.min(workforce, demand);
  var unemployed = Math.max(0, workforce - employed);
  var unempRate = workforce > 0 ? unemployed / workforce : 0;

  entry["Занятые"] = employed;
  entry["Безработные"] = unemployed;
  entry["Безработица"] = clamp01(unempRate);

  // Дефицит (по спросу)
  var deficit = Math.max(0, demand - workforce);
  var deficitRate = demand > 0 ? deficit / demand : 0;

  entry["Дефицит"] = deficit;
  entry["Дефицит %"] = clamp01(deficitRate);

  return entry;
}


/* =======================
   ПЕРЕСБОРКА РЫНКА ТРУДА: только наши + новости (новый стиль)
   ======================= */

function rebuildLaborMarketOurOnly(data) {
  ensureNews(data);
  normalizeLaborMarket(data);

  data["Рынок труда"] = [[]];

  var templatesMap = buildTemplatesMap(data);

  var stateId = getStateIdSafe(data);
  if (!stateId) return { ok: false, stateId: null, ourCount: 0 };

  var coef = getWorkforceCoefficientSafe(data);

  var ourMap = buildOurProvincesMap(data, stateId);
  var provinces = Object.keys(ourMap);

  var totalPop = 0;
  var totalWorkforce = 0;
  var totalDemand = 0;

  if (provinces.length === 0) {
    pushNotice(data, {
      category: "Рынок труда",
      sub: "Предупреждение",
      priority: 500,
      parts: [
        { text: "Провинции не найдены\n", bold: true, color: "#FF8C00" },
        { text: "┌────────────────────────────────────────────────────────┐\n", color: "#FF8C00" },

        { text: "┃", bold: true, color: "#FF8C00" },
        { text: " ➔ Государство: ", bold: true, color: "#CFC7BA" },
        { text: String(stateId) + "\n", bold: true, color: "#E6E6FA" },

        { text: "┃", bold: true, color: "#FF8C00" },
        { text: " ➔ Причина: ", bold: true, color: "#CFC7BA" },
        { text: "нет провинций (или не заполнен \"Владелец\")\n", bold: true, color: "#E36A6A" },

        { text: "└────────────────────────────────────────────────────────┘\n", color: "#FF8C00" }
      ]
    });
    return { ok: true, stateId: stateId, ourCount: 0 };
  }

  for (var i = 0; i < provinces.length; i++) {
    var provName = provinces[i];

    var popTotal = calculatePopulationTotal(data, provName);
    var workforce = calculateWorkforceFromPopulation(data, provName, coef);
    var demand = calculateLaborDemand(data, provName, templatesMap);

    totalPop += popTotal;
    totalWorkforce += workforce;
    totalDemand += demand;

    var entry = upsertLaborMarketEntry(data, provName, popTotal, workforce, demand, null);

    // ===== НОВОСТЬ ПО ПРОВИНЦИИ (рамка) =====
    var provParts = [
      { text: "Трудовые ресурсы провинции\n", bold: true, color: "#FF8C00" },
      { text: "┌────────────────────────────────────────────────────────┐\n", color: "#FF8C00" },

      { text: "┃", bold: true, color: "#FF8C00" },
      { text: " ➔ Провинция: ", bold: true, color: "#CFC7BA" },
      { text: String(provName) + "\n", bold: true, color: "#E6E6FA" },

      { text: "┃", bold: true, color: "#FF8C00" },
      { text: " ➔ Общее население: ", bold: true, color: "#CFC7BA" },
      { text: String(entry["Население"]) + "\n", bold: true, color: "#E6E6FA" },

      { text: "┃", bold: true, color: "#FF8C00" },
      { text: " ➔ Всего рабочих: ", bold: true, color: "#CFC7BA" },
      { text: String(entry["Рабочая сила"]) + "\n", bold: true, color: "#E6E6FA" },

      { text: "┃", bold: true, color: "#FF8C00" },
      { text: " ➔ Необходимо рабочих: ", bold: true, color: "#CFC7BA" },
      { text: String(entry["Спрос"]) + "\n", bold: true, color: "#E6E6FA" },

      { text: "┃", bold: true, color: "#FF8C00" },
      { text: " ➔ Занято рабочих: ", bold: true, color: "#CFC7BA" },
      { text: String(entry["Занятые"]) + "\n", bold: true, color: "#E6E6FA" },

      { text: "┃", bold: true, color: "#FF8C00" },
      { text: " ➔ Безработные: ", bold: true, color: "#CFC7BA" },
      { text: String(entry["Безработные"]) + "\n", bold: true, color: "#E36A6A" },

      { text: "┃", bold: true, color: "#FF8C00" },
      { text: " ➔ Уровень безработицы: ", bold: true, color: "#CFC7BA" },
      { text: String(Math.round(entry["Безработица"] * 1000) / 10) + "%\n", bold: true, color: "#E36A6A" }
    ];

    if (entry["Дефицит"] > 0) {
      provParts.push(
        { text: "┃", bold: true, color: "#FF8C00" },
        { text: " ➔ Дефицит рабочих: ", bold: true, color: "#CFC7BA" },
        { text: String(entry["Дефицит"]) + "\n", bold: true, color: "#E36A6A" },

        { text: "┃", bold: true, color: "#FF8C00" },
        { text: " ➔ Уровень дефицита: ", bold: true, color: "#CFC7BA" },
        { text: String(Math.round(entry["Дефицит %"] * 1000) / 10) + "%\n", bold: true, color: "#E36A6A" }
      );
    } else {
      provParts.push(
        { text: "┃", bold: true, color: "#FF8C00" },
        { text: " ➔ Дефицит рабочих: ", bold: true, color: "#CFC7BA" },
        { text: "0\n", bold: true, color: "#E6E6FA" }
      );
    }

    provParts.push({ text: "└────────────────────────────────────────────────────────┘\n", color: "#FF8C00" });

    pushNotice(data, {
      category: "Рынок труда",
      sub: "Провинция",
      priority: 80,
      parts: provParts
    });
  }

  // ===== ИТОГИ (один раз) =====
  var employedTotal = Math.min(totalWorkforce, totalDemand);
  var unemployedTotal = Math.max(0, totalWorkforce - employedTotal);
  var unempTotalRate = totalWorkforce > 0 ? unemployedTotal / totalWorkforce : 0;

  var totalDeficit = Math.max(0, totalDemand - totalWorkforce);
  var totalDeficitRate = totalDemand > 0 ? totalDeficit / totalDemand : 0;

  pushNotice(data, {
    category: "Рынок труда",
    sub: "Статистика",
    priority: 100,
    parts: [
      { text: "Общая статистика трудовых ресурсов государства\n", bold: true, color: "#FF8C00" },
      { text: "┌────────────────────────────────────────────────────────┐\n", color: "#FF8C00" },

      { text: "┃", bold: true, color: "#FF8C00" },
      { text: " ➔ Количество провинций: ", bold: true, color: "#CFC7BA" },
      { text: String(provinces.length) + "\n", bold: true, color: "#E6E6FA" },

      { text: "┃", bold: true, color: "#FF8C00" },
      { text: " ➔ Население государства: ", bold: true, color: "#CFC7BA" },
      { text: String(totalPop) + "\n", bold: true, color: "#E6E6FA" },

      { text: "┃", bold: true, color: "#FF8C00" },
      { text: " ➔ Общее количество рабочих: ", bold: true, color: "#CFC7BA" },
      { text: String(totalWorkforce) + "\n", bold: true, color: "#E6E6FA" },

      { text: "┃", bold: true, color: "#FF8C00" },
      { text: " ➔ Всего занятых рабочих: ", bold: true, color: "#CFC7BA" },
      { text: String(employedTotal) + "\n", bold: true, color: "#E6E6FA" },

      { text: "┃", bold: true, color: "#FF8C00" },
      { text: " ➔ Всего необходимо рабочих: ", bold: true, color: "#CFC7BA" },
      { text: String(totalDemand) + "\n", bold: true, color: "#E6E6FA" },

      { text: "┃", bold: true, color: "#FF8C00" },
      { text: " ➔ Всего свободных рабочих: ", bold: true, color: "#CFC7BA" },
      { text: String(unemployedTotal) + "\n", bold: true, color: "#E6E6FA" },

      { text: "┃", bold: true, color: "#FF8C00" },
      { text: " ➔ Уровень безработицы: ", bold: true, color: "#CFC7BA" },
      { text: String(Math.round(unempTotalRate * 1000) / 10) + "%\n", bold: true, color: "#E36A6A" },

      { text: "┃", bold: true, color: "#FF8C00" },
      { text: " ➔ Дефицит рабочих: ", bold: true, color: "#CFC7BA" },
      { text: String(totalDeficit) + "\n", bold: true, color: "#E36A6A" },

      { text: "┃", bold: true, color: "#FF8C00" },
      { text: " ➔ Уровень дефицита рабочих: ", bold: true, color: "#CFC7BA" },
      { text: String(Math.round(totalDeficitRate * 1000) / 10) + "%\n", bold: true, color: "#E36A6A" },

      { text: "└────────────────────────────────────────────────────────┘\n", color: "#FF8C00" }
    ]
  });

  return { ok: true, stateId: stateId, ourCount: provinces.length };
}


/* =======================
   ЗДАНИЯ: как "понимают" рабочих (без профессий)
   ======================= */

function getBuildingStaffingSimple(building, laborEntry) {
  var slots = (building && typeof building["Рабочие места"] === "number") ? building["Рабочие места"] : 0;
  if (!laborEntry || slots <= 0) {
    return { Рабочие: 0, Эффективность: 0 };
  }

  var share = clamp01(laborEntry["Доля занятости"]);
  var workers = Math.floor(slots * share);
  var eff = slots > 0 ? workers / slots : 0;

  return { Рабочие: workers, Эффективность: eff };
}

function applyLaborEffectToBuildingsOurOnly(data) {
  ensureNews(data);

  var stateId = getStateIdSafe(data);
  if (!stateId) return;

  var templatesMap = buildTemplatesMap(data);
  var ourMap = buildOurProvincesMap(data, stateId);

  if (!Array.isArray(data.Постройки)) {
    pushNotice(data, {
      category: "Рынок труда",
      sub: "Предупреждение",
      priority: 600,
      parts: [
        { text: "Постройки отсутствуют\n", bold: true, color: "#FF8C00" },
        { text: "┌────────────────────────────────────────────────────────┐\n", color: "#FF8C00" },

        { text: "┃", bold: true, color: "#FF8C00" },
        { text: " ➔ Причина: ", bold: true, color: "#CFC7BA" },
        { text: "data.Постройки отсутствует или не массив\n", bold: true, color: "#E36A6A" },

        { text: "┃", bold: true, color: "#FF8C00" },
        { text: " ➔ Эффект: ", bold: true, color: "#CFC7BA" },
        { text: "здания не обработаны\n", bold: true, color: "#E6E6FA" },

        { text: "└────────────────────────────────────────────────────────┘\n", color: "#FF8C00" }
      ]
    });
    return;
  }

  var rows = normalizeToArray(data.Постройки);
  var affected = 0;
  var turnedOff = 0;

  for (var i = 0; i < rows.length; i++) {
    var row = normalizeToArray(rows[i]);
    for (var j = 0; j < row.length; j++) {
      var b = row[j];
      if (!b || typeof b !== "object") continue;
      if (!b.Провинция) continue;
      if (!ourMap[b.Провинция]) continue;

      // Активно=false не потребляет труд
      if (b.Активно === false) {
        b._РабочиеМеста = 0;
        b._Рабочие = 0;
        b._ЭффективностьТруда = 0;

        affected++;

        pushNotice(data, {
          category: "Рынок труда",
          sub: "Здание",
          priority: 50,
          parts: [
            { text: "Здание уже остановлено\n", bold: true, color: "#FF8C00" },
            { text: "┌────────────────────────────────────────────────────────┐\n", color: "#FF8C00" },

            { text: "┃", bold: true, color: "#FF8C00" },
            { text: " ➔ Здание: ", bold: true, color: "#CFC7BA" },
            { text: String(b.Тип || "Здание") + "\n", bold: true, color: "#E6E6FA" },

            { text: "┃", bold: true, color: "#FF8C00" },
            { text: " ➔ Провинция: ", bold: true, color: "#CFC7BA" },
            { text: String(b.Провинция) + "\n", bold: true, color: "#E6E6FA" },

            { text: "┃", bold: true, color: "#FF8C00" },
            { text: " ➔ Причина: ", bold: true, color: "#CFC7BA" },
            { text: "здание остановлено до обработки рынка труда\n", bold: true, color: "#E36A6A" },

            { text: "┃", bold: true, color: "#FF8C00" },
            { text: " ➔ Эффект: ", bold: true, color: "#CFC7BA" },
            { text: "рабочие = 0, эффективность = 0%\n", bold: true, color: "#E6E6FA" },

            { text: "└────────────────────────────────────────────────────────┘\n", color: "#FF8C00" }
          ]
        });

        continue;
      }

      var labor = getLaborMarketByProvince(data, b.Провинция);
      var slots = getBuildingWorkSlots(data, templatesMap, b);

      if (slots <= 0) {
        b._РабочиеМеста = 0;
        b._Рабочие = 0;
        b._ЭффективностьТруда = 0;

        affected++;

        pushNotice(data, {
          category: "Рынок труда",
          sub: "Здание",
          priority: 200,
          parts: [
            { text: "Рабочие места не определены\n", bold: true, color: "#FF8C00" },
            { text: "┌────────────────────────────────────────────────────────┐\n", color: "#FF8C00" },

            { text: "┃", bold: true, color: "#FF8C00" },
            { text: " ➔ Здание: ", bold: true, color: "#CFC7BA" },
            { text: String(b.Тип || "Здание") + "\n", bold: true, color: "#E6E6FA" },

            { text: "┃", bold: true, color: "#FF8C00" },
            { text: " ➔ Провинция: ", bold: true, color: "#CFC7BA" },
            { text: String(b.Провинция) + "\n", bold: true, color: "#E6E6FA" },

            { text: "┃", bold: true, color: "#FF8C00" },
            { text: " ➔ Причина: ", bold: true, color: "#CFC7BA" },
            { text: "нет \"Рабочие места\" и/или отсутствует tpl.Труд\n", bold: true, color: "#E36A6A" },

            { text: "┃", bold: true, color: "#FF8C00" },
            { text: " ➔ Эффект: ", bold: true, color: "#CFC7BA" },
            { text: "рабочие места = 0, расчёт пропущен\n", bold: true, color: "#E6E6FA" },

            { text: "└────────────────────────────────────────────────────────┘\n", color: "#FF8C00" }
          ]
        });

        continue;
      }

      b._РабочиеМеста = slots;

      var tmp = { "Рабочие места": slots };
      var s = getBuildingStaffingSimple(tmp, labor);

      b._Рабочие = s.Рабочие;
      b._ЭффективностьТруда = s.Эффективность;

      affected++;

      // (опционально) дефицит по зданию
      var missingForBuilding = Math.max(0, slots - s.Рабочие);
      var missingRateForBuilding = slots > 0 ? missingForBuilding / slots : 0;

      if (s.Рабочие <= 0) {
        if (b.Активно !== false) turnedOff++;
        b.Активно = false;

        pushNotice(data, {
          category: "Рынок труда",
          sub: "Здание",
          priority: 900,
          parts: [
            { text: "Здание отключено (нет рабочих)\n", bold: true, color: "#E36A6A" },
            { text: "┌────────────────────────────────────────────────────────┐\n", color: "#E36A6A" },

            { text: "┃", bold: true, color: "#E36A6A" },
            { text: " ➔ Здание: ", bold: true, color: "#CFC7BA" },
            { text: String(b.Тип || "Здание") + "\n", bold: true, color: "#E6E6FA" },

            { text: "┃", bold: true, color: "#E36A6A" },
            { text: " ➔ Провинция: ", bold: true, color: "#CFC7BA" },
            { text: String(b.Провинция) + "\n", bold: true, color: "#E6E6FA" },

            { text: "┃", bold: true, color: "#E36A6A" },
            { text: " ➔ Необходимо рабочих: ", bold: true, color: "#CFC7BA" },
            { text: String(slots) + "\n", bold: true, color: "#E6E6FA" },

            { text: "┃", bold: true, color: "#E36A6A" },
            { text: " ➔ Доступно рабочих: ", bold: true, color: "#CFC7BA" },
            { text: "0\n", bold: true, color: "#E36A6A" },

            { text: "└────────────────────────────────────────────────────────┘\n", color: "#E36A6A" }
          ]
        });
      } else {
        var effPct = (Math.round(s.Эффективность * 1000) / 10);

        var bParts = [
          { text: "Обеспечение здания рабочими\n", bold: true, color: "#FF8C00" },
          { text: "┌────────────────────────────────────────────────────────┐\n", color: "#FF8C00" },

          { text: "┃", bold: true, color: "#FF8C00" },
          { text: " ➔ Здание: ", bold: true, color: "#CFC7BA" },
          { text: String(b.Тип || "Здание") + "\n", bold: true, color: "#E6E6FA" },

          { text: "┃", bold: true, color: "#FF8C00" },
          { text: " ➔ Провинция: ", bold: true, color: "#CFC7BA" },
          { text: String(b.Провинция) + "\n", bold: true, color: "#E6E6FA" },

          { text: "┃", bold: true, color: "#FF8C00" },
          { text: " ➔ Необходимо рабочих: ", bold: true, color: "#CFC7BA" },
          { text: String(slots) + "\n", bold: true, color: "#E6E6FA" },

          { text: "┃", bold: true, color: "#FF8C00" },
          { text: " ➔ Нанято рабочих: ", bold: true, color: "#CFC7BA" },
          { text: String(s.Рабочие) + "\n", bold: true, color: "#E6E6FA" },

          { text: "┃", bold: true, color: "#FF8C00" },
          { text: " ➔ Обеспечено: ", bold: true, color: "#CFC7BA" },
          { text: String(effPct) + "%\n", bold: true, color: "#E6E6FA" }
        ];

        if (missingForBuilding > 0) {
          bParts.push(
            { text: "┃", bold: true, color: "#FF8C00" },
            { text: " ➔ Нехватка рабочих: ", bold: true, color: "#CFC7BA" },
            { text: String(missingForBuilding), bold: true, color: "#E36A6A" },
            { text: " (" + (Math.round(missingRateForBuilding * 1000) / 10) + "%)\n", bold: true, color: "#E36A6A" }
          );
        } else {
          bParts.push(
            { text: "┃", bold: true, color: "#FF8C00" },
            { text: " ➔ Нехватка рабочих: ", bold: true, color: "#CFC7BA" },
            { text: "0\n", bold: true, color: "#E6E6FA" }
          );
        }

        bParts.push({ text: "└────────────────────────────────────────────────────────┘\n", color: "#FF8C00" });

        pushNotice(data, {
          category: "Рынок труда",
          sub: "Здание",
          priority: 70,
          parts: bParts
        });
      }
    }
  }

  pushNotice(data, {
    category: "Рынок труда",
    sub: "Итог",
    priority: 90,
    parts: [
      { text: "Итоги обработки рынка труда\n", bold: true, color: "#FF8C00" },
      { text: "┌────────────────────────────────────────────────────────┐\n", color: "#FF8C00" },

      { text: "┃", bold: true, color: "#FF8C00" },
      { text: " ➔ Обработано зданий: ", bold: true, color: "#CFC7BA" },
      { text: String(affected) + "\n", bold: true, color: "#E6E6FA" },

      { text: "┃", bold: true, color: "#FF8C00" },
      { text: " ➔ Отключено зданий из-за отсутствия рабочих: ", bold: true, color: "#CFC7BA" },
      { text: String(turnedOff) + "\n", bold: true, color: "#E36A6A" },

      { text: "└────────────────────────────────────────────────────────┘\n", color: "#FF8C00" }
    ]
  });
}


/* =======================
   ПУБЛИЧНАЯ ТОЧКА ВХОДА (НА ХОД)
   ======================= */

function processTurnLaborOurOnly(data) {
  ensureNews(data);

  var res = rebuildLaborMarketOurOnly(data);
  if (res && res.ok) {
    applyLaborEffectToBuildingsOurOnly(data);
  } else {
    pushNotice(data, {
      category: "Рынок труда",
      sub: "Ошибка",
      priority: 999,
      parts: [
        { text: "Обработка пропущена\n", bold: true, color: "#E36A6A" },
        { text: "┌────────────────────────────────────────────────────────┐\n", color: "#E36A6A" },

        { text: "┃", bold: true, color: "#E36A6A" },
        { text: " ➔ Причина: ", bold: true, color: "#CFC7BA" },
        { text: "ошибка чтения данных государства\n", bold: true, color: "#E36A6A" },

        { text: "┃", bold: true, color: "#E36A6A" },
        { text: " ➔ Эффект: ", bold: true, color: "#CFC7BA" },
        { text: "обработка зданий не выполнена\n", bold: true, color: "#E6E6FA" },

        { text: "└────────────────────────────────────────────────────────┘\n", color: "#E36A6A" }
      ]
    });
  }

  return data;
}