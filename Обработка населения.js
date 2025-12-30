/* =========================================================
   РЫНОК ТРУДА (БАЗОВЫЙ) — ОБНОВЛЁННЫЙ ПОД ТВОЙ ФОРМАТ "ЯЧЕЙКИ"
   Google Apps Script

   КЛЮЧЕВОЕ ОБНОВЛЕНИЕ:
   ✅ Читаем параметры государства ТОЛЬКО из data["Данные государства"],
      где формат может быть 1D или 2D (ячейка/строка из Sheets):

      1D:
      data["Данные государства"] = [
        {"Идентификатор государства": 62},
        {"Коэффициент рабочей силы": 0.4}
      ]

      2D:
      data["Данные государства"] = [
        [
          {"Идентификатор государства": 62},
          {"Коэффициент рабочей силы": 0.4}
        ]
      ]

   ❌ Старый keys/vals (data["Идентификатор данных государства"]) больше не используется

   Остальное:
   ✅ Рынок труда, рабочая сила и занятость считаются ТОЛЬКО для провинций нашего государства
   ✅ Рынок труда хранится в data["Рынок труда"]
   ✅ "Доля занятости" хранится в записи рынка труда провинции
   ✅ Новости в data.Новости
   ✅ Отказоустойчивость + понятные сообщения
   ========================================================= */


/* =======================
   ВСПОМОГАТЕЛЬНЫЕ
   ======================= */

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

// Функция расчёта рабочих мест по уровню
function computeRequiredWorkersByLevel(base, level, growthCoef) {
  base = Number(base) || 0;
  level = Math.max(1, Math.floor(Number(level) || 1));
  growthCoef = Number(growthCoef);
  if (isNaN(growthCoef)) growthCoef = 0.8;

  // base * (1 + growthCoef * (level - 1))
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

  // 1) если в здании явно задано число — используем его
  if (typeof building["Рабочие места"] === "number") {
    return Math.max(0, Math.floor(building["Рабочие места"]));
  }

  // 2) иначе — считаем по шаблону
  var tpl = templatesMap && building.Тип ? templatesMap[building.Тип] : null;
  if (!tpl || !tpl.Труд) return 0;

  var base = tpl.Труд.База;
  var k = tpl.Труд.ПриростЗаУровень;

  // уровень здания: поддержим "Уровень" или "Ур."
  var lvl = building.Уровень !== undefined ? building.Уровень : (building["Ур."] !== undefined ? building["Ур."] : 1);

  return computeRequiredWorkersByLevel(base, lvl, k);
}

/* =======================
   ГОСУДАРСТВО: читаем из "ячейки" data["Данные государства"]
   ======================= */

/**
 * Ищет параметр в data["Данные государства"] (1D/2D массив объектов).
 * Возвращает значение или undefined.
 */
function getStateParamFromCell(data, key) {
  var root = data ? data["Данные государства"] : null;
  if (root === null || root === undefined) return undefined;

  // flatten 2D -> 1D
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
    data.Новости.push("⛔ Рынок труда: не найден 'Идентификатор государства' в data['Данные государства'] (ячейка JSON).");
    return null;
  }
  return String(v).trim();
}

function getWorkforceCoefficientSafe(data) {
  ensureNews(data);

  var v = getStateParamFromCell(data, "Коэффициент рабочей силы");
  if (v === undefined || v === null || v === "") {
    data.Новости.push("⚠️ Рынок труда: не найден 'Коэффициент рабочей силы' в data['Данные государства']. Принято 0.");
    return 0;
  }

  var num = Number(v);
  if (isNaN(num)) {
    data.Новости.push("⚠️ Рынок труда: 'Коэффициент рабочей силы' не число (" + String(v) + "). Принято 0.");
    return 0;
  }

  var clamped = clamp01(num);
  if (clamped !== num) {
    data.Новости.push("⚠️ Рынок труда: 'Коэффициент рабочей силы' вне [0..1] (" + num + "). Обрезано до " + clamped + ".");
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
      if (
        pop &&
        typeof pop === "object" &&
        pop.Провинция === provinceName &&
        typeof pop.Количество === "number"
      ) {
        total += pop.Количество;
      }
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
   ПОСТРОЙКИ: спрос
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
      "Безработица": 0
    };
    data["Рынок труда"][0].push(entry);
  }

  entry["Население"] = population;
  entry["Рабочая сила"] = workforce;
  entry["Спрос"] = demand;

  // Доля занятости (как доля заполнения рабочих мест, у тебя было workforce/demand)
  var occ = null;
  if (occupancyShareNullable !== null && occupancyShareNullable !== undefined) {
    occ = clamp01(occupancyShareNullable);
  } else {
    occ = (demand > 0) ? clamp01(workforce / demand) : 0; // ✅ лучше 0, а не 1
  }
  entry["Доля занятости"] = occ;

  // ✅ Безработица (по рабочей силе)
  var employed = Math.min(workforce, demand);
  var unemployed = Math.max(0, workforce - employed);
  var unempRate = workforce > 0 ? unemployed / workforce : 0;

  entry["Занятые"] = employed;
  entry["Безработные"] = unemployed;
  entry["Безработица"] = clamp01(unempRate);

  return entry;
}

/* =======================
   ПЕРЕСБОРКА РЫНКА ТРУДА: только наши + новости
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
    data.Новости.push("⚠️ Рынок труда: у государства " + stateId + " нет провинций (или не заполнен 'Владелец').");
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

    data.Новости.push(
      "👷 Рынок труда: " + provName +
      " | Население: " + entry["Население"] +
      " | Раб.сила: " + entry["Рабочая сила"] +
      " | Спрос: " + entry["Спрос"] +
      " | Занятые: " + entry["Занятые"] +
      " | Безработные: " + entry["Безработные"] +
      " | Безработица: " + (Math.round(entry["Безработица"] * 1000) / 10) + "%"
    );
  } // ✅ ЗАКРЫЛИ for

  // ✅ ИТОГ ПОСЛЕ for
  var employedTotal = Math.min(totalWorkforce, totalDemand);
  var unemployedTotal = Math.max(0, totalWorkforce - employedTotal);
  var unempTotalRate = totalWorkforce > 0 ? unemployedTotal / totalWorkforce : 0;

  data.Новости.push(
    "📊 Рынок труда (итог): провинций=" + provinces.length +
    " | Население=" + totalPop +
    " | Раб.сила=" + totalWorkforce +
    " | Спрос=" + totalDemand +
    " | Занятые=" + employedTotal +
    " | Безработные=" + unemployedTotal +
    " | Безработица=" + (Math.round(unempTotalRate * 1000) / 10) + "%"
  );

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
    data.Новости.push("⚠️ Рынок труда: data.Постройки отсутствует или не массив — здания не обработаны.");
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
      
      if (b.Активно === false) {
      b._Рабочие = 0;
      b._ЭффективностьТруда = 0;

affected++;

      data.Новости.push(
        "⏸ Труд: " + (b.Тип || "Здание") + " (" + b.Провинция + ") " +
        "| Активно=false → рабочих=0"
      );
      continue;
    }

      var labor = getLaborMarketByProvince(data, b.Провинция);
      var slots = getBuildingWorkSlots(data, templatesMap, b);

if (slots <= 0) {
  b._РабочиеМеста = 0;
  b._Рабочие = 0;
  b._ЭффективностьТруда = 0;

  affected++;

  data.Новости.push(
    "⚠️ Труд: " + (b.Тип || "Здание") + " (" + b.Провинция + ") " +
    "| Раб.мест=0 (нет 'Рабочие места' и/или tpl.Труд) → пропуск"
  );
  continue;
}

b._РабочиеМеста = slots;
var tmp = { "Рабочие места": slots };
var s = getBuildingStaffingSimple(tmp, labor);

      b._Рабочие = s.Рабочие;
      b._ЭффективностьТруда = s.Эффективность;

      affected++;

      if (s.Рабочие <= 0) {
        if (b.Активно !== false) turnedOff++;
        b.Активно = false;

        data.Новости.push(
  "⛔ Труд: " + (b.Тип || "Здание") + " (" + b.Провинция + ") " +
  "| Раб.мест=" + slots +
  " | Рабочие=0 → отключено"
);
      } else {
        data.Новости.push(
          "🏭 Труд: " + (b.Тип || "Здание") + " (" + b.Провинция + ") " +
          "| Раб.мест=" + slots +
          " | Рабочие=" + s.Рабочие +
          " | Эфф=" + (Math.round(s.Эффективность * 1000) / 10) + "%"
        );
      }
    }
  }

  data.Новости.push("🏗 Труд (итог): обработано зданий=" + affected + ", отключено из-за 0 рабочих=" + turnedOff + ".");
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
    data.Новости.push("⛔ Рынок труда: пропуск обработки зданий из-за ошибок чтения данных государства.");
  }

  return data;
}