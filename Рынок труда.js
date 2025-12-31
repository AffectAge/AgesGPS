/* =========================================================
   РЫНОК ТРУДА (БАЗОВЫЙ) — ГОТОВАЯ ВЕРСИЯ (ТВОЙ ФОРМАТ "ЯЧЕЙКИ")
   Google Apps Script

   ✅ Чтение параметров государства из data["Данные государства"] (1D/2D)
   ✅ Рынок труда только для наших провинций
   ✅ Спрос считается по активным зданиям (Активно === true)
   ✅ Рабочие места:
      - если в здании задано "Рабочие места" (число) — берём его
      - иначе считаем из шаблона по уровню: base*(1+k*(level-1))
        tpl.Труд.База, tpl.Труд.ПриростЗаУровень (по умолчанию k=0.8)
   ✅ Здания с Активно=false не занимают рабочую силу (служебные поля = 0)
   ✅ Безработица (по рабочей силе) + Дефицит рабочей силы (по спросу)
   ✅ Пишем новости в data.Новости
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
      "📊 Трудовые ресурсы провинции " + "➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️" + "\n⛰️ Провинция: " + provName +
      "\n👨‍👩‍👦 Общее население: " + entry["Население"] +
      "\n👷‍♂️ Всего рабочих: " + entry["Рабочая сила"] +
      "\n🧑‍🔧 Необходимо рабочих: " + entry["Спрос"] +
      "\n🧑‍🔧 Занято рабочих: " + entry["Занятые"] +
      "\n🙋 Безработные рабочие: " + entry["Безработные"] +
      "\n🙋 Уровень безработицы: " + (Math.round(entry["Безработица"] * 1000) / 10) + "%" +
      (entry["Дефицит"] > 0
        ? " | Дефицит: " + entry["Дефицит"] +
          " (" + (Math.round(entry["Дефицит %"] * 1000) / 10) + "%)"
        : "")
    );
  }

  var employedTotal = Math.min(totalWorkforce, totalDemand);
  var unemployedTotal = Math.max(0, totalWorkforce - employedTotal);
  var unempTotalRate = totalWorkforce > 0 ? unemployedTotal / totalWorkforce : 0;

  var totalDeficit = Math.max(0, totalDemand - totalWorkforce);
  var totalDeficitRate = totalDemand > 0 ? totalDeficit / totalDemand : 0;

  data.Новости.push(
  "📊 Трудовые ресурсы государства\n" +
  "➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️➖️" +
  "⛰️ Провинций государства: " + provinces.length + "\n" +
  "👨‍👩‍👦 Общее население: " + totalPop + "\n" +
  "👷‍♂️ Всего рабочих: " + totalWorkforce + "\n" +
  "🧑‍🔧 Необходимо рабочих: " + totalDemand + "\n" +
  "🧑‍🔧 Занятые рабочие: " + employedTotal + "\n" +
  "🙋 Безработные рабочие: " + unemployedTotal + "\n" +
  "🙋 Уровень безработицы: " +
    (Math.round(unempTotalRate * 1000) / 10) + "%" +
  (totalDeficit > 0
    ? "\n❗ Дефицит: " + totalDeficit +
      " (" + (Math.round(totalDeficitRate * 1000) / 10) + "%)"
    : "")
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

      // Активно=false не потребляет труд
      if (b.Активно === false) {
        b._РабочиеМеста = 0;
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

      // (опционально) дефицит по зданию
      var missingForBuilding = Math.max(0, slots - s.Рабочие);
      var missingRateForBuilding = slots > 0 ? missingForBuilding / slots : 0;

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
          "🏭 Постройка" + b.Тип + " в провинции " + b.Провинция +
          "\n🧑‍🔧 Необходимо рабочих: " + slots +
          "\n🧑‍🔧 Нанято рабочих: " + s.Рабочие +
          "\n👷‍♂️ Обеспечено от необходимых" + (Math.round(s.Эффективность * 1000) / 10) + "%" +
          (missingForBuilding > 0
            ? " | Нехватка=" + missingForBuilding +
              " (" + (Math.round(missingRateForBuilding * 1000) / 10) + "%)"
            : "")
        );
      }
    }
  }

  data.Новости.push("🏗 Рынок труда обработал зданий: " + affected + "\n Из-за отсутствия свободных рабочих отключено:" + turnedOff + " зданий");
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