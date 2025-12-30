/* =========================================================
   РЫНОК ТРУДА (БАЗОВЫЙ) — ТОЛЬКО НАШИ ПРОВИНЦИИ
   Вариант: data["Данные государства"] — ОДИН JSON-ОБЪЕКТ
   - Коэффициент рабочей силы хранится в JSON: data["Данные государства"]["Коэффициент рабочей силы"]
   - Рынок труда хранится в data["Рынок труда"]
   - Добавлены новости (data.Новости)
   ========================================================= */

/* =======================
   ВСПОМОГАТЕЛЬНЫЕ
   ======================= */

function normalizeToArray(value) {
  if (Array.isArray(value)) return value;
  if (value === null || value === undefined) return [];
  return [value];
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

function ensureNews(data) {
  if (!Array.isArray(data.Новости)) data.Новости = [];
}

/* =======================
   ГОСУДАРСТВО (JSON)
   ======================= */

function getStateObject(data) {
  var s = data["Данные государства"];
  return (s && typeof s === "object" && !Array.isArray(s)) ? s : null;
}

function getStateId(data) {
  var s = getStateObject(data);
  if (!s) return null;
  var v = s["Идентификатор государства"];
  if (v === null || v === undefined) return null;
  return String(v).trim() || null;
}

function getWorkforceCoefficient(data) {
  var s = getStateObject(data);
  if (!s) return 0;
  var v = Number(s["Коэффициент рабочей силы"]);
  return isNaN(v) ? 0 : clamp01(v);
}

/* =======================
   ПРОВИНЦИИ (ТОЛЬКО НАШИ)
   ======================= */

function getAllProvincesFlat(data) {
  if (!Array.isArray(data.Провинции)) return [];
  return normalizeToArray(data.Провинции).reduce(function (acc, row) {
    return acc.concat(normalizeToArray(row));
  }, []).filter(function (p) {
    return p && typeof p === "object" && p.Провинция;
  });
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
   РЫНОК ТРУДА
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

/* === POP -> Население провинции === */
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

/* === Рабочая сила = население * коэффициент === */
function calculateWorkforceFromPopulation(data, provinceName) {
  var coef = getWorkforceCoefficient(data);
  if (coef <= 0) return 0;
  var popTotal = calculatePopulationTotal(data, provinceName);
  return Math.max(0, Math.floor(popTotal * coef));
}

/* === Спрос = сумма "Рабочие места" активных зданий в провинции ===
   Мы вызываем только для наших провинций, но оставляем защиту на b._isOurProvince если поле уже есть.
*/
function calculateLaborDemand(data, provinceName) {
  if (!Array.isArray(data.Постройки)) return 0;

  var demand = 0;
  var rows = normalizeToArray(data.Постройки);

  for (var i = 0; i < rows.length; i++) {
    var row = normalizeToArray(rows[i]);
    for (var j = 0; j < row.length; j++) {
      var b = row[j];
      if (!b || typeof b !== "object") continue;
      if (b.Провинция !== provinceName) continue;
      if (b.Активно === false) continue;
      if (b._isOurProvince === false) continue; // если кто-то выставил флаг
      if (typeof b["Рабочие места"] !== "number") continue;

      demand += b["Рабочие места"];
    }
  }

  return Math.max(0, Math.floor(demand));
}

function upsertLaborMarketEntry(data, provinceName, workforce, demand, occupancyShareNullable) {
  normalizeLaborMarket(data);

  var entry = getLaborMarketByProvince(data, provinceName);

  if (!entry) {
    entry = {
      Провинция: provinceName,
      "Население": 0,
      "Рабочая сила": 0,
      "Спрос": 0,
      "Доля занятости": 0
    };
    data["Рынок труда"][0].push(entry);
  }

  entry["Население"] = calculatePopulationTotal(data, provinceName);
  entry["Рабочая сила"] = workforce;
  entry["Спрос"] = demand;

  // Пользовательское правило: доля занятости хранится в рынке труда.
  // Если occupancyShareNullable передан — используем его, иначе рассчитываем базово.
  if (occupancyShareNullable !== null && occupancyShareNullable !== undefined) {
    entry["Доля занятости"] = clamp01(occupancyShareNullable);
  } else {
    entry["Доля занятости"] = (demand > 0) ? clamp01(workforce / demand) : 1;
  }

  return entry;
}

/* === ПЕРЕСБОРКА РЫНКА ТРУДА: только наши провинции + новости === */
function rebuildLaborMarketOurOnly(data) {
  ensureNews(data);
  normalizeLaborMarket(data);

  // очищаем и пересобираем
  data["Рынок труда"] = [[]];

  var stateId = getStateId(data);
  if (!stateId) {
    data.Новости.push("⛔ Рынок труда: не найден 'Идентификатор государства' в data['Данные государства'] (JSON).");
    return { stateId: null, ourCount: 0 };
  }

  var coef = getWorkforceCoefficient(data);
  if (coef <= 0) {
    data.Новости.push("⚠️ Рынок труда: 'Коэффициент рабочей силы' = 0 (или отсутствует). Рабочая сила будет 0.");
  }

  var ourMap = buildOurProvincesMap(data, stateId);
  var provinces = Object.keys(ourMap);

  var totalWorkforce = 0;
  var totalDemand = 0;

  for (var i = 0; i < provinces.length; i++) {
    var provName = provinces[i];

    var workforce = calculateWorkforceFromPopulation(data, provName);
    var demand = calculateLaborDemand(data, provName);

    totalWorkforce += workforce;
    totalDemand += demand;

    var entry = upsertLaborMarketEntry(data, provName, workforce, demand, null);

    // Новости по провинции (детализация). Если будет слишком шумно — можно отключить.
    data.Новости.push(
      "👷 Рынок труда: " + provName +
      " | Население: " + entry["Население"] +
      " | Раб.сила: " + entry["Рабочая сила"] +
      " | Спрос: " + entry["Спрос"] +
      " | Доля занятости: " + Math.round(entry["Доля занятости"] * 1000) / 10 + "%"
    );
  }

  data.Новости.push(
    "📊 Рынок труда (итог): провинций нашего государства: " + provinces.length +
    " | Раб.сила: " + totalWorkforce +
    " | Спрос: " + totalDemand +
    (totalDemand > 0 ? " | Средняя занятость: " + (Math.round((totalWorkforce / totalDemand) * 1000) / 10) + "%" : " | Спрос=0")
  );

  return { stateId: stateId, ourCount: provinces.length };
}

/* =======================
   ЗДАНИЯ: УКОМПЛЕКТОВАННОСТЬ
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

/* Применяем эффект труда к зданиям только в наших провинциях + новости */
function applyLaborEffectToBuildingsOurOnly(data) {
  ensureNews(data);

  var stateId = getStateId(data);
  if (!stateId) return;

  var ourMap = buildOurProvincesMap(data, stateId);

  if (!Array.isArray(data.Постройки)) return;

  var rows = normalizeToArray(data.Постройки);
  var affected = 0;
  var turnedOff = 0;

  for (var i = 0; i < rows.length; i++) {
    var row = normalizeToArray(rows[i]);
    for (var j = 0; j < row.length; j++) {
      var b = row[j];
      if (!b || typeof b !== "object") continue;
      if (!b.Провинция) continue;
      if (!ourMap[b.Провинция]) continue; // ключевое: только наши провинции

      var labor = getLaborMarketByProvince(data, b.Провинция);
      var s = getBuildingStaffingSimple(b, labor);

      b._Рабочие = s.Рабочие;
      b._ЭффективностьТруда = s.Эффективность;

      affected++;

      // Базовое правило: если работников нет — выключаем (можно изменить позже на "работает с 0%")
      if (s.Рабочие <= 0) {
        if (b.Активно !== false) turnedOff++;
        b.Активно = false;

        data.Новости.push(
          "⛔ Недостаток рабочей силы: " + (b.Тип || "Здание") +
          " в " + b.Провинция +
          " | Раб.мест: " + (b["Рабочие места"] || 0) +
          " | Рабочие: 0 → отключено"
        );
      } else {
        data.Новости.push(
          "🏭 Труд: " + (b.Тип || "Здание") +
          " в " + b.Провинция +
          " | Раб.мест: " + (b["Рабочие места"] || 0) +
          " | Рабочие: " + s.Рабочие +
          " | Эффективность: " + (Math.round(s.Эффективность * 1000) / 10) + "%"
        );
      }
    }
  }

  data.Новости.push(
    "🏗 Итог по зданиям (труд): обработано " + affected + ", отключено из-за 0 рабочих: " + turnedOff + "."
  );
}