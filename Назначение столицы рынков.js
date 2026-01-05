/* =========================================================
   НАЗНАЧЕНИЕ СТОЛИЦ РЫНКОВ (ТОЛЬКО "НАШИ" РЫНКИ) + НОВОСТИ
   Наш рынок = рынок, где есть хотя бы 1 провинция с Владелец === stateId
   ========================================================= */

function LOG_marketsAssignCapitalsByThroughput(data) {
  if (!data || typeof data !== "object") return data;

  ensureNews(data);

  var provinces = LOG_getAllProvincesFlat_(data);
  if (!provinces.length) return data;

  var stateId = LOG_getStateIdFromStateData_(data);
  if (!stateId) return data;
  stateId = String(stateId);

  // 1) Определяем рынки, где у нас есть хотя бы одна провинция
  var ourByMarket = LOG_groupOurProvincesByMarket_(provinces, stateId); // marketId -> [provKey]
  var targetMarketIds = Object.keys(ourByMarket);
  if (!targetMarketIds.length) return data;

  // 2) Для каждого такого рынка — собираем все провинции рынка
  var provByKey = {};
  for (var i = 0; i < provinces.length; i++) {
    var pk = LOG_getProvKey_(provinces[i]);
    if (pk) provByKey[pk] = provinces[i];
  }

  // Для новости
  var changes = []; // {marketId, prev, next, nextCap, reason}
  var processed = 0;
  var changedCount = 0;
  var assignedRandom = 0;

  for (var mi = 0; mi < targetMarketIds.length; mi++) {
    var marketId = String(targetMarketIds[mi]);

    // scope по рынку (все провинции рынка)
    var scope = LOG_getMarketScope_(provinces, marketId);
    var keys = scope.marketProvKeys || [];
    if (!keys.length) continue;

    var arr = [];
    for (var k = 0; k < keys.length; k++) {
      var p = provByKey[keys[k]];
      if (p) arr.push(p);
    }
    if (!arr.length) continue;

    processed++;

    // 0) текущие столицы
    var prevCapitals = [];
    for (var x = 0; x < arr.length; x++) {
      if (arr[x].СтолицаРынка === true) prevCapitals.push(arr[x]);
    }
    var prevKey = (prevCapitals.length === 1) ? (LOG_getProvKey_(prevCapitals[0]) || "") : "";

    // 1) нормализуем флаг + ищем max cap среди всех провинций рынка
    var bestProv = null;
    var bestCap = -Infinity;

    for (var j = 0; j < arr.length; j++) {
      var p2 = arr[j];

      if (p2.СтолицаРынка !== true) p2.СтолицаРынка = false;

      var cap = Number(p2.ПропускнаяСпособность);
      if (!isFinite(cap)) cap = 0;

      if (cap > bestCap) {
        bestCap = cap;
        bestProv = p2;
      }
    }

    // 2) если max cap <= 0 → случайная
    var reason = "max cap";
    if (!(bestCap > 0)) {
      var idx = Math.floor(Math.random() * arr.length);
      bestProv = arr[idx];
      bestCap = Number(bestProv && bestProv.ПропускнаяСпособность);
      if (!isFinite(bestCap)) bestCap = 0;

      reason = "случайная (все cap ≤ 0)";
      assignedRandom++;
    }

    // 3) выставляем единственную столицу
    for (var z = 0; z < arr.length; z++) {
      arr[z].СтолицаРынка = (arr[z] === bestProv);
    }

    var nextKey = LOG_getProvKey_(bestProv) || "";

    // 4) изменения в статистику
    var prevWasValid = (prevCapitals.length === 1);
    var changed = (!prevWasValid) || (prevKey !== nextKey);

    if (changed) {
      changedCount++;
      changes.push({
        marketId: marketId,
        prev: prevWasValid ? prevKey : (prevCapitals.length ? ("(" + prevCapitals.length + " столиц)") : "(нет столицы)"),
        next: nextKey,
        nextCap: bestCap,
        reason: reason
      });
    }
  }

  // 5) Новость (1 карточка)
  LOG_pushMarketCapitalAssignNotice_(data, {
    processed: processed,
    changedCount: changedCount,
    assignedRandom: assignedRandom,
    changes: changes
  });

  return data;
}

/* =======================
   НОВОСТЬ-СВОДКА
   ======================= */

function LOG_pushMarketCapitalAssignNotice_(data, stats) {
  ensureNews(data);

  var ORANGE = "#FF8C00";
  var RED    = "#E36A6A";
  var LABEL  = "#CFC7BA";
  var VALUE  = "#E6E6FA";

  var parts = [];

  parts.push({ text: "Столицы рынков (авто-настройка)\n", bold: true, color: ORANGE });
  parts.push({ text: "┌────────────────────────────────────────────────────────┐\n", color: ORANGE });

  parts.push({ text: "┃", bold: true, color: ORANGE });
  parts.push({ text: " ➔ Рынков (где есть наши провинции): ", bold: true, color: LABEL });
  parts.push({ text: String(stats.processed || 0) + "\n", bold: true, color: VALUE });

  parts.push({ text: "┃", bold: true, color: ORANGE });
  parts.push({ text: " ➔ Изменений/исправлений: ", bold: true, color: LABEL });
  parts.push({ text: String(stats.changedCount || 0) + "\n", bold: true, color: VALUE });

  parts.push({ text: "┃", bold: true, color: ORANGE });
  parts.push({ text: " ➔ Случайных назначений: ", bold: true, color: LABEL });
  parts.push({ text: String(stats.assignedRandom || 0) + "\n", bold: true, color: (stats.assignedRandom > 0 ? RED : VALUE) });

  var list = stats.changes || [];
  if (list.length) {
    parts.push({ text: "┃\n", bold: true, color: ORANGE });
    parts.push({ text: "┃", bold: true, color: ORANGE });
    parts.push({ text: " ➔ Детали (до 10):\n", bold: true, color: LABEL });

    var shown = Math.min(10, list.length);
    for (var i = 0; i < shown; i++) {
      var c = list[i];

      parts.push({ text: "┃", bold: true, color: ORANGE });
      parts.push({ text: "   • ", bold: true, color: LABEL });
      parts.push({ text: "Рынок ", bold: true, color: LABEL });
      parts.push({ text: String(c.marketId) + ": ", bold: true, color: VALUE });

      parts.push({ text: String(c.prev) + " → ", bold: true, color: VALUE });
      parts.push({ text: String(c.next), bold: true, color: VALUE });

      parts.push({ text: " (cap ", bold: true, color: LABEL });
      parts.push({ text: LOG_fmtInt_(c.nextCap), bold: true, color: VALUE });
      parts.push({ text: ")\n", bold: true, color: LABEL });

      parts.push({ text: "┃", bold: true, color: ORANGE });
      parts.push({ text: "     причина: ", bold: true, color: LABEL });
      parts.push({ text: String(c.reason) + "\n", bold: true, color: (String(c.reason).indexOf("случайная") >= 0 ? RED : VALUE) });
    }

    if (list.length > shown) {
      parts.push({ text: "┃", bold: true, color: ORANGE });
      parts.push({ text: "     ...и ещё ", bold: true, color: LABEL });
      parts.push({ text: String(list.length - shown), bold: true, color: VALUE });
      parts.push({ text: " рынков\n", bold: true, color: LABEL });
    }
  }

  parts.push({ text: "└────────────────────────────────────────────────────────┘\n", color: ORANGE });

  pushNotice(data, {
    category: "Логистика",
    sub: "Столицы рынков",
    priority: 95,
    parts: parts
  });
}