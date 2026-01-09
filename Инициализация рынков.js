/* =========================================================
   ТОРГОВЛЯ: рынки в СТОЛБЦЕ (1 рынок = 1 ячейка)
   + НОВОСТЬ-ОТЧЁТ (1 карточка) по итогам синхронизации

   Требования проекта:
   - normalizeToArray(value)
   - ensureNews(data)         (у тебя уже есть)
   - pushNotice(data, {category, sub, priority, parts}) (у тебя уже есть)
   ========================================================= */

function TRADE_syncMarkets(data) {
  if (!data || typeof data !== "object") return data;

  // ensure trade column exists
  if (!Array.isArray(data.Торговля)) data.Торговля = [];

  // === счётчики отчёта ===
  var rep = {
    marketsPresent: 0,
    marketsExisting: 0,
    marketsCreated: 0,
    marketsRemoved: 0,
    goodsUniverse: 0,
    marketsGoodsSynced: 0,
    goodsAdded: 0,
    goodsRemoved: 0
  };

  // 1) provinces: первая ячейка-массив из столбца "Провинции"
  var provinces = TRADE_pickFirstArrayCell_(data.Провинции);

  // 2) set рынков, которые реально присутствуют в провинциях
  var present = {}; // marketId -> true
  for (var i = 0; i < provinces.length; i++) {
    var p = provinces[i];
    if (!p || typeof p !== "object") continue;

    var mid = TRADE_marketIdFromProvince_(p);
    if (mid) present[mid] = true;
  }
  rep.marketsPresent = Object.keys(present).length;

  // 3) set товаров из шаблонов (строго Входы/Выходы)
  var goodsSet = TRADE_collectGoodsSetFromTemplatesColumn_STRICT_(data["Шаблоны зданий"]);
  rep.goodsUniverse = Object.keys(goodsSet).length;

  // 4) индекс существующих рынков в столбце Торговля
  var byId = {};         // marketId -> rowIndex
  var liveRowIdx = [];   // rowIndex[] рынков, которые должны существовать (present)

  for (var r = 0; r < data.Торговля.length; r++) {
    var cell = data.Торговля[r];
    if (!cell || typeof cell !== "object" || Array.isArray(cell)) continue;

    var id = TRADE_marketIdFromMarketCell_RU_(cell);
    if (!id) continue;

    byId[id] = r;
    rep.marketsExisting++;

    // если рынок реально используется провинциями — он “живой”
    if (present[id]) liveRowIdx.push(r);

    // ensure shape
    if (!cell["Товары"] || typeof cell["Товары"] !== "object") cell["Товары"] = {};
    if (cell["Идентификатор рынка"] == null || String(cell["Идентификатор рынка"]).trim() === "") {
      cell["Идентификатор рынка"] = String(id);
    }
  }

  // 5) + создать отсутствующие рынки (первый свободный слот)
  Object.keys(present).forEach(function (mid) {
    if (byId[mid] !== undefined) return;

    var marketObj = TRADE_makeDefaultMarket_RU_(data, mid, goodsSet);

    var slot = TRADE_findFirstFreeRow_(data.Торговля);
    if (slot === -1) {
      data.Торговля.push(marketObj);
      slot = data.Торговля.length - 1;
    } else {
      data.Торговля[slot] = marketObj;
    }

    byId[mid] = slot;
    liveRowIdx.push(slot);
    rep.marketsCreated++;
  });

  // 6) - удалить рынки, к которым не принадлежит ни одна провинция
  Object.keys(byId).forEach(function (mid) {
    if (present[mid]) return;
    var idx = byId[mid];
    data.Торговля[idx] = ""; // свободная ячейка
    delete byId[mid];
    rep.marketsRemoved++;
  });

  // 7) ✅ синхронизировать товары в “живых” рынках
  for (var k = 0; k < liveRowIdx.length; k++) {
    var rowIdx = liveRowIdx[k];
    var m = data.Торговля[rowIdx];
    if (!m || typeof m !== "object" || Array.isArray(m)) continue;

    var mid2 = TRADE_marketIdFromMarketCell_RU_(m);
    if (!mid2 || !present[mid2]) continue;

    if (!m["Товары"] || typeof m["Товары"] !== "object") m["Товары"] = {};
    var goods = m["Товары"];

    // + добавить недостающие товары (не меняя существующие)
    Object.keys(goodsSet).forEach(function (g) {
      if (!goods[g] || typeof goods[g] !== "object") {
        goods[g] = TRADE_defaultMarketGoodState_RU_(data, g);
        rep.goodsAdded++;
      }
    });

    // - удалить лишние товары
    Object.keys(goods).forEach(function (g) {
      if (!goodsSet[g]) {
        delete goods[g];
        rep.goodsRemoved++;
      }
    });

    rep.marketsGoodsSynced++;
  }

  // 8) новость-отчёт (1 карточка)
  TRADE_pushSyncNotice_RU_(data, rep);

  return data;
}

/* =======================
   NEWS (Vic3-tooltip style: единый тёмно-серый цвет)
   ======================= */

function TRADE_pushSyncNotice_RU_(data, rep) {
  if (typeof ensureNews === "function") ensureNews(data);
  if (typeof pushNotice !== "function") return;

  var C = "#6E675F"; // единый тёмно-серый

  function line(parts, s) { parts.push({ text: String(s) + "\n", color: C }); }

  var parts = [];
  line(parts, "Торговля: инициализация рынков и товаров");
  line(parts, "┌────────────────────────────────────────────────────────┐");
  line(parts, "┃ ➔ Рынков в провинциях: " + String(rep.marketsPresent));
  line(parts, "┃ ➔ Рынков с данными (до): " + String(rep.marketsExisting));
  line(parts, "┃ ➔ Создано рынков: " + String(rep.marketsCreated));
  line(parts, "┃ ➔ Удалено рынков: " + String(rep.marketsRemoved));
  line(parts, "┃");
  line(parts, "┃ ➔ Товаров в шаблонах: " + String(rep.goodsUniverse));
  line(parts, "┃ ➔ Рынков синхронизировано по товарам: " + String(rep.marketsGoodsSynced));
  line(parts, "┃ ➔ Добавлено товаров: " + String(rep.goodsAdded));
  line(parts, "┃ ➔ Удалено товаров: " + String(rep.goodsRemoved));
  line(parts, "└────────────────────────────────────────────────────────┘");

  pushNotice(data, {
    category: "Торговля",
    sub: "Инициализация",
    priority: 95,
    parts: parts
  });
}

/* =======================
   Helpers (как раньше)
   ======================= */

function TRADE_pickFirstArrayCell_(col) {
  if (!Array.isArray(col)) return [];
  for (var i = 0; i < col.length; i++) {
    if (Array.isArray(col[i])) return col[i];
  }
  return [];
}

function TRADE_marketIdFromProvince_(p) {
  if (!p || typeof p !== "object") return null;
  if (p.РынокId != null && String(p.РынокId).trim() !== "") return String(p.РынокId).trim();
  if (p.Рынок != null && String(p.Рынок).trim() !== "") return String(p.Рынок).trim();
  return null;
}

function TRADE_marketIdFromMarketCell_RU_(m) {
  if (!m || typeof m !== "object") return null;

  if (m["Идентификатор рынка"] != null && String(m["Идентификатор рынка"]).trim() !== "") {
    return String(m["Идентификатор рынка"]).trim();
  }
  // backward-compat
  if (m.РынокId != null && String(m.РынокId).trim() !== "") return String(m.РынокId).trim();
  if (m.id != null && String(m.id).trim() !== "") return String(m.id).trim();

  return null;
}

function TRADE_findFirstFreeRow_(tradeCol) {
  if (!Array.isArray(tradeCol)) return -1;
  for (var i = 0; i < tradeCol.length; i++) {
    var v = tradeCol[i];
    if (v === "" || v === null || v === undefined) return i;
    if (typeof v !== "object") return i;
    if (Array.isArray(v)) return i;
  }
  return -1;
}

function TRADE_collectGoodsSetFromTemplatesColumn_STRICT_(templatesCol) {
  var set = {};
  if (!Array.isArray(templatesCol)) return set;

  for (var r = 0; r < templatesCol.length; r++) {
    var cell = templatesCol[r];
    var list = [];

    if (Array.isArray(cell)) list = cell;
    else if (cell && typeof cell === "object") list = [cell];
    else continue;

    for (var i = 0; i < list.length; i++) {
      var tpl = list[i];
      if (!tpl || typeof tpl !== "object") continue;

      var ins = tpl.Входы;
      if (ins && typeof ins === "object" && !Array.isArray(ins)) {
        Object.keys(ins).forEach(function (g) {
          g = String(g || "").trim();
          if (g) set[g] = true;
        });
      }

      var outs = tpl.Выходы;
      if (outs && typeof outs === "object" && !Array.isArray(outs)) {
        Object.keys(outs).forEach(function (g) {
          g = String(g || "").trim();
          if (g) set[g] = true;
        });
      }
    }
  }

  return set;
}

function TRADE_makeDefaultMarket_RU_(data, marketId, goodsSet) {
  var market = {
    "Идентификатор рынка": String(marketId),
    "Товары": {}
  };

  Object.keys(goodsSet || {}).forEach(function (g) {
    market["Товары"][g] = TRADE_defaultMarketGoodState_RU_(data, g);
  });

  return market;
}

function TRADE_defaultMarketGoodState_RU_(data, good) {
  var basePrice = 1;

  if (data && data.ТорговляСправочникТоваров && data.ТорговляСправочникТоваров[good]) {
    var bp = data.ТорговляСправочникТоваров[good]["Базовая цена"];
    if (bp !== undefined && bp !== null && !isNaN(Number(bp))) basePrice = Number(bp);
  }

  return {
    "Цена": basePrice,
    "Запас": 0,
    "Предложение": 0,
    "Спрос": 0,
    "Нехватка": 0,
    "Куплено": 0,
    "Продано": 0
  };
}