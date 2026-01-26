/* =========================================================
   TRADE (NO MARKET STOCK):
   - Торговля только между зданиями (склады зданий = единственный источник товаров)
   - Рынки хранят цены и статистику, но НЕ хранят Запас
   - Покупатель платит throughput (1 за 1 единицу товара)
   - Внутри одного рынка пошлин нет
   - Через доступ к внешнему рынку:
       * Импортная пошлина платится покупателем (записывается в buyer.Пошлины)
       * Экспортная пошлина платится продавцом (удерживается из выручки, записывается в seller.Пошлины)
       * Доход от пошлин начисляется в data["Торговая политика государств"] по рынкам
   - Удаляем "мусор" на складе: оставляем только товары из Входов/Выходов постройки
   - ПрибыльЗаХод / РасходЗаХод — статистика (деньги реально двигаются в Деньги)

   ВАЖНО (по твоему требованию):
   ✅ Входы/Выходы берём ТОЛЬКО у постройки (b.Входы / b.Выходы)
   ✅ НЕ умножаем на уровень
   ❌ Никаких fallback на шаблон

   Требования проекта:
   - normalizeToArray(value)
   - ensureNews(data)
   - pushNotice(data, {category, sub, priority, parts})
   ========================================================= */


/* =======================
   MAIN ENTRY
   ======================= */

function TRADE_runBuildingCommerce(data) {
  if (!data || typeof data !== "object") return data;

  if (!Array.isArray(data.Торговля)) data.Торговля = [];
  if (!Array.isArray(data.Постройки)) data.Постройки = [];
  if (typeof ensureNews === "function") ensureNews(data);
  
  STATE_applyNationalMarket_Flat_(data)

  var stateId = TRADE_getStateId_(data);
  if (!stateId) {
    TRADE_pushErr_(data, "Торговля", "Ошибка", "Не найден \"Идентификатор государства\" в data[\"Данные государства\"]");
    return data;
  }
  stateId = String(stateId);

  // Provinces (flat + index)
  var provinces = TRADE_getAllProvincesFlat_(data);
  var provByKey = {};
  for (var i = 0; i < provinces.length; i++) {
    var p = provinces[i];
    var pk = TRADE_getProvKey_(p);
    if (pk) provByKey[String(pk)] = p;
  }

  // Markets index (only id + goods map with prices/fields)
  var marketById = TRADE_indexMarkets_(data.Торговля);

  // Templates (kept only because existing code expects arg; NOT used for IO)
  var tplByType = TRADE_buildTemplatesIndexByType_full_(data["Шаблоны зданий"]);

  // Policy index + cleanup/ensure
  var policyIdx = TRADE_buildPolicyIndex_(data);
  TRADE_policyEnsureAllMarketsHaveRows_(data, marketById, policyIdx);
  TRADE_policyCleanupOrphans_(data, marketById, policyIdx);
  TRADE_resetPolicyTurnCounters_(policyIdx);

  // Access pool (mutable) - from data["Данные государства"]
  var accessRef = TRADE_getStateAccessMarketsPool_(data, marketById);
  var accessPool = accessRef.pool; // {marketId: capRemaining}

  // Throughput left per province (buyers spend)
  var tpLeftProv = {};

  // Reset per-building turn fields + remove мусор со склада (по b.Входы/b.Выходы)
  TRADE_resetBuildingsTurnFieldsAndCleanStock_(data, stateId, provByKey, tplByType);

// === POP: prepare (ensure wallet/stock fields exist) ===
POPS_preparePopulationAgents_(data);

  // Collect orders (по b.Входы/b.Выходы, без умножения на уровень)
  var orders = TRADE_collectOrders_({
    data: data,
    stateId: stateId,
    provByKey: provByKey,
    tplByType: tplByType,
    marketById: marketById,
    tpLeftProv: tpLeftProv
  });
  
  // === POP: append buy orders (1st pass, preferred goods) ===
POPS_appendPopulationBuyOrders_({
  data: data,
  stateId: stateId,
  provByKey: provByKey,
  tpLeftProv: tpLeftProv,
  orders: orders
});

// === BUILD: append construction material buy orders (state as buyer) ===
BUILD_appendConstructionBuyOrders_({
  data: data,
  stateId: stateId,
  provByKey: provByKey,
  marketById: marketById,
  policyIdx: policyIdx,
  tpLeftProv: tpLeftProv,
  accessPool: accessPool,
  orders: orders
});

  // Clearing
  var rep = TRADE_clearInternalTrades_({
    data: data,
    orders: orders,
    marketById: marketById,
    policyIdx: policyIdx,
    tpLeftProv: tpLeftProv,
    accessPool: accessPool
  });
  
  // === POP: substitution 2nd pass ===
var rep2 = POPS_runSubstitutionSecondPass_({
  data: data,
  stateId: stateId,
  provByKey: provByKey,
  marketById: marketById,
  policyIdx: policyIdx,
  tpLeftProv: tpLeftProv,
  accessPool: accessPool,
  sellOrders: orders.sell
});

// === D) POP: consume & satisfaction (ОБЯЗАТЕЛЬНО ПОСЛЕ ОБОИХ КЛИРИНГОВ) ===
POPS_consumeAndScore_(data);

POPS_updateSoLAndPolitics_({
  data: data,
  stateId: stateId,
  provByKey: provByKey,
  marketById: marketById,
  policyIdx: policyIdx,
  accessPool: accessPool
});

POPS_pushNewsBundle_(data, {
  stateId: stateId,
  provByKey: provByKey,
  marketById: marketById,
  policyIdx: policyIdx,
  accessPool: accessPool
});

// merge a few counters so summary is correct
rep.buyBoughtUnits        += rep2.buyBoughtUnits;
rep.buyShortUnits         += rep2.buyShortUnits;
rep.buySpentMoney         += rep2.buySpentMoney;
rep.sellSoldUnits         += rep2.sellSoldUnits;
rep.sellEarnedMoney       += rep2.sellEarnedMoney;
rep.provThroughputSpent   += rep2.provThroughputSpent;
rep.accessThroughputSpent += rep2.accessThroughputSpent;
rep.tariffsPaid           += rep2.tariffsPaid;

TRADE_writeBackAccessMarketsPool_(accessRef.container, accessPool);

  // Summary notice
  TRADE_pushCommerceSummaryNotice_RU_(data, rep);

  return data;
}


/* =======================
   BUILDINGS: ensure/reset/clean
   ======================= */

function TRADE_resetBuildingsTurnFieldsAndCleanStock_(data, stateId, provByKey, tplByType) {
  normalizeToArray(data.Постройки).forEach(function (row) {
    normalizeToArray(row).forEach(function (b) {
      if (!b || typeof b !== "object") return;

      TRADE_ensureBuildingWalletAndStock_(b);
      b.ПрибыльЗаХод = 0;
      b.РасходЗаХод = 0;
      b.Пошлины = 0; // за ход (сбор позже на уровне государства)

      // чистим склад только для наших активных зданий (чтобы не трогать чужих/неактивных)
      if (b.Активно !== true) return;
      if (!b.Провинция) return;

      var p = provByKey[String(b.Провинция)];
      if (!p) return;
      if (String(p.Владелец || "") !== String(stateId)) return;

      // ✅ берём IO ТОЛЬКО у ПОСТРОЙКИ
      var io = TRADE_getBuildingIOMaps_(b);

      // ✅ удалить "мусор": оставить только товары из Входов/Выходов постройки
      var allowed = {};
      Object.keys(io.inputs).forEach(function (g) { allowed[String(g)] = true; });
      Object.keys(io.outputs).forEach(function (g) { allowed[String(g)] = true; });

      Object.keys(b.Склад).forEach(function (g) {
        if (!allowed[String(g)]) delete b.Склад[g];
      });

      // чистим нули
      Object.keys(b.Склад).forEach(function (g) {
        var v = Math.floor(Number(b.Склад[g]) || 0);
        if (v <= 0) delete b.Склад[g];
        else b.Склад[g] = v;
      });

      // чистим нехватки по тем же товарам
      if (b.Нехватка && typeof b.Нехватка === "object" && !Array.isArray(b.Нехватка)) {
        Object.keys(b.Нехватка).forEach(function (g) {
          if (!allowed[String(g)]) delete b.Нехватка[g];
        });
      }
    });
  });
}

function TRADE_ensureBuildingWalletAndStock_(b) {
  if (!b.Склад || typeof b.Склад !== "object" || Array.isArray(b.Склад)) b.Склад = {};
  if (b.Деньги == null || isNaN(Number(b.Деньги))) b.Деньги = 0;
  if (b.ПрибыльЗаХод == null || isNaN(Number(b.ПрибыльЗаХод))) b.ПрибыльЗаХод = 0;
  if (b.РасходЗаХод == null || isNaN(Number(b.РасходЗаХод))) b.РасходЗаХод = 0;
  if (b.Пошлины == null || isNaN(Number(b.Пошлины))) b.Пошлины = 0;
}

// ✅ IO maps from building only (no fallback)
function TRADE_getBuildingIOMaps_(b) {
  var inputs = (b && b.Входы && typeof b.Входы === "object" && !Array.isArray(b.Входы)) ? b.Входы : {};
  var outputs = (b && b.Выходы && typeof b.Выходы === "object" && !Array.isArray(b.Выходы)) ? b.Выходы : {};

  var in2 = {};
  Object.keys(inputs).forEach(function (g) {
    var v = Number(inputs[g]);
    if (isFinite(v) && v > 0) in2[String(g)] = v;
  });

  var out2 = {};
  Object.keys(outputs).forEach(function (g) {
    var v2 = Number(outputs[g]);
    if (isFinite(v2) && v2 > 0) out2[String(g)] = v2;
  });

  return { inputs: in2, outputs: out2 };
}


/* =======================
   ORDERS (BUY/SELL)
   ======================= */

function TRADE_collectOrders_(ctx) {
  var data = ctx.data;
  var stateId = ctx.stateId;
  var provByKey = ctx.provByKey;

  var orders = { buy: [], sell: [] };

  normalizeToArray(data.Постройки).forEach(function (row) {
    normalizeToArray(row).forEach(function (b) {
      if (!b || b.Активно !== true) return;
      if (!b.Провинция) return;

      var provKey = String(b.Провинция);
      var p = provByKey[provKey];
      if (!p) return;

      // only our provinces
      if (String(p.Владелец || "") !== String(stateId)) return;

      var localMarketId = TRADE_getMarketId_(p);
      if (!localMarketId) return;
      localMarketId = String(localMarketId);

      TRADE_ensureBuildingWalletAndStock_(b);

      // init throughput left (buyers spend)
      if (ctx.tpLeftProv[provKey] === undefined) {
        ctx.tpLeftProv[provKey] = Math.floor(Number(p.ПропускнаяСпособность) || 0);
      }

      // ✅ IO ТОЛЬКО у постройки, без умножения на уровень
      var io = TRADE_getBuildingIOMaps_(b);

      // BUY: need = inputs - stock
      Object.keys(io.inputs).forEach(function (g) {
        var needTotal = Math.floor(Number(io.inputs[g]) || 0);
        if (needTotal <= 0) return;

        var inStock = Math.floor(Number(b.Склад[g]) || 0);
        var need = Math.max(0, needTotal - inStock);
        if (need <= 0) return;

        orders.buy.push({
          b: b,
          provKey: provKey,
          localMarketId: localMarketId,
          good: String(g),
          qtyNeed: need
        });
      });

      // SELL: продаём только товары из outputs, которые реально есть на складе
      Object.keys(io.outputs).forEach(function (g) {
        var qty = Math.floor(Number(b.Склад[g]) || 0);
        if (qty <= 0) return;

        orders.sell.push({
          b: b,
          provKey: provKey,
          marketId: localMarketId,
          good: String(g),
          qtySell: qty
        });
      });
    });
  });

  return orders;
}


/* =======================
   CLEARING (building <-> building)
   ======================= */

function TRADE_clearInternalTrades_(ctx) {
  var buyOrders = ctx.orders.buy || [];
  var sellOrders = ctx.orders.sell || [];
  var marketById = ctx.marketById;
  var policyIdx = ctx.policyIdx;
  var tpLeftProv = ctx.tpLeftProv;
  var accessPool = ctx.accessPool;

  // sellers index: marketId -> good -> [sellerOrder]
  var sellers = {};
  for (var i = 0; i < sellOrders.length; i++) {
    var s = sellOrders[i];
    var mid = String(s.marketId);
    var g = String(s.good);

    if (!sellers[mid]) sellers[mid] = {};
    if (!sellers[mid][g]) sellers[mid][g] = [];
    sellers[mid][g].push(s);
  }

  // report
  var rep = {
    buyPlannedUnits: 0,
    buyBoughtUnits: 0,
    buyShortUnits: 0,
    buySpentMoney: 0,

    sellSoldUnits: 0,
    sellEarnedMoney: 0,

    provThroughputSpent: 0,
    accessThroughputSpent: 0,

    tariffsPaid: 0
  };

  // buyers priority: biggest need first
  buyOrders.sort(function (a, b) { return b.qtyNeed - a.qtyNeed; });

  for (var bi = 0; bi < buyOrders.length; bi++) {
    var o = buyOrders[bi];
    var buyer = o.b;
    var provKey = String(o.provKey);
    var localMarketId = String(o.localMarketId);
    var good = String(o.good);

    var need = Math.floor(Number(o.qtyNeed) || 0);
    if (need <= 0) continue;

    rep.buyPlannedUnits += need;

    // province throughput available
    var tpProv = Math.floor(Number(tpLeftProv[provKey]) || 0);

    // 1) LOCAL market first (no tariffs)
    if (need > 0 && tpProv > 0 && sellers[localMarketId] && sellers[localMarketId][good] && sellers[localMarketId][good].length) {
      var gotLocal = TRADE_matchFromSellerList_({
        buyer: buyer,
        need: need,
        sellerList: sellers[localMarketId][good],
        good: good,
        price: TRADE_getPriceFromMarket_(marketById, localMarketId, good),

        // tariffs: none local
        exportTariff: 0,
        importTariff: 0,
        exportMarketId: null,
        importMarketId: null,

        // throughput: province (buyer pays)
        tpKind: "PROV",
        tpAvail: tpProv,
        tpLeftProv: tpLeftProv,
        provKey: provKey,

        // access pool
        accessPool: accessPool,

        // policy accounting
        policyIdx: policyIdx,

        rep: rep
      });

      need -= gotLocal;
      tpProv = Math.floor(Number(tpLeftProv[provKey]) || 0);
    }

    // 2) ACCESS markets (tariffs apply, throughput from accessPool[sourceMarket])
    if (need > 0) {
      var accessKeys = Object.keys(accessPool || {});

      // sort by effective buy price (base + import tariff)
      accessKeys.sort(function (a, b) {
        return TRADE_effectiveBuyPrice_(marketById, policyIdx, localMarketId, a, good) -
               TRADE_effectiveBuyPrice_(marketById, policyIdx, localMarketId, b, good);
      });

      for (var ai = 0; ai < accessKeys.length && need > 0; ai++) {
        var srcMarketId = String(accessKeys[ai]);
        var cap = Math.floor(Number(accessPool[srcMarketId]) || 0);
        if (cap <= 0) continue;

        // policy permissions
        if (!TRADE_policyIsAllowed_(policyIdx, srcMarketId, "Экспорт", good)) continue;
        if (!TRADE_policyIsAllowed_(policyIdx, localMarketId, "Импорт", good)) continue;

        if (!sellers[srcMarketId] || !sellers[srcMarketId][good] || !sellers[srcMarketId][good].length) continue;

        var basePrice = TRADE_getPriceFromMarket_(marketById, srcMarketId, good);

        var exportT = TRADE_policyGetTariff_(policyIdx, srcMarketId, "Экспорт", good); // seller pays
        var importT = TRADE_policyGetTariff_(policyIdx, localMarketId, "Импорт", good); // buyer pays

        var gotExt = TRADE_matchFromSellerList_({
          buyer: buyer,
          need: need,
          sellerList: sellers[srcMarketId][good],
          good: good,
          price: basePrice,

          exportTariff: exportT,
          importTariff: importT,
          exportMarketId: srcMarketId,   // export duty goes to source market
          importMarketId: localMarketId, // import duty goes to destination market

          tpKind: "ACCESS",
          tpAvail: cap,
          tpLeftProv: tpLeftProv,
          provKey: provKey,
          accessPool: accessPool,

          policyIdx: policyIdx,

          rep: rep
        });

        need -= gotExt;
      }
    }

    // 3) Shortage
    if (need > 0) {
      rep.buyShortUnits += need;

      // market shortage stat (if market goods exists)
      var mLocal = marketById[localMarketId];
      if (mLocal && mLocal.Товары && mLocal.Товары[good]) {
        mLocal.Товары[good].Нехватка = (Number(mLocal.Товары[good].Нехватка) || 0) + need;
      }

      // building shortage
      if (!buyer.Нехватка || typeof buyer.Нехватка !== "object" || Array.isArray(buyer.Нехватка)) buyer.Нехватка = {};
      buyer.Нехватка[good] = (Number(buyer.Нехватка[good]) || 0) + need;
    }
  }

  return rep;
}


/* =======================
   MATCH (core): implements your tariff rules
   ======================= */

function TRADE_matchFromSellerList_(ctx) {
  var buyer = ctx.buyer;
  var need = Math.floor(Number(ctx.need) || 0);
  var tpAvail = Math.floor(Number(ctx.tpAvail) || 0);
  if (need <= 0 || tpAvail <= 0) return 0;

  var good = String(ctx.good);
  var price = Number(ctx.price) || 1;
  if (!isFinite(price) || price <= 0) price = 1;

  var exportT = Number(ctx.exportTariff) || 0; // экспорт платит ПРОДАВЕЦ
  var importT = Number(ctx.importTariff) || 0; // импорт платит ПОКУПАТЕЛЬ
  if (exportT < 0) exportT = 0;
  if (importT < 0) importT = 0;

  var bought = 0;

  for (var si = 0; si < ctx.sellerList.length && need > 0 && tpAvail > 0; si++) {
    var s = ctx.sellerList[si];
    var seller = s.b;

    var sq = Math.floor(Number(s.qtySell) || 0);
    if (sq <= 0) continue;

    // Сколько можем купить по деньгам покупателя:
    // "как в реальности": покупатель должен иметь деньги на (база продавцу + импортная пошлина)
    var moneyBuyer = Number(buyer.Деньги) || 0;
    if (moneyBuyer <= 0) break;

    var unitBase = price;
    var unitImportDuty = price * importT;
    var unitTotalBuyer = unitBase + unitImportDuty;
    if (!(unitTotalBuyer > 0)) unitTotalBuyer = 1;

    var maxByMoney = Math.floor(moneyBuyer / unitTotalBuyer);
    if (maxByMoney <= 0) break;

    var can = Math.min(need, tpAvail, sq, maxByMoney);
    can = Math.floor(can);
    if (can <= 0) continue;

    // --- goods transfer ---
    seller.Склад[good] = Math.floor(Number(seller.Склад[good]) || 0) - can;
    if (seller.Склад[good] < 0) seller.Склад[good] = 0;

    buyer.Склад[good] = Math.floor(Number(buyer.Склад[good]) || 0) + can;

    // --- money flow (реальная схема) ---
    var baseCost = can * price;

    // импортная пошлина: покупатель платит "таможне" отдельно
    var importDuty = baseCost * importT;

    // экспортная пошлина: продавец платит "таможне" отдельно
    var exportDuty = baseCost * exportT;

    // 1) Покупатель -> Продавец: только базовая стоимость
    buyer.Деньги = (Number(buyer.Деньги) || 0) - baseCost;
    buyer.РасходЗаХод = (Number(buyer.РасходЗаХод) || 0) + baseCost;

    seller.Деньги = (Number(seller.Деньги) || 0) + baseCost;
    seller.ПрибыльЗаХод = (Number(seller.ПрибыльЗаХод) || 0) + baseCost;

    // 2) Покупатель -> "Таможня": импортная пошлина (деньги уходят из покупателя)
    if (importDuty > 0) {
      buyer.Деньги = (Number(buyer.Деньги) || 0) - importDuty;
      buyer.РасходЗаХод = (Number(buyer.РасходЗаХод) || 0) + importDuty;
      buyer.Пошлины = (Number(buyer.Пошлины) || 0) + importDuty;
    }

    // 3) Продавец -> "Таможня": экспортная пошлина (деньги уходят из продавца)
    if (exportDuty > 0) {
      seller.Деньги = (Number(seller.Деньги) || 0) - exportDuty;
      seller.РасходЗаХод = (Number(seller.РасходЗаХод) || 0) + exportDuty;
      seller.Пошлины = (Number(seller.Пошлины) || 0) + exportDuty;
    }

    // --- tariffs accounting (только статистика отчёта, без записи в политику) ---
    var cross = !!(ctx.exportMarketId && ctx.importMarketId);
    if (cross) {
      ctx.rep.tariffsPaid += (importDuty + exportDuty);
    }

    // --- throughput spends (buyer only) ---
    if (ctx.tpKind === "PROV") {
      ctx.tpLeftProv[ctx.provKey] = Math.floor(Number(ctx.tpLeftProv[ctx.provKey]) || 0) - can;
      if (ctx.tpLeftProv[ctx.provKey] < 0) ctx.tpLeftProv[ctx.provKey] = 0;
      ctx.rep.provThroughputSpent += can;
    } else {
      ctx.accessPool[ctx.exportMarketId] = Math.floor(Number(ctx.accessPool[ctx.exportMarketId]) || 0) - can;
      if (ctx.accessPool[ctx.exportMarketId] < 0) ctx.accessPool[ctx.exportMarketId] = 0;
      ctx.rep.accessThroughputSpent += can;
    }

    // update seller order remaining
    s.qtySell = sq - can;

    // counters
    bought += can;
    ctx.rep.buyBoughtUnits += can;

    // В отчёте “потрачено денег” логично считать базу + импорт (полная цена для покупателя)
    ctx.rep.buySpentMoney += (baseCost + importDuty);

    ctx.rep.sellSoldUnits += can;

    // В отчёте “получено денег (продажи)” можно считать валовую выручку (база),
    // а экспортная пошлина отдельно отражается в tariffsPaid и в seller.РасходЗаХод
    ctx.rep.sellEarnedMoney += baseCost;

    need -= can;
    tpAvail -= can;
  }

  return bought;
}


/* =======================
   PRICE helpers
   ======================= */

function TRADE_getPriceFromMarket_(marketById, marketId, good) {
  var m = marketById ? marketById[String(marketId)] : null;
  if (!m || !m.Товары || !m.Товары[String(good)]) return 1;
  var p = Number(m.Товары[String(good)].Цена);
  if (!isFinite(p) || p <= 0) p = 1;
  return p;
}

// buyer effective price uses ONLY import tariff
function TRADE_effectiveBuyPrice_(marketById, policyIdx, dstMarketId, srcMarketId, good) {
  var base = TRADE_getPriceFromMarket_(marketById, srcMarketId, good);
  var importT = TRADE_policyGetTariff_(policyIdx, dstMarketId, "Импорт", good);
  return base * (1 + importT);
}


/* =======================
   MARKETS: index from column (1 market = 1 cell)
   ======================= */

function TRADE_indexMarkets_(tradeCol) {
  var out = {};
  if (!Array.isArray(tradeCol)) return out;

  // Понимаем 1D и 2D (в 2D берём все ячейки)
  var flat = [];
  for (var r = 0; r < tradeCol.length; r++) {
    var row = tradeCol[r];
    if (Array.isArray(row)) {
      for (var c = 0; c < row.length; c++) flat.push(row[c]);
    } else {
      flat.push(row);
    }
  }

  for (var i = 0; i < flat.length; i++) {
    var cell = flat[i];
    if (!cell || typeof cell !== "object" || Array.isArray(cell)) continue;

    var id = null;
    if (cell["Идентификатор рынка"] != null && String(cell["Идентификатор рынка"]).trim() !== "") id = String(cell["Идентификатор рынка"]).trim();
    else if (cell.РынокId != null && String(cell.РынокId).trim() !== "") id = String(cell.РынокId).trim();
    else if (cell.id != null && String(cell.id).trim() !== "") id = String(cell.id).trim();
    if (!id) continue;

    if (!cell["Товары"] || typeof cell["Товары"] !== "object") cell["Товары"] = {};
    out[id] = cell;
  }

  return out;
}

/* =======================
   TEMPLATES: build index by Type (NOT used for IO)
   ======================= */

function TRADE_buildTemplatesIndexByType_full_(templatesCol) {
  var out = {};
  if (!Array.isArray(templatesCol)) return out;

  for (var r = 0; r < templatesCol.length; r++) {
    var cell = templatesCol[r];
    var list = [];

    if (Array.isArray(cell)) list = cell;
    else if (cell && typeof cell === "object") list = [cell];
    else continue;

    for (var i = 0; i < list.length; i++) {
      var tpl = list[i];
      if (!tpl || typeof tpl !== "object") continue;

      var type = tpl.Тип || tpl["Тип здания"] || tpl.type;
      if (!type) continue;

      var inputs = tpl.Входы || tpl.inputs || {};
      var outputs = tpl.Выходы || tpl.outputs || {};

      var in2 = {};
      if (inputs && typeof inputs === "object" && !Array.isArray(inputs)) {
        Object.keys(inputs).forEach(function (g) {
          var v = Number(inputs[g]);
          if (!isFinite(v)) v = 0;
          if (v > 0) in2[String(g)] = v;
        });
      }

      var out2 = {};
      if (outputs && typeof outputs === "object" && !Array.isArray(outputs)) {
        Object.keys(outputs).forEach(function (g) {
          var v2 = Number(outputs[g]);
          if (!isFinite(v2)) v2 = 0;
          if (v2 > 0) out2[String(g)] = v2;
        });
      }

      out[String(type)] = { inputs: in2, outputs: out2 };
    }
  }

  return out;
}


/* =======================
   POLICY: data["Торговая политика государств"]
   ======================= */

function TRADE_buildPolicyIndex_(data) {
  if (!Array.isArray(data["Торговая политика государств"])) data["Торговая политика государств"] = [];
  var col = data["Торговая политика государств"];

  // flatten 1D/2D
  var flat = [];
  for (var r = 0; r < col.length; r++) {
    var row = col[r];
    if (Array.isArray(row)) {
      for (var c = 0; c < row.length; c++) flat.push({ r: r, c: c, cell: row[c] });
    } else {
      flat.push({ r: r, c: 0, cell: row });
    }
  }

  var idx = { col: col, byMarket: {} };

  for (var i = 0; i < flat.length; i++) {
    var cell = flat[i].cell;
    if (!cell || typeof cell !== "object" || Array.isArray(cell)) continue;

    var m = cell["Рынок"] || cell["Идентификатор рынка"] || cell.РынокId || cell.id;
    if (!m) continue;
    var mid = String(m).trim();
    if (!mid) continue;

    TRADE_policyEnsureShape_(cell, mid);
    idx.byMarket[mid] = { rowIndex: flat[i].r, colIndex: flat[i].c, obj: cell };
  }

  return idx;
}

function TRADE_policyEnsureShape_(obj, marketId) {
  if (!obj["Рынок"]) obj["Рынок"] = String(marketId);

  if (!obj["Тарифы"] || typeof obj["Тарифы"] !== "object") obj["Тарифы"] = {};
  if (!obj["Тарифы"]["Импорт"] || typeof obj["Тарифы"]["Импорт"] !== "object") obj["Тарифы"]["Импорт"] = { default: 0 };
  if (!obj["Тарифы"]["Экспорт"] || typeof obj["Тарифы"]["Экспорт"] !== "object") obj["Тарифы"]["Экспорт"] = { default: 0 };

  if (!obj["Разрешено"] || typeof obj["Разрешено"] !== "object") obj["Разрешено"] = {};
  if (!obj["Разрешено"]["Импорт"] || typeof obj["Разрешено"]["Импорт"] !== "object") obj["Разрешено"]["Импорт"] = { default: true };
  if (!obj["Разрешено"]["Экспорт"] || typeof obj["Разрешено"]["Экспорт"] !== "object") obj["Разрешено"]["Экспорт"] = { default: true };

  if (obj["ПошлиныЗаХод"] == null || isNaN(Number(obj["ПошлиныЗаХод"]))) obj["ПошлиныЗаХод"] = 0;
  if (obj["ПошлиныИтого"] == null || isNaN(Number(obj["ПошлиныИтого"]))) obj["ПошлиныИтого"] = 0;
}

function TRADE_policyEnsureAllMarketsHaveRows_(data, marketById, policyIdx) {
  var col = policyIdx.col;

  Object.keys(marketById).forEach(function (mid) {
    if (policyIdx.byMarket[mid]) return;

    var obj = {
      "Рынок": String(mid),
      "Тарифы": { "Импорт": { default: 0 }, "Экспорт": { default: 0 } },
      "Разрешено": { "Импорт": { default: true }, "Экспорт": { default: true } },
      "ПошлиныЗаХод": 0,
      "ПошлиныИтого": 0
    };

    var r = TRADE_putPolicyRowToFirstFreeCell_(col, obj);
    policyIdx.byMarket[mid] = { rowIndex: r, colIndex: 0, obj: obj };
  });
}

function TRADE_policyCleanupOrphans_(data, marketById, policyIdx) {
  var col = policyIdx.col;
  for (var r = 0; r < col.length; r++) {
    var cell = col[r];
    if (!cell || typeof cell !== "object" || Array.isArray(cell)) continue;
    var mid = String(cell["Рынок"] || "").trim();
    if (!mid) continue;
    if (marketById[mid]) continue;
    col[r] = ""; // освобождаем строку
  }
}

function TRADE_resetPolicyTurnCounters_(policyIdx) {
  Object.keys(policyIdx.byMarket).forEach(function (mid) {
    var obj = policyIdx.byMarket[mid].obj;
    obj["ПошлиныЗаХод"] = 0;
  });
}

function TRADE_policyGetTariff_(policyIdx, marketId, kind, good) {
  var row = policyIdx.byMarket[String(marketId)];
  if (!row) return 0;
  var obj = row.obj;
  TRADE_policyEnsureShape_(obj, marketId);

  var map = obj["Тарифы"][String(kind)] || {};
  var g = String(good);
  var v = map[g];
  if (v == null || v === "") v = map["default"];
  v = Number(v);
  if (!isFinite(v) || v < 0) v = 0;
  return v;
}

function TRADE_policyIsAllowed_(policyIdx, marketId, kind, good) {
  var row = policyIdx.byMarket[String(marketId)];
  if (!row) return true;
  var obj = row.obj;
  TRADE_policyEnsureShape_(obj, marketId);

  var map = obj["Разрешено"][String(kind)] || {};
  var g = String(good);
  var v = map[g];
  if (v == null || v === "") v = map["default"];
  return v !== false;
}

function TRADE_policyAddTariffIncome_(policyIdx, marketId, amount) {
  var a = Number(amount) || 0;
  if (!(a > 0)) return;

  var row = policyIdx.byMarket[String(marketId)];
  if (!row) return;

  var obj = row.obj;
  obj["ПошлиныЗаХод"] = (Number(obj["ПошлиныЗаХод"]) || 0) + a;
  obj["ПошлиныИтого"] = (Number(obj["ПошлиныИтого"]) || 0) + a;
}


/* =======================
   ACCESS: from data["Данные государства"]
   ======================= */

function TRADE_getStateAccessMarketsPool_(data, marketById) {
  var pool = {};
  var root = data["Данные государства"];
  var flat = TRADE_flatten2D_(root);

  var accessContainer = null; // ссылка на объект/массив внутри "Данные государства"

  for (var i = 0; i < flat.length; i++) {
    var o = flat[i];
    if (o && typeof o === "object" && !Array.isArray(o) && Object.prototype.hasOwnProperty.call(o, "РынкиДоступа")) {
      accessContainer = o["РынкиДоступа"];
      break;
    }
  }

  if (!accessContainer) return { pool: pool, container: null };

  // Считываем в pool (рабочая копия)
  if (Array.isArray(accessContainer)) {
    for (var j = 0; j < accessContainer.length; j++) {
      var it = accessContainer[j];
      if (!it || typeof it !== "object") continue;
      var mid = it["Рынок"] || it["Идентификатор рынка"] || it.РынокId || it.id;
      if (!mid) continue;
      mid = String(mid).trim();
      if (!mid || !marketById[mid]) continue;

      var cap = Number(it["ПропускнаяСпособность"] != null ? it["ПропускнаяСпособность"] : it["cap"]);
      if (!isFinite(cap) || cap <= 0) continue;

      pool[mid] = Math.floor(cap);
    }
  } else if (accessContainer && typeof accessContainer === "object") {
    Object.keys(accessContainer).forEach(function (mid) {
      var m = String(mid).trim();
      if (!m || !marketById[m]) return;

      var cap2 = Number(accessContainer[mid]);
      if (!isFinite(cap2) || cap2 <= 0) return;

      pool[m] = Math.floor(cap2);
    });
  }

  return { pool: pool, container: accessContainer };
}


/* =======================
   STATE ID: from data["Данные государства"]
   ======================= */

function TRADE_getStateId_(data) {
  var root = data ? data["Данные государства"] : null;
  var flat = TRADE_flatten2D_(root);
  for (var i = 0; i < flat.length; i++) {
    var obj = flat[i];
    if (obj && typeof obj === "object" && !Array.isArray(obj)) {
      if (Object.prototype.hasOwnProperty.call(obj, "Идентификатор государства")) {
        var s = String(obj["Идентификатор государства"]).trim();
        if (s !== "") return s;
      }
    }
  }
  return null;
}

function TRADE_flatten2D_(v) {
  var out = [];
  normalizeToArray(v).forEach(function (row) {
    normalizeToArray(row).forEach(function (cell) { out.push(cell); });
  });
  return out;
}


/* =======================
   PROVINCES helpers
   ======================= */

function TRADE_getAllProvincesFlat_(data) {
  if (!Array.isArray(data.Провинции)) return [];
  return normalizeToArray(data.Провинции)
    .reduce(function (acc, row) { return acc.concat(normalizeToArray(row)); }, [])
    .filter(function (p) { return p && typeof p === "object" && TRADE_getProvKey_(p); });
}

function TRADE_getProvKey_(p) {
  return p.Провинция || p.Название || p.id || null;
}

function TRADE_getMarketId_(p) {
  if (!p || typeof p !== "object") return null;
  if (p.РынокId != null && String(p.РынокId).trim() !== "") return String(p.РынокId).trim();
  if (p.Рынок != null && String(p.Рынок).trim() !== "") return String(p.Рынок).trim();
  return null;
}


/* =======================
   NEWS: summary (Vic3 tooltip style, single dark-gray color)
   ======================= */

function TRADE_pushCommerceSummaryNotice_RU_(data, rep) {
  if (typeof pushNotice !== "function") return;

  var C = "#6E675F";
  function line(parts, s) { parts.push({ text: String(s) + "\n", color: C }); }

  var parts = [];
  line(parts, "Торговля: клиринг между зданиями (без запасов рынков)");
  line(parts, "┌────────────────────────────────────────────────────────┐");
  line(parts, "┃ ➔ План закупок (ед.): " + String(rep.buyPlannedUnits));
  line(parts, "┃ ➔ Куплено (ед.): " + String(rep.buyBoughtUnits));
  line(parts, "┃ ➔ Нехватка (ед.): " + String(rep.buyShortUnits));
  line(parts, "┃");
  line(parts, "┃ ➔ Потрачено денег: " + TRADE_fmtMoney_(rep.buySpentMoney));
  line(parts, "┃ ➔ Получено денег (продажи): " + TRADE_fmtMoney_(rep.sellEarnedMoney));
  line(parts, "┃ ➔ Уплачено пошлин: " + TRADE_fmtMoney_(rep.tariffsPaid));
  line(parts, "┃");
  line(parts, "┃ ➔ Throughput провинций (списано): " + String(rep.provThroughputSpent));
  line(parts, "┃ ➔ Throughput доступа (списано): " + String(rep.accessThroughputSpent));
  line(parts, "└────────────────────────────────────────────────────────┘");

  pushNotice(data, {
    category: "Торговля",
    sub: "Клиринг",
    priority: 92,
    parts: parts
  });
}

function TRADE_fmtMoney_(x) {
  var n = Number(x) || 0;
  n = Math.round(n * 100) / 100;
  var s = String(n);
  var parts = s.split(".");
  parts[0] = parts[0].replace(/\B(?=(\d{3})+(?!\d))/g, " ");
  return parts.join(".");
}

function TRADE_pushErr_(data, category, sub, msg) {
  if (typeof pushNotice !== "function") return;
  pushNotice(data, {
    category: category || "Торговля",
    sub: sub || "Ошибка",
    priority: 999,
    parts: [
      { text: String(msg || "Ошибка") + "\n", bold: true, color: "#E36A6A" },
      { text: "┌────────────────────────────────────────────────────────┐\n", color: "#E36A6A" },
      { text: "┃ ➔ " + String(msg || "Неизвестная ошибка") + "\n", color: "#E36A6A" },
      { text: "└────────────────────────────────────────────────────────┘\n", color: "#E36A6A" }
    ]
  });
}

/* =======================
   Хелперы
   ======================= */
function TRADE_putPolicyRowToFirstFreeCell_(col, obj) {
  // Ищем пустое место
  for (var r = 0; r < col.length; r++) {
    var row = col[r];

    // 2D
    if (Array.isArray(row)) {
      if (row.length === 0 || row[0] == null || row[0] === "") {
        row[0] = obj;
        return r;
      }
    } 
    // 1D
    else {
      if (row == null || row === "") {
        col[r] = obj;
        return r;
      }
    }
  }

  // Если пустых нет — добавляем в конец
  if (col.length > 0 && Array.isArray(col[0])) {
    col.push([obj]);          // 2D
    return col.length - 1;
  } else {
    col.push(obj);            // 1D
    return col.length - 1;
  }
}

function TRADE_writeBackAccessMarketsPool_(accessContainer, accessPool) {
  if (!accessContainer) return;

  // Массивный формат: [{Рынок:"...", ПропускнаяСпособность: N}, ...]
  if (Array.isArray(accessContainer)) {
    for (var i = 0; i < accessContainer.length; i++) {
      var it = accessContainer[i];
      if (!it || typeof it !== "object") continue;

      var mid = it["Рынок"] || it["Идентификатор рынка"] || it.РынокId || it.id;
      if (!mid) continue;
      mid = String(mid).trim();
      if (!mid) continue;

      if (Object.prototype.hasOwnProperty.call(accessPool, mid)) {
        it["ПропускнаяСпособность"] = Math.floor(Number(accessPool[mid]) || 0);
      }
    }
    return;
  }

  // Объектный формат: {"РынокA": N, "РынокB": M}
  if (typeof accessContainer === "object") {
    Object.keys(accessPool || {}).forEach(function (mid) {
      accessContainer[mid] = Math.floor(Number(accessPool[mid]) || 0);
    });
  }
}

/* =========================================================
   STATE: National Market (flat field)
   data["Данные государства"] хранит:
     { ..., "Национальный рынок": "MARKET_ID", ... }

   Правило:
   1) Если "Национальный рынок" задан -> все провинции государства в этот рынок
   2) Если не задан -> ставим "Национальный рынок" = stateId, затем переводим
   Защита:
   - не переписываем p.РынокId/p.Рынок, если уже совпадает
   Требует: normalizeToArray(value)
   ========================================================= */

function STATE_applyNationalMarket_Flat_(data) {
  if (!data || typeof data !== "object") return data;

  var stateId = TRADE_getStateId_(data);
  if (!stateId) return data;
  stateId = String(stateId);

  // 1) нормализуем "Данные государства" в 2D
  var dg = data["Данные государства"];
  if (!Array.isArray(dg)) {
    data["Данные государства"] = dg != null ? [[dg]] : [[]];
    dg = data["Данные государства"];
  }

  // 2) находим контейнер-объект, где живёт (или будет жить) ключ "Национальный рынок"
  var container = STATE_findStateContainerForNationalMarket_Flat_(dg);

  // 3) читаем/назначаем target рынок
  var target = null;
  if (container["Национальный рынок"] != null && String(container["Национальный рынок"]).trim() !== "") {
    target = String(container["Национальный рынок"]).trim();
  }

  if (!target) {
    target = stateId;
    container["Национальный рынок"] = target;
  }

  // 4) переводим провинции (с защитой от лишних записей)
  var changed = 0;
  var totalOur = 0;

  if (Array.isArray(data.Провинции)) {
    normalizeToArray(data.Провинции).forEach(function (row) {
      normalizeToArray(row).forEach(function (p) {
        if (!p || typeof p !== "object") return;
        if (String(p.Владелец || "") !== String(stateId)) return;

        totalOur++;

        var curId = (p.РынокId != null) ? String(p.РынокId).trim() : "";
        var curNm = (p.Рынок != null) ? String(p.Рынок).trim() : "";

        // если уже в нужном рынке — ничего не делаем
        if (curId === target && curNm === target) return;
        if (curId === target && !curNm) { p.Рынок = target; changed++; return; }
        if (curNm === target && !curId) { p.РынокId = target; changed++; return; }

        // иначе обновляем оба поля (унификация)
        p.РынокId = target;
        p.Рынок = target;
        changed++;
      });
    });
  }

  // 5) (optional) новость
  if (typeof pushNotice === "function") {
    var C = "#6E675F";
    var parts = [];
    parts.push({ text: "Национальный рынок\n", color: C });
    parts.push({ text: "┌────────────────────────────────────────────────────────┐\n", color: C });
    parts.push({ text: "┃ ➔ Государство: " + stateId + "\n", color: C });
    parts.push({ text: "┃ ➔ Рынок: " + target + "\n", color: C });
    parts.push({ text: "┃ ➔ Наших провинций: " + String(totalOur) + "\n", color: C });
    parts.push({ text: "┃ ➔ Изменено: " + String(changed) + "\n", color: C });
    parts.push({ text: "└────────────────────────────────────────────────────────┘\n", color: C });

    pushNotice(data, {
      category: "Политика",
      sub: "Национальный рынок",
      priority: 90,
      parts: parts
    });
  }

  return data;
}

/* =======================
   Helper: find/create container object in data["Данные государства"]
   (flat national market key)
   Priority:
   1) объект, где уже есть ключ "Национальный рынок"
   2) объект, где есть "Идентификатор государства"
   3) первый попавшийся объект
   4) создать новый объект и вставить в первую свободную ячейку
   ======================= */

function STATE_findStateContainerForNationalMarket_Flat_(dg2d) {
  var flat = [];
  for (var r = 0; r < dg2d.length; r++) {
    var row = dg2d[r];
    if (Array.isArray(row)) {
      for (var c = 0; c < row.length; c++) flat.push({ r: r, c: c, cell: row[c] });
    } else {
      flat.push({ r: r, c: 0, cell: row });
    }
  }

  // 1) where key exists
  for (var i = 0; i < flat.length; i++) {
    var cell = flat[i].cell;
    if (cell && typeof cell === "object" && !Array.isArray(cell) &&
        Object.prototype.hasOwnProperty.call(cell, "Национальный рынок")) {
      return cell;
    }
  }

  // 2) where state id exists (most canonical container)
  for (var j = 0; j < flat.length; j++) {
    var cell2 = flat[j].cell;
    if (cell2 && typeof cell2 === "object" && !Array.isArray(cell2) &&
        Object.prototype.hasOwnProperty.call(cell2, "Идентификатор государства")) {
      return cell2;
    }
  }

  // 3) first object
  for (var k = 0; k < flat.length; k++) {
    var cell3 = flat[k].cell;
    if (cell3 && typeof cell3 === "object" && !Array.isArray(cell3)) return cell3;
  }

  // 4) create new object and insert into first free cell
  var obj = {};
  for (var r2 = 0; r2 < dg2d.length; r2++) {
    var row2 = dg2d[r2];

    if (Array.isArray(row2)) {
      for (var c2 = 0; c2 < row2.length; c2++) {
        if (row2[c2] == null || row2[c2] === "") {
          row2[c2] = obj;
          return obj;
        }
      }
      if (row2.length === 0) {
        row2[0] = obj;
        return obj;
      }
    } else {
      if (row2 == null || row2 === "") {
        dg2d[r2] = obj;
        return obj;
      }
    }
  }

  if (dg2d.length > 0 && Array.isArray(dg2d[0])) dg2d.push([obj]);
  else dg2d.push(obj);

  return obj;
}