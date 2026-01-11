/* =========================================================
   TRADE COMMERCE MODULE (без синхронизации рынков)
   =========================================================
   Данные ожидаются:
   - data.Торговля: столбец, 1 рынок = 1 ячейка (объект с "Идентификатор рынка", "Товары")
   - data.Постройки: здания (Активно, Провинция, Тип, Уровень)
   - data.Провинции: провинции (Владелец, РынокId/Рынок, ПропускнаяСпособность)
   - data["Шаблоны зданий"]: шаблоны (Тип, Входы, Выходы)
   - data["Данные государства"]: содержит {"Идентификатор государства": ...}
     и опционально {"Доступ к рынкам": { marketId: cap, ... }}
   - data["Торговая политика государств"]: по строке на рынок (создаём/чистим)
   ========================================================= */

/* =======================
   НАСТРОЙКИ
   ======================= */

var TRADE_STATE_ACCESS_KEY = "Доступ к рынкам";
var TRADE_POLICY_COL = "Торговая политика государств";

var TRADE_SELECT_BY_EFFECTIVE_PRICE = true;
var TRADE_ADD_DEMAND_ON_BUY = true;
var TRADE_ADD_SUPPLY_ON_SELL = true;
var TRADE_ENFORCE_STOCK = true; // важно: если true, покупать можно только при Запас>0

var TRADE_TRADE_STATS_TOP_MARKETS = 10;
var TRADE_TRADE_STATS_TOP_GOODS = 10;

/* =========================================================
   ОСНОВНАЯ ФУНКЦИЯ ХОДА: закупка/продажа зданий
   ========================================================= */

function TRADE_runBuildingCommerceEachTurn(data) {
  if (!data || typeof data !== "object") return data;

  if (!Array.isArray(data.Торговля)) data.Торговля = [];
  if (!Array.isArray(data.Постройки)) data.Постройки = [];

  if (typeof ensureNews === "function") ensureNews(data);

  // --- stateId ---
  var stateId = TRADE_getStateId_(data);
  if (!stateId) return data;
  stateId = String(stateId);

  // --- provinces ---
  var provinces = TRADE_getAllProvincesFlat_(data);
  var provByKey = {};
  for (var i = 0; i < provinces.length; i++) {
    var p = provinces[i];
    var pk = TRADE_getProvKey_(p);
    if (pk) provByKey[String(pk)] = p;
  }

  // --- markets ---
  var marketById = TRADE_indexMarkets_(data.Торговля);

  // --- templates ---
  var tplByType = TRADE_buildTemplatesIndexByType_full_(data["Шаблоны зданий"]);

  // --- policy index (by reference) ---
  var policyIdx = TRADE_buildPolicyIndex_(data);

  // ✅ 1) авто-создание политик для всех РЕАЛЬНО существующих рынков
  var createdPolicies = TRADE_policyEnsureAllMarketsHaveRows_(data, marketById, policyIdx);

  // ✅ 2) удаление политик рынков, которых больше нет в data.Торговля
  var removedPolicies = TRADE_policyCleanupOrphans_(data, marketById, policyIdx);

  // ✅ 3) сброс пошлин за ход
  TRADE_resetPolicyTurnCounters_(policyIdx);

  // --- access pool (mutable) ---
  var accessRef = TRADE_getStateAccessMarketsPool_(data, marketById);
  var accessPool = accessRef.pool;

  // --- local throughput left ---
  var tpLeftProv = {}; // provKey -> capLeft

  // --- stats containers ---
  var buildingStats = [];
  var tradeAgg = TRADE_makeTradeAgg_();

  // --- summary ---
  var rep = {
    buildingsTouched: 0,

    buyPlannedUnits: 0,
    buyBoughtUnits: 0,
    buyShortUnits: 0,
    buySpentMoney: 0,

    sellSoldUnits: 0,
    sellEarnedMoney: 0,

    provThroughputSpent: 0,
    accessThroughputSpent: 0,

    tariffsPaid: 0,

    accessPoolStart: TRADE_sumPool_(accessPool),
    accessPoolEnd: 0,

    policiesCreated: createdPolicies,
    policiesRemoved: removedPolicies
  };

  // === iterate buildings ===
  normalizeToArray(data.Постройки).forEach(function (row) {
    normalizeToArray(row).forEach(function (b) {
      if (!b || b.Активно !== true) return;
      if (!b.Провинция || !b.Тип) return;

      var provKey = String(b.Провинция);
      var p = provByKey[provKey];
      if (!p) return;

      // only our provinces
      if (String(p.Владелец || "") !== stateId) return;

      // local market
      var localMarketId = TRADE_getMarketId_(p);
      if (!localMarketId) return;
      localMarketId = String(localMarketId);

      // market must exist (since sync is external)
      if (!marketById[localMarketId]) return;

      // ensure building stock/wallet
      TRADE_ensureBuildingWalletAndStock_(b);
      TRADE_resetBuildingTurnStats_(b);

      // init local throughput left
      if (tpLeftProv[provKey] === undefined) tpLeftProv[provKey] = Math.floor(Number(p.ПропускнаяСпособность) || 0);

      // template
      var type = String(b.Тип);
      var tpl = tplByType[type];
      if (!tpl) {
        TRADE_pushBuildingToStats_(buildingStats, b);
        return;
      }

      var level = Number(b.Уровень);
      if (isNaN(level) || level <= 0) level = 1;

      rep.buildingsTouched++;

      // BUY inputs -> stock
      if (tpl.inputs && Object.keys(tpl.inputs).length) {
        TRADE_buyInputsForBuilding_({
          b: b,
          level: level,
          tpl: tpl,

          localMarketId: localMarketId,
          marketById: marketById,

          tpLeftProv: tpLeftProv,
          accessPool: accessPool,

          policyIdx: policyIdx,

          rep: rep,
          tradeAgg: tradeAgg
        });
      }

      // SELL from stock
      TRADE_sellFromBuildingStock_({
        b: b,
        tpl: tpl,

        localMarketId: localMarketId,
        marketById: marketById,

        tpLeftProv: tpLeftProv,
        accessPool: accessPool,

        policyIdx: policyIdx,

        rep: rep,
        tradeAgg: tradeAgg
      });

      // write back remaining local throughput
      p.ПропускнаяСпособность = tpLeftProv[provKey];

      // collect building stats
      TRADE_pushBuildingToStats_(buildingStats, b);
    });
  });

  rep.accessPoolEnd = TRADE_sumPool_(accessPool);

  // === notices ===
  TRADE_pushCommerceSummaryNotice_RU_(data, rep);
  TRADE_pushBuildingTopNotice_RU_(data, buildingStats);
  TRADE_pushTradeStatsNotice_RU_(data, tradeAgg, policyIdx);
  TRADE_pushStateTradeIntelNotice_RU_(data, tradeAgg);

  return data;
}

/* =========================================================
   BUY: inputs -> stock (P1 + двойные тарифы)
   ========================================================= */

function TRADE_buyInputsForBuilding_(ctx) {
  var b = ctx.b;
  var level = ctx.level;
  var tpl = ctx.tpl;

  Object.keys(tpl.inputs).forEach(function (g) {
    var perLevel = Number(tpl.inputs[g]);
    if (isNaN(perLevel) || perLevel <= 0) return;

    var needTotal = Math.floor(perLevel * level);
    if (needTotal <= 0) return;

    var inStock = Math.floor(Number(b.Склад[g]) || 0);
    var need = Math.max(0, needTotal - inStock);
    if (need <= 0) return;

    ctx.rep.buyPlannedUnits += need;

    // demand on local market
    if (TRADE_ADD_DEMAND_ON_BUY) {
      var m0 = ctx.marketById[ctx.localMarketId];
      if (m0 && m0.Товары && m0.Товары[g]) {
        m0.Товары[g].Спрос = (Number(m0.Товары[g].Спрос) || 0) + need;
      }
    }

    while (need > 0) {
      var pick = TRADE_pickMarketForBuy_({
        good: g,
        localMarketId: ctx.localMarketId,
        marketById: ctx.marketById,
        tpLeftProv: ctx.tpLeftProv,
        accessPool: ctx.accessPool,
        provKey: String(b.Провинция),
        money: Number(b.Деньги) || 0,

        policyIdx: ctx.policyIdx,
        tradeAgg: ctx.tradeAgg
      });

      if (!pick) break;

      var m = pick.market;
      var st = m.Товары[g];
      if (!st) break;

      var basePrice = Number(st.Цена);
      if (!isFinite(basePrice) || basePrice <= 0) basePrice = 1;

      var moneyAvail = Number(b.Деньги) || 0;
      var tpAvail = Math.floor(Number(pick.tpAvail) || 0);
      if (moneyAvail <= 0 || tpAvail <= 0) break;

      var stock = Math.floor(Number(st.Запас) || 0);
      if (TRADE_ENFORCE_STOCK && stock <= 0) break;

      // P1 BUY: база = цена ИСТОЧНИКА
      var exportT = 0, importT = 0;
      if (pick.source === "ACCESS") {
        exportT = TRADE_policyGetTariff_(ctx.policyIdx, pick.marketId, "Экспорт", g);
        importT = TRADE_policyGetTariff_(ctx.policyIdx, ctx.localMarketId, "Импорт", g);
      }
      var effPrice = basePrice * (1 + exportT + importT);
      if (!(effPrice > 0)) effPrice = 1;

      var maxByMoney = Math.floor(moneyAvail / effPrice);
      if (maxByMoney <= 0) break;

      var can = Math.min(need, tpAvail, maxByMoney);
      if (TRADE_ENFORCE_STOCK) can = Math.min(can, stock);
      can = Math.floor(can);
      if (can <= 0) break;

      // apply market
      if (TRADE_ENFORCE_STOCK) st.Запас = stock - can;
      st.Куплено = (Number(st.Куплено) || 0) + can;

      // money
      var baseCost = can * basePrice;
      var exportTariff = baseCost * exportT;
      var importTariff = baseCost * importT;
      var tariffs = exportTariff + importTariff;
      var totalCost = baseCost + tariffs;

      b.Деньги = (Number(b.Деньги) || 0) - totalCost;
      b.РасходЗаХод = (Number(b.РасходЗаХод) || 0) + totalCost;

      // tariffs income to markets
      if (pick.source === "ACCESS" && tariffs > 0) {
        TRADE_policyAddTariffIncome_(ctx.policyIdx, pick.marketId, exportTariff);
        TRADE_policyAddTariffIncome_(ctx.policyIdx, ctx.localMarketId, importTariff);
        ctx.rep.tariffsPaid += tariffs;
      }

      // throughput (1 per unit)
      if (pick.source === "LOCAL") {
        ctx.tpLeftProv[String(b.Провинция)] = (ctx.tpLeftProv[String(b.Провинция)] || 0) - can;
        ctx.rep.provThroughputSpent += can;
      } else {
        ctx.accessPool[pick.marketId] = (ctx.accessPool[pick.marketId] || 0) - can;
        ctx.rep.accessThroughputSpent += can;
      }

      // stock to building
      b.Склад[g] = Math.floor(Number(b.Склад[g]) || 0) + can;

      // rep
      ctx.rep.buyBoughtUnits += can;
      ctx.rep.buySpentMoney += totalCost;

      // agg
      TRADE_recordTrade_(ctx.tradeAgg, "BUY", pick.source, pick.marketId, g, can, totalCost, can, tariffs);

      if (pick.source === "ACCESS") {
        var pAgg = TRADE_ensurePartnerAgg_(ctx.tradeAgg, pick.marketId);
        pAgg.importsUnits += can;
        pAgg.importsCost += totalCost;
        pAgg.tariffs += tariffs;

        TRADE_recordFlow_(ctx.tradeAgg, pick.marketId, ctx.localMarketId, can, baseCost, tariffs);

        var lAgg = TRADE_ensureLocalAgg_(ctx.tradeAgg, ctx.localMarketId);
        lAgg.importsCost += totalCost;
        var lp = TRADE_ensureLocalPartnerAgg_(lAgg, pick.marketId);
        lp.importsCost += totalCost;
      }

      need -= can;
    }

    if (need > 0) {
      ctx.rep.buyShortUnits += need;

      var mLocal = ctx.marketById[ctx.localMarketId];
      if (mLocal && mLocal.Товары && mLocal.Товары[g]) {
        mLocal.Товары[g].Нехватка = (Number(mLocal.Товары[g].Нехватка) || 0) + need;
      }

      TRADE_recordShortage_(ctx.tradeAgg, ctx.localMarketId, g, need);
    }
  });
}

function TRADE_pickMarketForBuy_(ctx) {
  var g = ctx.good;
  var candidates = [];

  // local
  var m0 = ctx.marketById[ctx.localMarketId];
  if (m0 && m0.Товары && m0.Товары[g]) {
    var tp0 = Math.floor(Number(ctx.tpLeftProv[ctx.provKey]) || 0);
    candidates.push({
      marketId: String(ctx.localMarketId),
      market: m0,
      source: "LOCAL",
      tpAvail: tp0,
      effPrice: TRADE_marketPriceSafe_(m0, g)
    });
  }

  // access
  Object.keys(ctx.accessPool || {}).forEach(function (mid) {
    var cap = Math.floor(Number(ctx.accessPool[mid]) || 0);
    if (cap <= 0) return;

    var m = ctx.marketById[String(mid)];
    if (!m || !m.Товары || !m.Товары[g]) return;

    if (!TRADE_policyIsAllowed_(ctx.policyIdx, String(mid), "Экспорт", g)) {
      if (ctx.tradeAgg) TRADE_recordPolicyBlock_(ctx.tradeAgg, "BUY", String(mid), g);
      return;
    }
    if (!TRADE_policyIsAllowed_(ctx.policyIdx, String(ctx.localMarketId), "Импорт", g)) {
      if (ctx.tradeAgg) TRADE_recordPolicyBlock_(ctx.tradeAgg, "BUY", String(ctx.localMarketId), g);
      return;
    }

    var basePrice = TRADE_marketPriceSafe_(m, g);
    var exportT = TRADE_policyGetTariff_(ctx.policyIdx, String(mid), "Экспорт", g);
    var importT = TRADE_policyGetTariff_(ctx.policyIdx, String(ctx.localMarketId), "Импорт", g);
    var effPrice = basePrice * (1 + exportT + importT);

    candidates.push({
      marketId: String(mid),
      market: m,
      source: "ACCESS",
      tpAvail: cap,
      effPrice: effPrice
    });
  });

  var money = Number(ctx.money) || 0;

  candidates = candidates.filter(function (c) {
    if (!(c.tpAvail > 0)) return false;
    var st = c.market.Товары[g];
    if (!st) return false;

    if (TRADE_ENFORCE_STOCK) {
      var stock = Math.floor(Number(st.Запас) || 0);
      if (stock <= 0) return false;
    }

    var ep = Number(c.effPrice);
    if (!isFinite(ep) || ep <= 0) ep = 1;
    return money >= ep;
  });

  if (!candidates.length) return null;
  if (!TRADE_SELECT_BY_EFFECTIVE_PRICE) return candidates[0];

  candidates.sort(function (a, b) { return (a.effPrice || 1) - (b.effPrice || 1); });
  return candidates[0];
}

/* =========================================================
   SELL: stock -> markets (P1 + двойные тарифы)
   ========================================================= */

function TRADE_sellFromBuildingStock_(ctx) {
  var b = ctx.b;
  var tpl = ctx.tpl;

  if (!b.Склад || typeof b.Склад !== "object") return;

  var sellGoods = {};
  if (tpl.outputs && Object.keys(tpl.outputs).length) {
    Object.keys(tpl.outputs).forEach(function (g) { sellGoods[g] = true; });
  } else {
    Object.keys(b.Склад).forEach(function (g) { sellGoods[g] = true; });
  }

  Object.keys(sellGoods).forEach(function (g) {
    var qty = Math.floor(Number(b.Склад[g]) || 0);
    if (qty <= 0) return;

    while (qty > 0) {
      var pick = TRADE_pickMarketForSell_({
        good: g,
        localMarketId: ctx.localMarketId,
        marketById: ctx.marketById,
        tpLeftProv: ctx.tpLeftProv,
        accessPool: ctx.accessPool,
        provKey: String(b.Провинция),

        policyIdx: ctx.policyIdx,
        tradeAgg: ctx.tradeAgg
      });

      if (!pick) break;

      var m = pick.market;
      var st = m.Товары[g];
      if (!st) break;

      // P1 SELL: база = цена НАЗНАЧЕНИЯ
      var basePrice = TRADE_marketPriceSafe_(m, g);

      var tpAvail = Math.floor(Number(pick.tpAvail) || 0);
      if (tpAvail <= 0) break;

      var can = Math.min(qty, tpAvail);
      can = Math.floor(can);
      if (can <= 0) break;

      var exportT = 0, importT = 0;
      if (pick.source === "ACCESS") {
        exportT = TRADE_policyGetTariff_(ctx.policyIdx, ctx.localMarketId, "Экспорт", g);
        importT = TRADE_policyGetTariff_(ctx.policyIdx, pick.marketId, "Импорт", g);
      }

      var gross = can * basePrice;
      var exportTariff = gross * exportT;
      var importTariff = gross * importT;
      var tariffs = exportTariff + importTariff;
      var net = gross - tariffs;
      if (net < 0) net = 0;

      // building stock decreases
      b.Склад[g] = Math.floor(Number(b.Склад[g]) || 0) - can;

      // market stock increases
      st.Запас = Math.floor(Number(st.Запас) || 0) + can;
      st.Продано = (Number(st.Продано) || 0) + can;
      if (TRADE_ADD_SUPPLY_ON_SELL) st.Предложение = (Number(st.Предложение) || 0) + can;

      // money + stats
      b.Деньги = (Number(b.Деньги) || 0) + net;
      b.ПрибыльЗаХод = (Number(b.ПрибыльЗаХод) || 0) + net;

      // tariffs income
      if (pick.source === "ACCESS" && tariffs > 0) {
        TRADE_policyAddTariffIncome_(ctx.policyIdx, ctx.localMarketId, exportTariff);
        TRADE_policyAddTariffIncome_(ctx.policyIdx, pick.marketId, importTariff);
        ctx.rep.tariffsPaid += tariffs;
      }

      // throughput (1 per unit)
      if (pick.source === "LOCAL") {
        ctx.tpLeftProv[String(b.Провинция)] = (ctx.tpLeftProv[String(b.Провинция)] || 0) - can;
        ctx.rep.provThroughputSpent += can;
      } else {
        ctx.accessPool[pick.marketId] = (ctx.accessPool[pick.marketId] || 0) - can;
        ctx.rep.accessThroughputSpent += can;
      }

      // rep
      ctx.rep.sellSoldUnits += can;
      ctx.rep.sellEarnedMoney += net;

      // agg
      TRADE_recordTrade_(ctx.tradeAgg, "SELL", pick.source, pick.marketId, g, can, net, can, tariffs);

      if (pick.source === "ACCESS") {
        var pAgg2 = TRADE_ensurePartnerAgg_(ctx.tradeAgg, pick.marketId);
        pAgg2.exportsUnits += can;
        pAgg2.exportsRevenue += net;
        pAgg2.tariffs += tariffs;

        TRADE_recordFlow_(ctx.tradeAgg, ctx.localMarketId, pick.marketId, can, gross, tariffs);

        var lAgg2 = TRADE_ensureLocalAgg_(ctx.tradeAgg, ctx.localMarketId);
        lAgg2.exportsRevenue += net;
        var lp2 = TRADE_ensureLocalPartnerAgg_(lAgg2, pick.marketId);
        lp2.exportsRevenue += net;
      }

      qty -= can;
    }
  });
}

function TRADE_pickMarketForSell_(ctx) {
  var g = ctx.good;
  var candidates = [];

  // local
  var m0 = ctx.marketById[ctx.localMarketId];
  if (m0 && m0.Товары && m0.Товары[g]) {
    var tp0 = Math.floor(Number(ctx.tpLeftProv[ctx.provKey]) || 0);
    candidates.push({
      marketId: String(ctx.localMarketId),
      market: m0,
      source: "LOCAL",
      tpAvail: tp0,
      effRevenue: TRADE_marketPriceSafe_(m0, g)
    });
  }

  // access
  Object.keys(ctx.accessPool || {}).forEach(function (mid) {
    var cap = Math.floor(Number(ctx.accessPool[mid]) || 0);
    if (cap <= 0) return;

    var m = ctx.marketById[String(mid)];
    if (!m || !m.Товары || !m.Товары[g]) return;

    if (!TRADE_policyIsAllowed_(ctx.policyIdx, String(ctx.localMarketId), "Экспорт", g)) {
      if (ctx.tradeAgg) TRADE_recordPolicyBlock_(ctx.tradeAgg, "SELL", String(ctx.localMarketId), g);
      return;
    }
    if (!TRADE_policyIsAllowed_(ctx.policyIdx, String(mid), "Импорт", g)) {
      if (ctx.tradeAgg) TRADE_recordPolicyBlock_(ctx.tradeAgg, "SELL", String(mid), g);
      return;
    }

    var basePrice = TRADE_marketPriceSafe_(m, g);
    var exportT = TRADE_policyGetTariff_(ctx.policyIdx, String(ctx.localMarketId), "Экспорт", g);
    var importT = TRADE_policyGetTariff_(ctx.policyIdx, String(mid), "Импорт", g);

    var effRevenue = basePrice * (1 - exportT - importT);
    if (effRevenue < 0) effRevenue = 0;

    candidates.push({
      marketId: String(mid),
      market: m,
      source: "ACCESS",
      tpAvail: cap,
      effRevenue: effRevenue
    });
  });

  candidates = candidates.filter(function (c) {
    if (!(c.tpAvail > 0)) return false;
    var v = Number(c.effRevenue);
    return isFinite(v) && v > 0;
  });

  if (!candidates.length) return null;
  if (!TRADE_SELECT_BY_EFFECTIVE_PRICE) return candidates[0];

  candidates.sort(function (a, b) { return (b.effRevenue || 0) - (a.effRevenue || 0); });
  return candidates[0];
}

/* =========================================================
   BUILDING wallet/stock
   ========================================================= */

function TRADE_ensureBuildingWalletAndStock_(b) {
  if (!b.Склад || typeof b.Склад !== "object" || Array.isArray(b.Склад)) b.Склад = {};
  if (b.Деньги == null || isNaN(Number(b.Деньги))) b.Деньги = 0;

  if (b.ПрибыльЗаХод == null || isNaN(Number(b.ПрибыльЗаХод))) b.ПрибыльЗаХод = 0;
  if (b.РасходЗаХод == null || isNaN(Number(b.РасходЗаХод))) b.РасходЗаХод = 0;
}
function TRADE_resetBuildingTurnStats_(b) { b.ПрибыльЗаХод = 0; b.РасходЗаХод = 0; }

/* =========================================================
   POLICY: index, defaults, cleanup
   ========================================================= */

function TRADE_buildPolicyIndex_(data) {
  var idx = {};
  var col = data ? data[TRADE_POLICY_COL] : null;
  if (!Array.isArray(col)) return idx;

  for (var r = 0; r < col.length; r++) {
    var cell = col[r];
    var obj = TRADE_parseMaybeJsonObject_(cell);
    if (!obj || typeof obj !== "object" || Array.isArray(obj)) continue;

    var mid = obj["Рынок"] || obj["Идентификатор рынка"] || obj["marketId"] || obj["id"] || null;
    if (!mid) continue;
    mid = String(mid).trim();
    if (!mid) continue;

    TRADE_ensurePolicyShape_(obj, mid);
    idx[mid] = obj;
  }
  return idx;
}

function TRADE_policyMakeDefaultRow_(marketId) {
  return {
    "Рынок": String(marketId),
    "Тарифы": { "Импорт": { "default": 0 }, "Экспорт": { "default": 0 } },
    "Разрешения": {
      "Импорт":  { mode: "allow", goods: ["*"] },
      "Экспорт": { mode: "allow", goods: ["*"] }
    },
    "Учёт": { "ПошлиныЗаХод": 0, "ПошлиныИтого": 0 }
  };
}

function TRADE_policyEnsureAllMarketsHaveRows_(data, marketById, policyIdx) {
  if (!data || typeof data !== "object") return 0;
  if (!Array.isArray(data[TRADE_POLICY_COL])) data[TRADE_POLICY_COL] = [];

  var col = data[TRADE_POLICY_COL];
  var created = 0;

  Object.keys(marketById || {}).forEach(function (mid) {
    mid = String(mid);
    if (policyIdx && policyIdx[mid]) return;

    var row = TRADE_policyMakeDefaultRow_(mid);
    var slot = TRADE_findFirstFreeRow_(col);
    if (slot === -1) col.push(row);
    else col[slot] = row;

    if (policyIdx) policyIdx[mid] = row;
    created++;
  });

  return created;
}

function TRADE_policyCleanupOrphans_(data, marketById, policyIdx) {
  if (!data || typeof data !== "object") return 0;
  var col = data[TRADE_POLICY_COL];
  if (!Array.isArray(col)) return 0;

  var removed = 0;
  for (var r = 0; r < col.length; r++) {
    var obj = TRADE_parseMaybeJsonObject_(col[r]);
    if (!obj || typeof obj !== "object" || Array.isArray(obj)) continue;

    var mid = obj["Рынок"] || obj["Идентификатор рынка"] || obj["marketId"] || obj["id"] || null;
    if (!mid) continue;
    mid = String(mid).trim();
    if (!mid) continue;

    if (!marketById || !marketById[mid]) {
      col[r] = "";
      if (policyIdx && policyIdx[mid]) delete policyIdx[mid];
      removed++;
    }
  }
  return removed;
}

function TRADE_resetPolicyTurnCounters_(policyIdx) {
  Object.keys(policyIdx || {}).forEach(function (mid) {
    var p = policyIdx[mid];
    if (!p.Учёт || typeof p.Учёт !== "object") p.Учёт = {};
    p.Учёт.ПошлиныЗаХод = 0;
  });
}

function TRADE_ensurePolicyShape_(obj, mid) {
  if (!obj["Рынок"]) obj["Рынок"] = String(mid);

  if (!obj.Тарифы || typeof obj.Тарифы !== "object") obj.Тарифы = {};
  if (!obj.Тарифы.Импорт || typeof obj.Тарифы.Импорт !== "object") obj.Тарифы.Импорт = { "default": 0 };
  if (!obj.Тарифы.Экспорт || typeof obj.Тарифы.Экспорт !== "object") obj.Тарифы.Экспорт = { "default": 0 };

  if (!obj.Разрешения || typeof obj.Разрешения !== "object") obj.Разрешения = {};
  if (!obj.Разрешения.Импорт || typeof obj.Разрешения.Импорт !== "object") obj.Разрешения.Импорт = { mode: "allow", goods: ["*"] };
  if (!obj.Разрешения.Экспорт || typeof obj.Разрешения.Экспорт !== "object") obj.Разрешения.Экспорт = { mode: "allow", goods: ["*"] };

  if (!obj.Учёт || typeof obj.Учёт !== "object") obj.Учёт = {};
  if (obj.Учёт.ПошлиныЗаХод == null || isNaN(Number(obj.Учёт.ПошлиныЗаХод))) obj.Учёт.ПошлиныЗаХод = 0;
  if (obj.Учёт.ПошлиныИтого == null || isNaN(Number(obj.Учёт.ПошлиныИтого))) obj.Учёт.ПошлиныИтого = 0;
}

function TRADE_policyIsAllowed_(policyIdx, marketId, kind, good) {
  var p = policyIdx ? policyIdx[String(marketId)] : null;
  if (!p) return true;

  var rules = (p.Разрешения && p.Разрешения[kind]) ? p.Разрешения[kind] : null;
  if (!rules || typeof rules !== "object") return true;

  var mode = String(rules.mode || "allow").toLowerCase();
  var list = Array.isArray(rules.goods) ? rules.goods : [];
  var set = {};
  for (var i = 0; i < list.length; i++) {
    var s = String(list[i] || "").trim();
    if (s) set[s] = true;
  }

  var g = String(good || "").trim();
  if (!g) return true;

  if (set["*"]) return (mode === "allow");
  if (mode === "allow") return !!set[g];
  return !set[g];
}

function TRADE_policyGetTariff_(policyIdx, marketId, kind, good) {
  var p = policyIdx ? policyIdx[String(marketId)] : null;
  if (!p) return 0;

  var tRoot = (p.Тарифы && p.Тарифы[kind] && typeof p.Тарифы[kind] === "object") ? p.Тарифы[kind] : null;
  if (!tRoot) return 0;

  var g = String(good || "").trim();
  var v = (g && tRoot[g] != null) ? tRoot[g] : tRoot["default"];

  v = Number(v);
  if (!isFinite(v) || v < 0) v = 0;
  if (v > 0.95) v = 0.95;
  return v;
}

function TRADE_policyAddTariffIncome_(policyIdx, marketId, amount) {
  if (!(amount > 0)) return;
  var p = policyIdx ? policyIdx[String(marketId)] : null;
  if (!p) return;

  if (!p.Учёт || typeof p.Учёт !== "object") p.Учёт = {};
  p.Учёт.ПошлиныЗаХод = (Number(p.Учёт.ПошлиныЗаХод) || 0) + amount;
  p.Учёт.ПошлиныИтого = (Number(p.Учёт.ПошлиныИтого) || 0) + amount;
}

/* =========================================================
   ACCESS POOL (data["Данные государства"]["Доступ к рынкам"])
   ========================================================= */

function TRADE_getStateAccessMarketsPool_(data, marketById) {
  var root = data ? data["Данные государства"] : null;
  var hit = TRADE_findStateObjectByKey_(root, TRADE_STATE_ACCESS_KEY);

  var obj, idx;
  if (hit) { obj = hit.obj; idx = hit.idx; }
  else {
    obj = {}; obj[TRADE_STATE_ACCESS_KEY] = {};
    idx = TRADE_appendStateObject_(data, obj);
  }

  var val = obj[TRADE_STATE_ACCESS_KEY];
  var pool = TRADE_parseAccessMarketsValueToPool_(val, marketById);
  obj[TRADE_STATE_ACCESS_KEY] = pool;

  return { pool: pool, rawRef: { obj: obj, idx: idx } };
}

function TRADE_parseAccessMarketsValueToPool_(val, marketById) {
  var pool = {};

  if (val && typeof val === "object" && !Array.isArray(val)) {
    Object.keys(val).forEach(function (k) {
      var mid = TRADE_resolveMarketId_(k, marketById);
      var cap = Math.floor(Number(val[k]) || 0);
      if (mid && cap > 0) pool[mid] = cap;
    });
    return pool;
  }

  if (Array.isArray(val)) {
    val.forEach(function (row) {
      if (!row || typeof row !== "object") return;
      var k = row.Рынок || row.market || row.id || row["Идентификатор рынка"] || null;
      var cap = Math.floor(Number(row.Пропускная || row.cap || row.throughput || row["Пропускная способность"]) || 0);
      var mid = TRADE_resolveMarketId_(k, marketById);
      if (mid && cap > 0) pool[mid] = cap;
    });
    return pool;
  }

  if (typeof val === "string") {
    var s = val.trim();
    if (!s) return pool;

    var parsed = TRADE_parseMaybeJsonObject_(s);
    if (parsed) return TRADE_parseAccessMarketsValueToPool_(parsed, marketById);

    s.split(",").forEach(function (part) {
      var t = part.trim();
      if (!t) return;
      var kv = t.split(":");
      if (kv.length < 2) return;
      var mid = TRADE_resolveMarketId_(kv[0].trim(), marketById);
      var cap = Math.floor(Number(kv[1]) || 0);
      if (mid && cap > 0) pool[mid] = cap;
    });
    return pool;
  }

  return pool;
}

/* =========================================================
   STATS/NEWS (TOP + агрегаты)
   ========================================================= */

function TRADE_pushBuildingToStats_(arr, b) {
  arr.push({
    prov: String(b.Провинция || ""),
    type: String(b.Тип || ""),
    lvl: Number(b.Уровень) || 1,
    income: Number(b.ПрибыльЗаХод) || 0,
    expense: Number(b.РасходЗаХод) || 0,
    money: Number(b.Деньги) || 0
  });
}

function TRADE_pushBuildingTopNotice_RU_(data, buildingStats) {
  if (typeof ensureNews === "function") ensureNews(data);
  if (typeof pushNotice !== "function") return;

  var C = "#6E675F";
  function line(parts, s) { parts.push({ text: String(s) + "\n", color: C }); }

  var topIncome = (buildingStats || []).slice().sort(function (a, b) { return b.income - a.income; }).slice(0, 20);
  var topExpense = (buildingStats || []).slice().sort(function (a, b) { return b.expense - a.expense; }).slice(0, 20);

  var parts = [];
  line(parts, "Торговля: TOP зданий по доходам/расходам (за ход)");
  line(parts, "┌────────────────────────────────────────────────────────┐");

  line(parts, "┃ ➔ TOP-20 по доходам:");
  if (!topIncome.length) line(parts, "┃    - нет данных");
  for (var i = 0; i < topIncome.length; i++) {
    var x = topIncome[i];
    line(parts, "┃    " + (i + 1) + ") " + x.type + " [" + x.prov + "] ур." + x.lvl + " — +" + TRADE_fmtMoney_(x.income));
  }

  line(parts, "┃");
  line(parts, "┃ ➔ TOP-20 по расходам:");
  if (!topExpense.length) line(parts, "┃    - нет данных");
  for (var j = 0; j < topExpense.length; j++) {
    var y = topExpense[j];
    line(parts, "┃    " + (j + 1) + ") " + y.type + " [" + y.prov + "] ур." + y.lvl + " — -" + TRADE_fmtMoney_(y.expense));
  }

  line(parts, "└────────────────────────────────────────────────────────┘");

  pushNotice(data, { category: "Торговля", sub: "TOP зданий", priority: 84, parts: parts });
}

function TRADE_pushCommerceSummaryNotice_RU_(data, rep) {
  if (typeof ensureNews === "function") ensureNews(data);
  if (typeof pushNotice !== "function") return;

  var C = "#6E675F";
  function line(parts, s) { parts.push({ text: String(s) + "\n", color: C }); }

  var parts = [];
  line(parts, "Торговля: сводка закупок/продаж (за ход)");
  line(parts, "┌────────────────────────────────────────────────────────┐");
  line(parts, "┃ ➔ Зданий обработано: " + String(rep.buildingsTouched));
  line(parts, "┃");
  line(parts, "┃ ➔ Покупка: план " + TRADE_fmtInt_(rep.buyPlannedUnits) +
    ", куплено " + TRADE_fmtInt_(rep.buyBoughtUnits) +
    ", нехватка " + TRADE_fmtInt_(rep.buyShortUnits));
  line(parts, "┃ ➔ Потрачено денег: " + TRADE_fmtMoney_(rep.buySpentMoney));
  line(parts, "┃");
  line(parts, "┃ ➔ Продажа: продано " + TRADE_fmtInt_(rep.sellSoldUnits));
  line(parts, "┃ ➔ Доход: " + TRADE_fmtMoney_(rep.sellEarnedMoney));
  line(parts, "┃");
  line(parts, "┃ ➔ Пошлины уплачено: " + TRADE_fmtMoney_(rep.tariffsPaid));
  line(parts, "┃");
  line(parts, "┃ ➔ Throughput провинций: -" + TRADE_fmtInt_(rep.provThroughputSpent));
  line(parts, "┃ ➔ Throughput доступа:   -" + TRADE_fmtInt_(rep.accessThroughputSpent));
  line(parts, "┃ ➔ Пул доступа: " + TRADE_fmtInt_(rep.accessPoolStart) + " → " + TRADE_fmtInt_(rep.accessPoolEnd));
  line(parts, "┃");
  line(parts, "┃ ➔ Политики: создано " + TRADE_fmtInt_(rep.policiesCreated) + ", удалено " + TRADE_fmtInt_(rep.policiesRemoved));
  line(parts, "└────────────────────────────────────────────────────────┘");

  pushNotice(data, { category: "Торговля", sub: "Сводка", priority: 85, parts: parts });
}

/* ---------- агрегатор торговли ---------- */

function TRADE_makeTradeAgg_() {
  return {
    buyUnits: 0, sellUnits: 0, buyCost: 0, sellRevenue: 0,
    provThroughputSpent: 0, accessThroughputSpent: 0,
    tariffs: 0,
    byMarket: {}, byGood: {},
    partners: {}, flows: {}, ourLocalMarkets: {},
    shortages: { byLocalMarket: {}, byGood: {} },
    policyBlocks: { buyBlocked: 0, sellBlocked: 0, byMarket: {}, byGood: {} }
  };
}

function TRADE_ensureMarketAgg_(agg, marketId) {
  marketId = String(marketId);
  if (!agg.byMarket[marketId]) agg.byMarket[marketId] = {
    buyUnits: 0, sellUnits: 0, buyCost: 0, sellRevenue: 0, provTP: 0, accessTP: 0, tariffs: 0, goods: {}
  };
  return agg.byMarket[marketId];
}
function TRADE_ensureGoodAgg_(agg, good) {
  good = String(good);
  if (!agg.byGood[good]) agg.byGood[good] = { buyUnits: 0, sellUnits: 0, buyCost: 0, sellRevenue: 0, tariffs: 0 };
  return agg.byGood[good];
}
function TRADE_ensureMarketGoodAgg_(mAgg, good) {
  good = String(good);
  if (!mAgg.goods[good]) mAgg.goods[good] = { buyUnits: 0, sellUnits: 0, buyCost: 0, sellRevenue: 0, tariffs: 0 };
  return mAgg.goods[good];
}

function TRADE_ensurePartnerAgg_(agg, partnerId) {
  partnerId = String(partnerId);
  if (!agg.partners[partnerId]) agg.partners[partnerId] = { importsUnits: 0, importsCost: 0, exportsUnits: 0, exportsRevenue: 0, tariffs: 0 };
  return agg.partners[partnerId];
}
function TRADE_ensureLocalAgg_(agg, localId) {
  localId = String(localId);
  if (!agg.ourLocalMarkets[localId]) agg.ourLocalMarkets[localId] = { importsCost: 0, exportsRevenue: 0, partners: {} };
  return agg.ourLocalMarkets[localId];
}
function TRADE_ensureLocalPartnerAgg_(localAgg, partnerId) {
  partnerId = String(partnerId);
  if (!localAgg.partners[partnerId]) localAgg.partners[partnerId] = { importsCost: 0, exportsRevenue: 0 };
  return localAgg.partners[partnerId];
}
function TRADE_recordFlow_(agg, fromMarketId, toMarketId, units, baseTurnover, tariffs) {
  var key = String(fromMarketId) + "->" + String(toMarketId);
  if (!agg.flows[key]) agg.flows[key] = { units: 0, baseTurnover: 0, tariffs: 0 };
  agg.flows[key].units += Math.floor(Number(units) || 0);
  agg.flows[key].baseTurnover += Number(baseTurnover) || 0;
  agg.flows[key].tariffs += Number(tariffs) || 0;
}
function TRADE_recordShortage_(agg, localMarketId, good, units) {
  units = Math.floor(Number(units) || 0);
  if (units <= 0) return;
  localMarketId = String(localMarketId); good = String(good);
  if (!agg.shortages.byLocalMarket[localMarketId]) agg.shortages.byLocalMarket[localMarketId] = {};
  agg.shortages.byLocalMarket[localMarketId][good] = (agg.shortages.byLocalMarket[localMarketId][good] || 0) + units;
  agg.shortages.byGood[good] = (agg.shortages.byGood[good] || 0) + units;
}
function TRADE_recordPolicyBlock_(agg, kind, marketId, good) {
  if (!agg || !agg.policyBlocks) return;
  if (kind === "BUY") agg.policyBlocks.buyBlocked++; else agg.policyBlocks.sellBlocked++;
  marketId = String(marketId); good = String(good);
  agg.policyBlocks.byMarket[marketId] = (agg.policyBlocks.byMarket[marketId] || 0) + 1;
  agg.policyBlocks.byGood[good] = (agg.policyBlocks.byGood[good] || 0) + 1;
}
function TRADE_recordTrade_(agg, kind, source, marketId, good, units, moneyAmount, tpUnits, tariffsAmount) {
  units = Math.floor(Number(units) || 0);
  moneyAmount = Number(moneyAmount) || 0;
  tpUnits = Math.floor(Number(tpUnits) || 0);
  tariffsAmount = Number(tariffsAmount) || 0;
  if (!(units > 0)) return;

  var mAgg = TRADE_ensureMarketAgg_(agg, marketId);
  var gAgg = TRADE_ensureGoodAgg_(agg, good);
  var mgAgg = TRADE_ensureMarketGoodAgg_(mAgg, good);

  if (kind === "BUY") {
    agg.buyUnits += units; agg.buyCost += moneyAmount;
    mAgg.buyUnits += units; mAgg.buyCost += moneyAmount;
    gAgg.buyUnits += units; gAgg.buyCost += moneyAmount;
    mgAgg.buyUnits += units; mgAgg.buyCost += moneyAmount;
  } else {
    agg.sellUnits += units; agg.sellRevenue += moneyAmount;
    mAgg.sellUnits += units; mAgg.sellRevenue += moneyAmount;
    gAgg.sellUnits += units; gAgg.sellRevenue += moneyAmount;
    mgAgg.sellUnits += units; mgAgg.sellRevenue += moneyAmount;
  }

  if (tariffsAmount > 0) {
    agg.tariffs += tariffsAmount;
    mAgg.tariffs += tariffsAmount;
    gAgg.tariffs += tariffsAmount;
    mgAgg.tariffs += tariffsAmount;
  }

  if (source === "LOCAL") { agg.provThroughputSpent += tpUnits; mAgg.provTP += tpUnits; }
  else { agg.accessThroughputSpent += tpUnits; mAgg.accessTP += tpUnits; }
}

function TRADE_pushTradeStatsNotice_RU_(data, agg, policyIdx) {
  if (typeof ensureNews === "function") ensureNews(data);
  if (typeof pushNotice !== "function") return;

  var C = "#6E675F";
  function line(parts, s) { parts.push({ text: String(s) + "\n", color: C }); }

  var markets = Object.keys(agg.byMarket || {}).map(function (mid) {
    var m = agg.byMarket[mid];
    var turnover = (Number(m.buyCost) || 0) + (Number(m.sellRevenue) || 0);
    return { mid: mid, m: m, turnover: turnover };
  }).sort(function (a, b) { return b.turnover - a.turnover; }).slice(0, TRADE_TRADE_STATS_TOP_MARKETS);

  var goods = Object.keys(agg.byGood || {}).map(function (g) {
    var x = agg.byGood[g];
    var turnover = (Number(x.buyCost) || 0) + (Number(x.sellRevenue) || 0);
    return { g: g, x: x, turnover: turnover };
  }).sort(function (a, b) { return b.turnover - a.turnover; }).slice(0, TRADE_TRADE_STATS_TOP_GOODS);

  var tariffMarkets = [];
  Object.keys(policyIdx || {}).forEach(function (mid) {
    var p = policyIdx[mid];
    var th = (p && p.Учёт) ? Number(p.Учёт.ПошлиныЗаХод) || 0 : 0;
    if (th > 0) tariffMarkets.push({ mid: mid, th: th });
  });
  tariffMarkets.sort(function (a, b) { return b.th - a.th; });
  tariffMarkets = tariffMarkets.slice(0, 10);

  var parts = [];
  line(parts, "Торговля: статистика операций (за ход)");
  line(parts, "┌────────────────────────────────────────────────────────┐");
  line(parts, "┃ ➔ Покупки:  " + TRADE_fmtInt_(agg.buyUnits) + " ед. на " + TRADE_fmtMoney_(agg.buyCost));
  line(parts, "┃ ➔ Продажи:  " + TRADE_fmtInt_(agg.sellUnits) + " ед. на " + TRADE_fmtMoney_(agg.sellRevenue));
  line(parts, "┃ ➔ Баланс:   " + TRADE_fmtMoney_((Number(agg.sellRevenue) || 0) - (Number(agg.buyCost) || 0)));
  line(parts, "┃ ➔ Пошлины (встроенные): " + TRADE_fmtMoney_(agg.tariffs));
  line(parts, "┃");
  line(parts, "┃ ➔ Throughput провинций: -" + TRADE_fmtInt_(agg.provThroughputSpent));
  line(parts, "┃ ➔ Throughput доступа:   -" + TRADE_fmtInt_(agg.accessThroughputSpent));

  if (markets.length) {
    line(parts, "┃");
    line(parts, "┃ ➔ TOP рынков по обороту:");
    for (var i = 0; i < markets.length; i++) {
      var t = markets[i];
      line(parts, "┃    " + (i + 1) + ") Рынок " + t.mid +
        ": покупка " + TRADE_fmtMoney_(t.m.buyCost) +
        ", продажа " + TRADE_fmtMoney_(t.m.sellRevenue) +
        ", пошлины " + TRADE_fmtMoney_(t.m.tariffs));
    }
  }

  if (goods.length) {
    line(parts, "┃");
    line(parts, "┃ ➔ TOP товаров по обороту:");
    for (var j = 0; j < goods.length; j++) {
      var u = goods[j];
      line(parts, "┃    " + (j + 1) + ") " + u.g +
        ": куплено " + TRADE_fmtInt_(u.x.buyUnits) + " (" + TRADE_fmtMoney_(u.x.buyCost) + ")" +
        ", продано " + TRADE_fmtInt_(u.x.sellUnits) + " (" + TRADE_fmtMoney_(u.x.sellRevenue) + ")" +
        ", пошлины " + TRADE_fmtMoney_(u.x.tariffs));
    }
  }

  if (tariffMarkets.length) {
    line(parts, "┃");
    line(parts, "┃ ➔ TOP рынков по доходу от пошлин (ПошлиныЗаХод):");
    for (var k = 0; k < tariffMarkets.length; k++) {
      line(parts, "┃    " + (k + 1) + ") Рынок " + tariffMarkets[k].mid + " — " + TRADE_fmtMoney_(tariffMarkets[k].th));
    }
  }

  line(parts, "└────────────────────────────────────────────────────────┘");

  pushNotice(data, { category: "Торговля", sub: "Статистика", priority: 83, parts: parts });
}

function TRADE_pushStateTradeIntelNotice_RU_(data, agg) {
  if (typeof ensureNews === "function") ensureNews(data);
  if (typeof pushNotice !== "function") return;

  var C = "#6E675F";
  function line(parts, s) { parts.push({ text: String(s) + "\n", color: C }); }

  var partners = Object.keys(agg.partners || {}).map(function (mid) {
    var p = agg.partners[mid];
    var imp = Number(p.importsCost) || 0;
    var exp = Number(p.exportsRevenue) || 0;
    return { mid: mid, imp: imp, exp: exp, turn: imp + exp, bal: exp - imp, tariffs: Number(p.tariffs) || 0 };
  }).sort(function (a, b) { return b.turn - a.turn; }).slice(0, 15);

  var flows = Object.keys(agg.flows || {}).map(function (k) {
    var f = agg.flows[k];
    return { k: k, units: f.units || 0, base: f.baseTurnover || 0, tariffs: f.tariffs || 0 };
  }).sort(function (a, b) { return b.base - a.base; }).slice(0, 10);

  var shortageGoods = Object.keys((agg.shortages && agg.shortages.byGood) ? agg.shortages.byGood : {}).map(function (g) {
    return { g: g, u: agg.shortages.byGood[g] };
  }).sort(function (a, b) { return b.u - a.u; }).slice(0, 10);

  var pb = agg.policyBlocks || { buyBlocked: 0, sellBlocked: 0, byMarket: {}, byGood: {} };
  var pbGoods = Object.keys(pb.byGood || {}).map(function (g) { return { g: g, c: pb.byGood[g] }; })
    .sort(function (a, b) { return b.c - a.c; }).slice(0, 10);

  var parts = [];
  line(parts, "Торговля: гос-аналитика по внешним рынкам (за ход)");
  line(parts, "┌────────────────────────────────────────────────────────┐");

  line(parts, "┃ ➔ Внешний оборот (по зданиям):");
  line(parts, "┃    импорт: " + TRADE_fmtMoney_(agg.buyCost) + " | экспорт: " + TRADE_fmtMoney_(agg.sellRevenue));
  line(parts, "┃    баланс: " + TRADE_fmtMoney_((Number(agg.sellRevenue) || 0) - (Number(agg.buyCost) || 0)));
  line(parts, "┃    пошлины: " + TRADE_fmtMoney_(agg.tariffs));
  line(parts, "┃");

  line(parts, "┃ ➔ TOP партнёров по обороту:");
  if (!partners.length) line(parts, "┃    - нет внешних сделок");
  for (var i = 0; i < partners.length; i++) {
    var t = partners[i];
    line(parts, "┃    " + (i + 1) + ") Рынок " + t.mid +
      " | оборот " + TRADE_fmtMoney_(t.turn) +
      " | импорт " + TRADE_fmtMoney_(t.imp) +
      " | экспорт " + TRADE_fmtMoney_(t.exp) +
      " | баланс " + TRADE_fmtMoney_(t.bal) +
      " | пошлины " + TRADE_fmtMoney_(t.tariffs));
  }

  if (flows.length) {
    line(parts, "┃");
    line(parts, "┃ ➔ TOP маршрутов (рынок->рынок) по базе:");
    for (var j = 0; j < flows.length; j++) {
      var f = flows[j];
      line(parts, "┃    " + (j + 1) + ") " + f.k +
        " | " + TRADE_fmtInt_(f.units) + " ед." +
        " | база " + TRADE_fmtMoney_(f.base) +
        " | пошлины " + TRADE_fmtMoney_(f.tariffs));
    }
  }

  if (shortageGoods.length) {
    line(parts, "┃");
    line(parts, "┃ ➔ Дефицит закупок (не удалось купить):");
    for (var k = 0; k < shortageGoods.length; k++) {
      line(parts, "┃    " + (k + 1) + ") " + shortageGoods[k].g + " — " + TRADE_fmtInt_(shortageGoods[k].u) + " ед.");
    }
  }

  line(parts, "┃");
  line(parts, "┃ ➔ Блокировки торговой политики:");
  line(parts, "┃    BUY: " + TRADE_fmtInt_(pb.buyBlocked) + " | SELL: " + TRADE_fmtInt_(pb.sellBlocked));
  if (pbGoods.length) {
    line(parts, "┃    TOP товаров по блокировкам:");
    for (var z = 0; z < pbGoods.length; z++) {
      line(parts, "┃      " + (z + 1) + ") " + pbGoods[z].g + " — " + TRADE_fmtInt_(pbGoods[z].c));
    }
  }

  line(parts, "└────────────────────────────────────────────────────────┘");

  pushNotice(data, { category: "Торговля", sub: "Гос-аналитика", priority: 82, parts: parts });
}

/* =========================================================
   HELPERS: index/parsing/utils/state/provinces/templates
   ========================================================= */

function TRADE_parseMaybeJsonObject_(v) {
  if (v === null || v === undefined || v === "") return null;
  if (typeof v === "object") return v;
  if (typeof v !== "string") return null;

  var s = v.trim();
  if (!s) return null;

  if (typeof safeParse === "function") {
    var o1 = safeParse(s);
    if (o1 && typeof o1 === "object") return o1;
  }

  if (s[0] === "{" || s[0] === "[") {
    try {
      var o2 = JSON.parse(s);
      if (o2 && typeof o2 === "object") return o2;
    } catch (e) {}
  }
  return null;
}

function TRADE_findFirstFreeRow_(col) {
  if (!Array.isArray(col)) return -1;
  for (var i = 0; i < col.length; i++) {
    var v = col[i];
    if (v === "" || v === null || v === undefined) return i;
    if (typeof v !== "object") return i;
    if (Array.isArray(v)) return i;
  }
  return -1;
}

function TRADE_marketPriceSafe_(marketObj, good) {
  var st = marketObj && marketObj.Товары ? marketObj.Товары[good] : null;
  var p = st ? Number(st.Цена) : 1;
  if (!isFinite(p) || p <= 0) p = 1;
  return p;
}

function TRADE_indexMarkets_(tradeCol) {
  var byId = {};
  for (var r = 0; r < tradeCol.length; r++) {
    var cell = tradeCol[r];
    if (!cell || typeof cell !== "object" || Array.isArray(cell)) continue;

    var id = TRADE_marketIdFromMarketCell_RU_(cell);
    if (!id) continue;

    if (!cell.Товары || typeof cell.Товары !== "object") cell.Товары = {};
    byId[String(id)] = cell;
  }
  return byId;
}

function TRADE_marketIdFromMarketCell_RU_(m) {
  if (!m || typeof m !== "object") return null;
  if (m["Идентификатор рынка"] != null && String(m["Идентификатор рынка"]).trim() !== "") return String(m["Идентификатор рынка"]).trim();
  if (m.РынокId != null && String(m.РынокId).trim() !== "") return String(m.РынокId).trim();
  if (m.id != null && String(m.id).trim() !== "") return String(m.id).trim();
  return null;
}

function TRADE_buildTemplatesIndexByType_full_(templatesCol) {
  var map = {};
  if (!Array.isArray(templatesCol)) return map;

  for (var r = 0; r < templatesCol.length; r++) {
    var cell = templatesCol[r];
    var list = [];

    if (Array.isArray(cell)) list = cell;
    else if (cell && typeof cell === "object") list = [cell];
    else continue;

    for (var i = 0; i < list.length; i++) {
      var tpl = list[i];
      if (!tpl || typeof tpl !== "object") continue;

      var t = tpl.Тип || tpl["Тип"] || tpl.type || null;
      if (!t) continue;
      t = String(t);

      var ins = tpl.Входы;
      if (!ins || typeof ins !== "object" || Array.isArray(ins)) ins = {};

      var outs = tpl.Выходы;
      if (!outs || typeof outs !== "object" || Array.isArray(outs)) outs = {};

      map[t] = { inputs: ins, outputs: outs };
    }
  }
  return map;
}

function TRADE_fmtMoney_(x) {
  x = Number(x);
  if (!isFinite(x)) x = 0;
  return (Math.round(x * 100) / 100).toString();
}

function TRADE_fmtInt_(x) {
  x = Math.floor(Number(x) || 0);
  return String(x).replace(/\B(?=(\d{3})+(?!\d))/g, " ");
}

function TRADE_sumPool_(pool) {
  var sum = 0;
  Object.keys(pool || {}).forEach(function (k) { sum += Math.floor(Number(pool[k]) || 0); });
  return sum;
}

// stateId wrappers
function TRADE_getStateId_(data) {
  try { if (typeof LOG_getStateIdFromStateData_ === "function") return LOG_getStateIdFromStateData_(data); } catch (e) {}
  try { if (typeof getStateIdSafe === "function") return getStateIdSafe(data); } catch (e2) {}
  return null;
}
function TRADE_getAllProvincesFlat_(data) {
  try { if (typeof LOG_getAllProvincesFlat_ === "function") return LOG_getAllProvincesFlat_(data); } catch (e) {}
  if (!Array.isArray(data.Провинции)) return [];
  return normalizeToArray(data.Провинции).reduce(function (acc, row) { return acc.concat(normalizeToArray(row)); }, []);
}
function TRADE_getProvKey_(p) {
  try { if (typeof LOG_getProvKey_ === "function") return LOG_getProvKey_(p); } catch (e) {}
  return p.Провинция || p.Название || p.id || null;
}
function TRADE_getMarketId_(p) {
  try { if (typeof LOG_getMarketId_ === "function") return LOG_getMarketId_(p); } catch (e) {}
  if (!p || typeof p !== "object") return null;
  if (p.РынокId != null && String(p.РынокId).trim() !== "") return String(p.РынокId).trim();
  if (p.Рынок != null && String(p.Рынок).trim() !== "") return String(p.Рынок).trim();
  return null;
}

// state data search/append for access pool
function TRADE_findStateObjectByKey_(root, key) {
  if (root === null || root === undefined) return null;
  var flat = [];
  normalizeToArray(root).forEach(function (row) { normalizeToArray(row).forEach(function (cell) { flat.push(cell); }); });
  for (var i = 0; i < flat.length; i++) {
    var obj = TRADE_parseMaybeJsonObject_(flat[i]);
    if (obj && typeof obj === "object" && !Array.isArray(obj)) {
      if (Object.prototype.hasOwnProperty.call(obj, key)) return { obj: obj, idx: i };
    }
  }
  return null;
}
function TRADE_appendStateObject_(data, obj) {
  if (!data["Данные государства"]) data["Данные государства"] = [];
  if (!Array.isArray(data["Данные государства"])) data["Данные государства"] = [data["Данные государства"]];

  for (var r = 0; r < data["Данные государства"].length; r++) {
    if (Array.isArray(data["Данные государства"][r])) {
      data["Данные государства"][r].push(obj);
      return data["Данные государства"][r].length - 1;
    }
  }

  data["Данные государства"].push([obj]);
  return 0;
}
function TRADE_resolveMarketId_(token, marketById) {
  if (token === null || token === undefined) return null;
  var s = String(token).trim();
  if (!s) return null;
  // если id есть как ключ в marketById — ок; иначе всё равно возвращаем s (для будущих рынков)
  if (marketById && marketById[s]) return s;
  return s;
}