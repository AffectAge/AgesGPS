/* =========================================================
   POP NEEDS MODULE
   - Strata: Basic / Standard / Luxury
   - First pass: preferred goods
   - Second pass: substitution goods
   - Consumption + satisfaction
   ========================================================= */


/* =======================
   PREPARE POP AGENTS
   ======================= */

function POPS_preparePopulationAgents_(data) {
  if (!data) return;
  if (!Array.isArray(data.Население)) data.Население = [];

  normalizeToArray(data.Население).forEach(function (row) {
    normalizeToArray(row).forEach(function (g) {
      if (!g || typeof g !== "object") return;

      if (!g.Склад || typeof g.Склад !== "object" || Array.isArray(g.Склад)) g.Склад = {};
      if (!g.Нехватка || typeof g.Нехватка !== "object" || Array.isArray(g.Нехватка)) g.Нехватка = {};

      if (!g.ПотребностиЗаХод || typeof g.ПотребностиЗаХод !== "object")
        g.ПотребностиЗаХод = {};

      if (!g.ПотребностиСлои || typeof g.ПотребностиСлои !== "object")
        g.ПотребностиСлои = { Basic: {}, Standard: {}, Luxury: {} };

      if (!g.СубститутыЗаХод || typeof g.СубститутыЗаХод !== "object")
        g.СубститутыЗаХод = {};

      if (g.Деньги == null || isNaN(Number(g.Деньги))) {
        var w = POPS_getWealth_(g);
        g.Деньги = isFinite(w) ? w : 0;
      }

      if (g.РасходЗаХод == null) g.РасходЗаХод = 0;
      if (g.Пошлины == null) g.Пошлины = 0;
      if (g.Удовлетворение == null) g.Удовлетворение = 1;
    });
  });
}


/* =======================
   NEEDS TEMPLATE
   ======================= */

function POPS_getNeedsTemplate_(data) {
  var t = data && data["Шаблоны потребностей населения"];
  if (t && typeof t === "object") return t;

  // дефолт
  return {
    strataPer1000: {
      Basic:    { "Зерно": 8, "Рыба": 3, "Ткань": 2, "Дрова": 2 },
      Standard: { "Одежда": 1, "Мебель": 0.5 },
      Luxury:   { "Чай": 0.8, "Сахар": 0.8, "Вино": 0.6 }
    },
    thresholdsWealthPerCapita: {
      Basic: 0,
      Standard: 6,
      Luxury: 18
    },
    strataWeights: {
      Basic: 3,
      Standard: 1.5,
      Luxury: 0.7
    },
    goodWeights: {
      "Зерно": 3, "Рыба": 2, "Ткань": 2, "Дрова": 2,
      "Одежда": 1.5, "Мебель": 1,
      "Чай": 1, "Сахар": 1, "Вино": 0.7
    },
    substitutions: {
      "Зерно": ["Рыба", "Мясо"],
      "Рыба": ["Зерно", "Мясо"],
      "Дрова": ["Уголь"]
    }
  };
}


/* =======================
   BUILD NEEDS PLAN (STRATA)
   ======================= */

function POPS_computeStrataPlans_(group, tpl) {
  var pop = POPS_getPopCount_(group);
  var per1000 = pop / 1000;

  var wealth = POPS_getWealth_(group);
  var wpc = pop > 0 ? wealth / pop : 0;

  var enabled = {
    Basic: true,
    Standard: wpc >= (tpl.thresholdsWealthPerCapita.Standard || 0),
    Luxury: wpc >= (tpl.thresholdsWealthPerCapita.Luxury || 0)
  };

  var flat = {};
  var byStrata = { Basic: {}, Standard: {}, Luxury: {} };

  ["Basic", "Standard", "Luxury"].forEach(function (S) {
    if (!enabled[S]) return;
    var src = tpl.strataPer1000[S] || {};

    Object.keys(src).forEach(function (g) {
      var q = Math.floor((Number(src[g]) || 0) * per1000);
      if (q <= 0) return;

      byStrata[S][g] = (byStrata[S][g] || 0) + q;
      flat[g] = (flat[g] || 0) + q;
    });
  });

  return { flat: flat, byStrata: byStrata };
}


/* =======================
   APPEND BUY ORDERS (1st pass)
   ======================= */

function POPS_appendPopulationBuyOrders_(ctx) {
  var data = ctx.data;
  var provByKey = ctx.provByKey;
  var tpLeftProv = ctx.tpLeftProv;
  var orders = ctx.orders;

  var tpl = POPS_getNeedsTemplate_(data);
  if (!orders || !orders.buy) return;

  normalizeToArray(data.Население).forEach(function (row) {
    normalizeToArray(row).forEach(function (g) {
      if (!g || !g.Провинция) return;
      var p = provByKey[String(g.Провинция)];
      if (!p) return;

      var marketId = TRADE_getMarketId_(p);
      if (!marketId) return;

      var provKey = String(g.Провинция);
      if (tpLeftProv[provKey] == null) {
        tpLeftProv[provKey] = Math.floor(Number(p.ПропускнаяСпособность) || 0);
      }

      var plan = POPS_computeStrataPlans_(g, tpl);
      g.ПотребностиЗаХод = plan.flat;
      g.ПотребностиСлои = plan.byStrata;

      Object.keys(plan.flat).forEach(function (good) {
        var need = Math.max(0,
          Math.floor(plan.flat[good]) - Math.floor(Number(g.Склад[good]) || 0)
        );
        if (need <= 0) return;

        orders.buy.push({
          b: g,
          provKey: provKey,
          localMarketId: String(marketId),
          good: String(good),
          qtyNeed: need,
          _kind: "POP",
          _pass: 1
        });
      });
    });
  });
}


/* =======================
   SUBSTITUTION (2nd pass)
   ======================= */

function POPS_runSubstitutionSecondPass_(ctx) {
  var data = ctx.data;
  var provByKey = ctx.provByKey;
  var sellOrders = ctx.sellOrders || [];

  var tpl = POPS_getNeedsTemplate_(data);
  var subs = tpl.substitutions || {};

  var orders = { buy: [], sell: sellOrders };

  normalizeToArray(data.Население).forEach(function (row) {
    normalizeToArray(row).forEach(function (g) {
      if (!g || !g.Провинция) return;
      var p = provByKey[String(g.Провинция)];
      if (!p) return;

      var marketId = TRADE_getMarketId_(p);
      if (!marketId) return;

      Object.keys(g.ПотребностиЗаХод || {}).forEach(function (pref) {
        var need = Math.max(
          0,
          Math.floor(g.ПотребностиЗаХод[pref]) -
          Math.floor(Number(g.Склад[pref]) || 0)
        );
        if (need <= 0) return;

        var list = subs[pref];
        if (!Array.isArray(list)) return;

        list.forEach(function (sg) {
          orders.buy.push({
            b: g,
            provKey: String(g.Провинция),
            localMarketId: String(marketId),
            good: String(sg),
            qtyNeed: need,
            _kind: "POP",
            _pass: 2,
            _subFor: pref
          });
        });
      });
    });
  });

  return TRADE_clearInternalTrades_({
    data: data,
    orders: orders,
    marketById: ctx.marketById,
    policyIdx: ctx.policyIdx,
    tpLeftProv: ctx.tpLeftProv,
    accessPool: ctx.accessPool
  });
}


/* =======================
   CONSUMPTION + SATISFACTION
   ======================= */

function POPS_consumeAndScore_(data) {
  var tpl = POPS_getNeedsTemplate_(data);
  var subs = tpl.substitutions || {};
  var wS = tpl.strataWeights || {};
  var wG = tpl.goodWeights || {};

  normalizeToArray(data.Население).forEach(function (row) {
    normalizeToArray(row).forEach(function (g) {
      if (!g || !g.ПотребностиСлои) return;

      var totalNum = 0, totalDen = 0;

      ["Basic", "Standard", "Luxury"].forEach(function (S) {
        var plan = g.ПотребностиСлои[S] || {};
        var num = 0, den = 0;

        Object.keys(plan).forEach(function (good) {
          var need = Math.floor(plan[good]);
          if (need <= 0) return;

          var have = Math.floor(Number(g.Склад[good]) || 0);
          var use = Math.min(have, need);

          g.Склад[good] = have - use;
          if (g.Склад[good] <= 0) delete g.Склад[good];

          var left = need - use;

          if (left > 0 && subs[good]) {
            subs[good].forEach(function (sg) {
              if (left <= 0) return;
              var h2 = Math.floor(Number(g.Склад[sg]) || 0);
              var u2 = Math.min(h2, left);
              g.Склад[sg] = h2 - u2;
              if (g.Склад[sg] <= 0) delete g.Склад[sg];
              g.СубститутыЗаХод[sg] = (g.СубститутыЗаХод[sg] || 0) + u2;
              left -= u2;
            });
          }

          if (left > 0) {
            g.Нехватка[good] = (g.Нехватка[good] || 0) + left;
          }

          var wg = wG[good] || 1;
          num += ((need - left) / need) * wg;
          den += wg;
        });

        var satS = den > 0 ? num / den : 1;
        var ws = wS[S] || 1;

        totalNum += satS * ws;
        totalDen += ws;
      });

      g.Удовлетворение = totalDen > 0 ? Math.max(0, Math.min(1, totalNum / totalDen)) : 1;
    });
  });
}


/* =======================
   HELPERS
   ======================= */

function POPS_getPopCount_(g) {
  var d = Array.isArray(g["Основные данные"]) ? g["Основные данные"][0] : null;
  var n = d ? Number(d["Количество"]) : 0;
  return Math.max(0, Math.floor(n || 0));
}

function POPS_getWealth_(g) {
  var d = Array.isArray(g["Основные данные"]) ? g["Основные данные"][0] : null;
  var w = d ? Number(d["Богатство"]) : NaN;
  if (!isFinite(w)) w = Number(g.Деньги);
  return isFinite(w) ? w : 0;
}