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
      
      // --- reset per-turn fields ---
      g.РасходЗаХод = 0;
      g.Пошлины = 0;
      g.СубститутыЗаХод = {};
      g.Нехватка = {};
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
      
      if (ctx.stateId != null && String(p.Владелец || "") !== String(ctx.stateId)) return;

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
      
      if (ctx.stateId != null && String(p.Владелец || "") !== String(ctx.stateId)) return;

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

/* =========================================================
   POP NEWS (1,2,4,5)
   - 1) Сводка потребностей
   - 2) Критический дефицит Basic
   - 4) Финансовое давление
   - 5) SoL + Радикалы/лоялисты
   ========================================================= */

function POPS_pushNewsBundle_(data, ctx) {
  if (typeof pushNotice !== "function") return;
  var C = "#6E675F";

  // 1) Summary needs
  POPS_pushNeedsSummaryNews_(data, ctx, C);

  // 2) Critical basic deficits
  POPS_pushCriticalBasicDeficitNews_(data, ctx, C);

  // 4) Financial pressure
  POPS_pushFinancialPressureNews_(data, ctx, C);

  // 5) SoL + politics
  POPS_pushSoLPoliticsNews_(data, ctx, C);
}


/* ============ 1) SUMMARY ============ */

function POPS_pushNeedsSummaryNews_(data, ctx, C) {
  var stat = POPS_aggregatePopStats_(data, ctx);
  if (stat.groupsTotal <= 0) return;

  var parts = [];
  parts.push({ text: "Население: сводка потребностей\n", color: C });
  parts.push({ text: "┌────────────────────────────────────────────────────────┐\n", color: C });
  parts.push({ text: "┃ ➔ Групп POP: " + stat.groupsTotal + "\n", color: C });
  parts.push({ text: "┃ ➔ Население: " + stat.popTotal + "\n", color: C });
  parts.push({ text: "┃ ➔ Среднее удовлетворение: " + POPS_round_(stat.avgSat, 2) + "\n", color: C });
  parts.push({ text: "┃ ➔ Групп с дефицитом: " + stat.groupsWithShort + " / " + stat.groupsTotal + "\n", color: C });
  parts.push({ text: "┃ ➔ Топ-нехватка: " + (stat.topShortText || "нет") + "\n", color: C });
  parts.push({ text: "┃ ➔ Субституты (ед.): " + stat.subUsed + "\n", color: C });
  parts.push({ text: "└────────────────────────────────────────────────────────┘\n", color: C });

  pushNotice(data, { category: "Население", sub: "Потребности", priority: 80, parts: parts });
}


/* ============ 2) CRITICAL BASIC DEFICIT ============ */

function POPS_pushCriticalBasicDeficitNews_(data, ctx, C) {
  var alerts = POPS_findCriticalBasicDeficits_(data, ctx);
  if (!alerts.length) return;

  // ограничим, чтобы не спамить
  var max = Math.min(8, alerts.length);

  var parts = [];
  parts.push({ text: "Население: критический дефицит Basic\n", color: C });
  parts.push({ text: "┌────────────────────────────────────────────────────────┐n".replace("n", "\n"), color: C });

  for (var i = 0; i < max; i++) {
    var a = alerts[i];
    parts.push({ text: "┃ ➔ " + a.prov + ": " + a.goodsText + " | BasicSat=" + POPS_round_(a.basicSat, 2) + "\n", color: C });
  }

  if (alerts.length > max) {
    parts.push({ text: "┃ … ещё: " + (alerts.length - max) + "\n", color: C });
  }

  parts.push({ text: "└────────────────────────────────────────────────────────┘\n", color: C });

  pushNotice(data, { category: "Население", sub: "Дефицит", priority: 95, parts: parts });
}


/* ============ 4) FINANCIAL PRESSURE ============ */

function POPS_pushFinancialPressureNews_(data, ctx, C) {
  var fin = POPS_aggregateFinancialStats_(data, ctx);
  if (fin.groupsTotal <= 0) return;

  // новость только если есть смысл (пошлины или есть группы без денег или расходы большие)
  if (fin.groupsNoMoney <= 0 && fin.tariffShare < 0.1 && fin.spentTotal <= 0) return;

  var parts = [];
  parts.push({ text: "Население: финансовое давление\n", color: C });
  parts.push({ text: "┌────────────────────────────────────────────────────────┐\n", color: C });
  parts.push({ text: "┃ ➔ Расходы POP за ход: " + TRADE_fmtMoney_(fin.spentTotal) + "\n", color: C });
  parts.push({ text: "┃ ➔ Пошлины POP за ход: " + TRADE_fmtMoney_(fin.tariffsTotal) + "\n", color: C });
  parts.push({ text: "┃ ➔ Доля пошлин: " + POPS_round_(fin.tariffShare * 100, 1) + "%\n", color: C });
  parts.push({ text: "┃ ➔ Групп без денег: " + fin.groupsNoMoney + " / " + fin.groupsTotal + "\n", color: C });
  parts.push({ text: "└────────────────────────────────────────────────────────┘\n", color: C });

  pushNotice(data, { category: "Население", sub: "Финансы", priority: 78, parts: parts });
}


/* ============ 5) SOL + RADICALS/LOYALISTS ============ */

function POPS_pushSoLPoliticsNews_(data, ctx, C) {
  var pol = POPS_aggregateSoLPolitics_(data, ctx);
  if (pol.groupsTotal <= 0) return;

  // показываем если есть изменения/напряжение
  if (pol.avgRad < 0.02 && pol.avgLoy < 0.02) return;

  var parts = [];
  parts.push({ text: "Население: SoL и политическое напряжение\n", color: C });
  parts.push({ text: "┌────────────────────────────────────────────────────────┐\n", color: C });
  parts.push({ text: "┃ ➔ Средний уровень жизни (SoL): " + POPS_round_(pol.avgSol, 2) + "\n", color: C });
  parts.push({ text: "┃ ➔ Радикалы (средн., доля): " + POPS_round_(pol.avgRad * 100, 2) + "%\n", color: C });
  parts.push({ text: "┃ ➔ Лоялисты (средн., доля): " + POPS_round_(pol.avgLoy * 100, 2) + "%\n", color: C });
  parts.push({ text: "┃ ➔ Худшие провинции: " + (pol.worstText || "нет данных") + "\n", color: C });
  parts.push({ text: "└────────────────────────────────────────────────────────┘\n", color: C });

  pushNotice(data, { category: "Население", sub: "SoL", priority: 85, parts: parts });
}


/* =======================
   AGGREGATORS
   ======================= */

function POPS_aggregatePopStats_(data, ctx) {
  var out = {
    groupsTotal: 0,
    popTotal: 0,
    avgSat: 1,
    groupsWithShort: 0,
    subUsed: 0,
    topShortText: ""
  };

  var satSum = 0;
  var shortAgg = {};

  normalizeToArray(data.Население).forEach(function (row) {
    normalizeToArray(row).forEach(function (g) {
      if (!g || !g.Провинция) return;

      var p = ctx.provByKey[String(g.Провинция)];
      if (!p) return;
      if (ctx.stateId != null && String(p.Владелец || "") !== String(ctx.stateId)) return;

      var pop = POPS_getPopCount_(g);
      out.groupsTotal++;
      out.popTotal += pop;

      var sat = Number(g.Удовлетворение);
      if (!isFinite(sat)) sat = 1;
      satSum += sat;

      // нехватка
      var hadShort = false;
      var short = g.Нехватка || {};
      Object.keys(short).forEach(function (good) {
        var v = Math.floor(Number(short[good]) || 0);
        if (v <= 0) return;
        hadShort = true;
        shortAgg[good] = (shortAgg[good] || 0) + v;
      });
      if (hadShort) out.groupsWithShort++;

      // субституты
      var sub = g.СубститутыЗаХод || {};
      Object.keys(sub).forEach(function (good2) {
        out.subUsed += Math.floor(Number(sub[good2]) || 0);
      });
    });
  });

  out.avgSat = out.groupsTotal ? (satSum / out.groupsTotal) : 1;

  // top 3 shortage goods
  var top = Object.keys(shortAgg)
    .map(function (k) { return { g: k, v: shortAgg[k] }; })
    .sort(function (a, b) { return b.v - a.v; })
    .slice(0, 3);

  out.topShortText = top.length
    ? top.map(function (x) { return x.g + " (" + x.v + ")"; }).join(", ")
    : "";

  return out;
}

function POPS_findCriticalBasicDeficits_(data, ctx) {
  var alerts = [];

  normalizeToArray(data.Население).forEach(function (row) {
    normalizeToArray(row).forEach(function (g) {
      if (!g || !g.Провинция) return;

      var p = ctx.provByKey[String(g.Провинция)];
      if (!p) return;
      if (ctx.stateId != null && String(p.Владелец || "") !== String(ctx.stateId)) return;

      var basicSat = POPS_estimateBasicSatisfaction_(g);
      if (basicSat >= 0.6) return;

      // топ нехватка по Basic товарам
      var plan = (g.ПотребностиСлои && g.ПотребностиСлои.Basic) ? g.ПотребностиСлои.Basic : {};
      var short = g.Нехватка || {};
      var items = [];

      Object.keys(plan).forEach(function (good) {
        var v = Math.floor(Number(short[good]) || 0);
        if (v > 0) items.push({ g: good, v: v });
      });

      items.sort(function (a, b) { return b.v - a.v; });
      var goodsText = items.slice(0, 2).map(function (x) { return x.g + "(-" + x.v + ")"; }).join(", ");
      if (!goodsText) goodsText = "дефицит Basic";

      alerts.push({
        prov: String(g.Провинция),
        basicSat: basicSat,
        goodsText: goodsText
      });
    });
  });

  // худшие сначала
  alerts.sort(function (a, b) { return a.basicSat - b.basicSat; });
  return alerts;
}

function POPS_aggregateFinancialStats_(data, ctx) {
  var out = { groupsTotal: 0, groupsNoMoney: 0, spentTotal: 0, tariffsTotal: 0, tariffShare: 0 };

  normalizeToArray(data.Население).forEach(function (row) {
    normalizeToArray(row).forEach(function (g) {
      if (!g || !g.Провинция) return;

      var p = ctx.provByKey[String(g.Провинция)];
      if (!p) return;
      if (ctx.stateId != null && String(p.Владелец || "") !== String(ctx.stateId)) return;

      out.groupsTotal++;

      var money = Number(g.Деньги) || 0;
      if (money <= 0) out.groupsNoMoney++;

      out.spentTotal += (Number(g.РасходЗаХод) || 0);
      out.tariffsTotal += (Number(g.Пошлины) || 0);
    });
  });

  out.tariffShare = (out.spentTotal > 0) ? (out.tariffsTotal / out.spentTotal) : 0;
  return out;
}

function POPS_aggregateSoLPolitics_(data, ctx) {
  var out = { groupsTotal: 0, avgSol: 0, avgRad: 0, avgLoy: 0, worstText: "" };
  var solSum = 0, radSum = 0, loySum = 0;

  var worstByProv = {}; // prov -> rad
  normalizeToArray(data.Население).forEach(function (row) {
    normalizeToArray(row).forEach(function (g) {
      if (!g || !g.Провинция) return;

      var p = ctx.provByKey[String(g.Провинция)];
      if (!p) return;
      if (ctx.stateId != null && String(p.Владелец || "") !== String(ctx.stateId)) return;

      out.groupsTotal++;

      var sol = Number(g.УровеньЖизни) || 0;
      var rad = Number(g.Радикалы) || 0;
      var loy = Number(g.Лоялисты) || 0;

      solSum += sol;
      radSum += rad;
      loySum += loy;

      var prov = String(g.Провинция);
      worstByProv[prov] = Math.max(Number(worstByProv[prov]) || 0, rad);
    });
  });

  if (out.groupsTotal > 0) {
    out.avgSol = solSum / out.groupsTotal;
    out.avgRad = radSum / out.groupsTotal;
    out.avgLoy = loySum / out.groupsTotal;
  }

  var worst = Object.keys(worstByProv)
    .map(function (k) { return { prov: k, rad: worstByProv[k] }; })
    .sort(function (a, b) { return b.rad - a.rad; })
    .slice(0, 3);

  out.worstText = worst.length
    ? worst.map(function (x) { return x.prov + " (" + POPS_round_(x.rad * 100, 1) + "%)"; }).join(", ")
    : "";

  return out;
}

/* =========================================================
   POP: SOL + RADICALS/LOYALISTS (механика 5)
   - SoL считается от: денег на душу / стоимости базовой корзины / удовлетворения
   - Радикалы растут при проблемах Basic и падении SoL
   ========================================================= */

function POPS_updateSoLAndPolitics_(ctx) {
  var data = ctx.data;
  if (!data || !Array.isArray(data.Население)) return;

  var tpl = POPS_getNeedsTemplate_(data);
  var basicWeights = (tpl && tpl.strataWeights && tpl.strataWeights.Basic) ? Number(tpl.strataWeights.Basic) : 3;

  normalizeToArray(data.Население).forEach(function (row) {
    normalizeToArray(row).forEach(function (g) {
      if (!g || !g.Провинция) return;

      var p = ctx.provByKey[String(g.Провинция)];
      if (!p) return;

      // (опционально) только наши провинции
      if (ctx.stateId != null && String(p.Владелец || "") !== String(ctx.stateId)) return;

      POPS_ensurePoliticsFields_(g);

      var pop = POPS_getPopCount_(g);
      if (pop <= 0) return;

      var wealth = POPS_getWealth_(g);
      var moneyNow = Number(g.Деньги) || 0;
      var moneyPerCapita = moneyNow / pop;

      // стоимость базовой корзины на душу (с учётом "самый дешёвый доступный рынок")
      var basicCostPerCapita = POPS_calcBasicBasketCostPerCapita_({
        group: g,
        province: p,
        marketById: ctx.marketById,
        policyIdx: ctx.policyIdx,
        accessPool: ctx.accessPool
      });

      // satisfaction (0..1) — уже посчитан POPS_consumeAndScore_
      var sat = (g.Удовлетворение == null) ? 1 : Number(g.Удовлетворение);
      if (!isFinite(sat)) sat = 1;
      sat = Math.max(0, Math.min(1, sat));

      // SoL (0..20) — простая, но рабочая шкала:
      //  - базовая способность покупать корзину => SoL растёт
      //  - удовлетворение усиливает/ослабляет
      var affordability = (basicCostPerCapita > 0) ? (moneyPerCapita / basicCostPerCapita) : 10;
      var sol = 10 + 5 * Math.log(Math.max(0.1, affordability)); // центр ~10
      sol = sol * (0.7 + 0.3 * sat); // влияние удовлетворения
      sol = Math.max(0, Math.min(20, sol));

      var prev = Number(g.УровеньЖизни) || sol;
      g.УровеньЖизни = POPS_round_(sol, 2);

      // Радикалы/лоялисты (в долях населения)
      var basicSat = POPS_estimateBasicSatisfaction_(g); // грубая оценка по нехватке basic
      var solDelta = sol - prev;

      var rad = Number(g.Радикалы) || 0;   // 0..1
      var loy = Number(g.Лоялисты) || 0;   // 0..1

      // рост радикалов при плохом Basic и падении SoL
      rad += (basicSat < 0.6 ? 0.02 : 0) + (solDelta < -0.5 ? 0.015 : 0);
      // рост лоялистов при хорошем Basic и росте SoL
      loy += (basicSat > 0.85 ? 0.01 : 0) + (solDelta > 0.5 ? 0.01 : 0);

      // затухание
      rad *= 0.98;
      loy *= 0.99;

      g.Радикалы = Math.max(0, Math.min(1, POPS_round_(rad, 4)));
      g.Лоялисты = Math.max(0, Math.min(1, POPS_round_(loy, 4)));
    });
  });
}

// Стоимость BASIC корзины на душу, берём потребности Basic из g.ПотребностиСлои.Basic
function POPS_calcBasicBasketCostPerCapita_(ctx) {
  var g = ctx.group;
  var p = ctx.province;
  var plan = (g && g.ПотребностиСлои && g.ПотребностиСлои.Basic) ? g.ПотребностиСлои.Basic : {};
  var pop = POPS_getPopCount_(g);
  if (pop <= 0) return 0;

  var marketId = TRADE_getMarketId_(p);
  if (!marketId) return 0;

  var totalCost = 0;

  Object.keys(plan).forEach(function (good) {
    var qty = Number(plan[good]) || 0;
    if (!(qty > 0)) return;

    // минимальная эффективная цена из доступного (локальный + доступ)
    var unit = POPS_minEffectivePriceForPop_({
      dstMarketId: String(marketId),
      good: String(good),
      marketById: ctx.marketById,
      policyIdx: ctx.policyIdx,
      accessPool: ctx.accessPool
    });

    totalCost += qty * unit;
  });

  return totalCost / pop;
}

// “самая дешёвая доступная” цена для POP: локальный или любой рынок из accessPool (с импортной пошлиной)
function POPS_minEffectivePriceForPop_(ctx) {
  var dst = String(ctx.dstMarketId);
  var good = String(ctx.good);

  var best = TRADE_getPriceFromMarket_(ctx.marketById, dst, good); // локальный без пошлин

  var keys = Object.keys(ctx.accessPool || {});
  for (var i = 0; i < keys.length; i++) {
    var src = String(keys[i]);
    if (Math.floor(Number(ctx.accessPool[src]) || 0) <= 0) continue;

    // если импорт запрещён — не считаем
    if (ctx.policyIdx && !TRADE_policyIsAllowed_(ctx.policyIdx, dst, "Импорт", good)) continue;
    if (ctx.policyIdx && !TRADE_policyIsAllowed_(ctx.policyIdx, src, "Экспорт", good)) continue;

    var base = TRADE_getPriceFromMarket_(ctx.marketById, src, good);
    var importT = ctx.policyIdx ? TRADE_policyGetTariff_(ctx.policyIdx, dst, "Импорт", good) : 0;
    var eff = base * (1 + (Number(importT) || 0));
    if (eff < best) best = eff;
  }

  if (!isFinite(best) || best <= 0) best = 1;
  return best;
}

// Грубая оценка Basic удовлетворения: если есть нехватка по базовым товарам — падает
function POPS_estimateBasicSatisfaction_(g) {
  // если нет данных о нехватке — считаем ок
  var plan = (g && g.ПотребностиСлои && g.ПотребностиСлои.Basic) ? g.ПотребностиСлои.Basic : null;
  if (!plan) return 1;

  var short = g.Нехватка || {};
  var needSum = 0, shortSum = 0;

  Object.keys(plan).forEach(function (good) {
    var need = Math.floor(Number(plan[good]) || 0);
    if (need <= 0) return;
    needSum += need;
    shortSum += Math.floor(Number(short[good]) || 0);
  });

  if (needSum <= 0) return 1;
  var sat = 1 - (shortSum / needSum);
  return Math.max(0, Math.min(1, sat));
}

function POPS_ensurePoliticsFields_(g) {
  if (g.УровеньЖизни == null || !isFinite(Number(g.УровеньЖизни))) g.УровеньЖизни = 10;
  if (g.Радикалы == null || !isFinite(Number(g.Радикалы))) g.Радикалы = 0;
  if (g.Лоялисты == null || !isFinite(Number(g.Лоялисты))) g.Лоялисты = 0;
}

function POPS_round_(x, digits) {
  var n = Number(x) || 0;
  var d = Math.pow(10, digits || 0);
  return Math.round(n * d) / d;
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

