/* =========================================================
   ТОРГОВЛЯ: РАСЧЁТ ЦЕН (Вариант A, Vic3-like)
   - Цена = basePrice * (1 + k * imbalance)
   - clamp по min/max множителям
   - инерция (плавное приближение к target)
   - считаем ТОЛЬКО для рынков, где есть хотя бы одна НАША провинция

   Базовые цены и параметры берём из data["Глобальные настройки"] (1 столбец):
   - ищем ячейку с JSON-объектом, содержащим блок цен
   - если не нашли — создаём дефолтный блок + дополняем товарами

   Требования проекта:
   - normalizeToArray(value)
   - ensureNews(data)
   - pushNotice(data, {category, sub, priority, parts})
   - (опционально) safeParse(v)  // если есть, используем

   Ожидаемый формат глобальных настроек (создастся автоматически):
   {
     "Цены": {
       "defaults": { "k": 2.0, "inertia": 0.25, "minMult": 0.25, "maxMult": 2.75 },
       "товары": {
         "Зерно": { "basePrice": 20, "k": 2.2, "inertia": 0.30, "minMult": 0.25, "maxMult": 2.75 },
         ...
       }
     }
   }
   ========================================================= */

function TRADE_computePrices(data) {
  if (!data || typeof data !== "object") return data;

  if (!Array.isArray(data.Торговля)) data.Торговля = [];

  // 1) provinces: первая ячейка-массив из столбца "Провинции"
  var provinces = TRADE_pickFirstArrayCell_(data.Провинции);

  // 2) рынки, где присутствует хотя бы одна НАША провинция
  var ourMarkets = TRADE_collectOurMarketsFromProvinces_(data, provinces);

  // Если нет наших рынков — можно тихо выйти (или написать короткую новость)
  if (Object.keys(ourMarkets).length === 0) {
    TRADE_pushPriceNotice_RU_(data, {
      marketsTouched: 0,
      goodsTouched: 0,
      priceUpdates: 0,
      settingsCreated: false,
      goodsSettingsAdded: 0,
      topChanges: []
    }, "Нет рынков с нашими провинциями — цены не пересчитывались.");
    return data;
  }

  // 3) universe товаров из шаблонов (строго Входы/Выходы)
  var goodsSet = TRADE_collectGoodsSetFromTemplatesColumn_STRICT_(data["Шаблоны зданий"]);
  var goodsList = Object.keys(goodsSet);

  // 4) загрузить/создать глобальные настройки цен и гарантировать записи по товарам
  var settingsRes = TRADE_getOrCreatePriceSettings_(data, goodsList);
  var priceCfg = settingsRes.cfg;        // { defaults, товары }
  var settingsCreated = settingsRes.created;
  var goodsSettingsAdded = settingsRes.goodsAdded;

  // 5) пересчёт цен для рынков-объектов в столбце Торговля
  var rep = {
    marketsTouched: 0,
    goodsTouched: 0,
    priceUpdates: 0,
    settingsCreated: settingsCreated,
    goodsSettingsAdded: goodsSettingsAdded,
    topChanges: [] // [{marketId, good, oldPrice, newPrice, deltaPct}]
  };

  for (var r = 0; r < data.Торговля.length; r++) {
    var cell = data.Торговля[r];
    if (!cell || typeof cell !== "object" || Array.isArray(cell)) continue;

    var mid = TRADE_marketIdFromMarketCell_RU_(cell);
    if (!mid) continue;

    // Считаем ТОЛЬКО если в этом рынке есть хотя бы одна НАША провинция
    if (!ourMarkets[mid]) continue;

    if (!cell["Товары"] || typeof cell["Товары"] !== "object") cell["Товары"] = {};
    var goods = cell["Товары"];

    rep.marketsTouched++;

    Object.keys(goods).forEach(function (g) {
      var st = goods[g];
      if (!st || typeof st !== "object") return;

      // Вариант A использует только Спрос/Предложение (остальное — игнор)
      var D = Number(st["Спрос"]);
      var S = Number(st["Предложение"]);

      if (isNaN(D)) D = 0;
      if (isNaN(S)) S = 0;

      // Настройки товара (с fallback на defaults)
      var gcfg = (priceCfg.товары && priceCfg.товары[g]) ? priceCfg.товары[g] : null;

      var basePrice = TRADE_numOr_(gcfg && gcfg.basePrice, TRADE_numOr_(st["Базовая цена"], 1));
      var k = TRADE_numOr_(gcfg && gcfg.k, priceCfg.defaults.k);
      var inertia = TRADE_numOr_(gcfg && gcfg.inertia, priceCfg.defaults.inertia);
      var minMult = TRADE_numOr_(gcfg && gcfg.minMult, priceCfg.defaults.minMult);
      var maxMult = TRADE_numOr_(gcfg && gcfg.maxMult, priceCfg.defaults.maxMult);

      basePrice = Math.max(0.0001, basePrice);
      inertia = TRADE_clamp_(inertia, 0, 1);
      if (minMult <= 0) minMult = 0.01;
      if (maxMult < minMult) {
        var tmp = maxMult;
        maxMult = minMult;
        minMult = tmp;
      }

      // imbalance: (D - S) / max(1, D + S)
      var denom = Math.max(1, D + S);
      var imbalance = (D - S) / denom; // [-1..1] примерно

      // target
      var target = basePrice * (1 + k * imbalance);

      // clamp относительно basePrice
      var minP = basePrice * minMult;
      var maxP = basePrice * maxMult;
      target = TRADE_clamp_(target, minP, maxP);

      // current price
      var cur = Number(st["Цена"]);
      if (isNaN(cur) || cur <= 0) cur = basePrice;

      // smooth
      var next = cur + (target - cur) * inertia;

      // Стабилизация на мелких значениях
      if (!isFinite(next) || next <= 0) next = basePrice;

      // запись
      if (Math.abs(next - cur) > 1e-9) {
        st["Цена"] = next;
        rep.priceUpdates++;

        // сохранить в топ-изменения (по проценту)
        var deltaPct = (cur === 0) ? 0 : ((next - cur) / cur) * 100;
        rep.topChanges.push({
          marketId: mid,
          good: g,
          oldPrice: cur,
          newPrice: next,
          deltaPct: deltaPct
        });
      }

      rep.goodsTouched++;
    });
  }

  // 6) TOP-10 самых сильных изменений цены
  rep.topChanges.sort(function (a, b) { return Math.abs(b.deltaPct) - Math.abs(a.deltaPct); });
  if (rep.topChanges.length > 10) rep.topChanges.length = 10;

  // 7) новость-отчёт (1 карточка, единый тёмно-серый)
  TRADE_pushPriceNotice_RU_(data, rep);

  return data;
}

/* =======================
   OUR MARKETS: сбор рынков, где есть хотя бы одна наша провинция
   ======================= */

function TRADE_collectOurMarketsFromProvinces_(data, provinces) {
  var out = {};

  // пытаемся определить ID нашего государства
  var ourId = null;
  if (typeof getStateIdSafe === "function") {
    try { ourId = getStateIdSafe(data); } catch (e) {}
  }
  if (ourId != null) ourId = String(ourId).trim();

  for (var i = 0; i < provinces.length; i++) {
    var p = provinces[i];
    if (!p || typeof p !== "object") continue;

    if (!TRADE_isOurProvince_(p, ourId)) continue;

    var mid = TRADE_marketIdFromProvince_(p);
    if (mid) out[mid] = true;
  }

  return out;
}

// максимально “терпимый” детектор "наша провинция"
function TRADE_isOurProvince_(p, ourId) {
  // если у нас нет ourId — считаем "не наша" (безопаснее, чем пересчитывать всё)
  if (ourId == null || String(ourId).trim() === "") return false;

  // частые поля
  var candidates = [
    p["ID государства"], p["Идентификатор государства"], p["ГосударствоId"], p["StateId"],
    p["СтранаId"], p["ID страны"], p["Идентификатор страны"],
    p.stateId, p.countryId, p.ownerId, p.ВладелецId, p.Владелец
  ];

  for (var i = 0; i < candidates.length; i++) {
    var v = candidates[i];
    if (v == null) continue;
    if (String(v).trim() === String(ourId).trim()) return true;
  }

  // если провинция хранит объект владельца
  if (p.Владелец && typeof p.Владелец === "object") {
    var v2 = p.Владелец.id || p.Владелец.ID || p.Владелец["Идентификатор"];
    if (v2 != null && String(v2).trim() === String(ourId).trim()) return true;
  }

  return false;
}

/* =======================
   GLOBAL SETTINGS: найти/создать блок цен в "Глобальные настройки"
   ======================= */

function TRADE_getOrCreatePriceSettings_(data, goodsList) {
  if (!Array.isArray(data["Глобальные настройки"])) data["Глобальные настройки"] = [];
  var col = data["Глобальные настройки"];

  // найти ячейку с JSON-объектом, где есть "Цены"
  var foundIdx = -1;
  var foundObj = null;

  for (var r = 0; r < col.length; r++) {
    var cell = col[r];
    var obj = TRADE_tryParseJsonCell_(cell);
    if (!obj || typeof obj !== "object" || Array.isArray(obj)) continue;

    if (obj["Цены"] && typeof obj["Цены"] === "object") {
      foundIdx = r;
      foundObj = obj;
      break;
    }
  }

  var created = false;

  if (foundIdx === -1) {
    // создать дефолтный объект настроек
    foundObj = {
      "Цены": {
        "defaults": { "k": 2.0, "inertia": 0.25, "minMult": 0.25, "maxMult": 2.75 },
        "товары": {}
      }
    };

    // положить в первый свободный слот
    var slot = TRADE_findFirstFreeRow_(col);
    if (slot === -1) {
      col.push(foundObj);
      foundIdx = col.length - 1;
    } else {
      col[slot] = foundObj;
      foundIdx = slot;
    }

    created = true;
  } else {
    // нормализовать структуру
    if (!foundObj["Цены"]) foundObj["Цены"] = {};
    if (!foundObj["Цены"]["defaults"] || typeof foundObj["Цены"]["defaults"] !== "object") {
      foundObj["Цены"]["defaults"] = { "k": 2.0, "inertia": 0.25, "minMult": 0.25, "maxMult": 2.75 };
    }
    if (!foundObj["Цены"]["товары"] || typeof foundObj["Цены"]["товары"] !== "object") {
      foundObj["Цены"]["товары"] = {};
    }
  }

  var cfg = foundObj["Цены"];
  // гарантируем defaults
  cfg.defaults = cfg.defaults || {};
  if (cfg.defaults.k == null) cfg.defaults.k = 2.0;
  if (cfg.defaults.inertia == null) cfg.defaults.inertia = 0.25;
  if (cfg.defaults.minMult == null) cfg.defaults.minMult = 0.25;
  if (cfg.defaults.maxMult == null) cfg.defaults.maxMult = 2.75;

  // дополнить товарами
  var added = 0;
  var tmap = cfg["товары"];
  for (var i = 0; i < goodsList.length; i++) {
    var g = goodsList[i];
    if (!g) continue;
    if (!tmap[g] || typeof tmap[g] !== "object") {
      // дефолтная базовая цена = 1 (можно потом руками настроить)
      tmap[g] = { "basePrice": 1 };
      added++;
    } else {
      if (tmap[g].basePrice == null || isNaN(Number(tmap[g].basePrice))) {
        tmap[g].basePrice = 1;
      }
    }
  }

  // важно: записываем объект назад в ячейку (если в таблице требуются именно объекты)
  col[foundIdx] = foundObj;

  return { cfg: cfg, created: created, goodsAdded: added };
}

function TRADE_tryParseJsonCell_(cell) {
  // если уже объект
  if (cell && typeof cell === "object" && !Array.isArray(cell)) return cell;

  // если строка — пробуем парсить
  if (typeof cell === "string") {
    var s = cell.trim();
    if (!s) return null;

    if (typeof safeParse === "function") {
      var v = safeParse(cell);
      if (v && typeof v === "object" && !Array.isArray(v)) return v;
      return null;
    }

    if (s[0] === "{" || s[0] === "[") {
      try {
        var o = JSON.parse(s);
        if (o && typeof o === "object" && !Array.isArray(o)) return o;
      } catch (e) {}
    }
  }

  return null;
}

/* =======================
   NEWS: отчёт по ценам (1 карточка, единый тёмно-серый)
   ======================= */

function TRADE_pushPriceNotice_RU_(data, rep, noteLine) {
  if (typeof ensureNews === "function") ensureNews(data);
  if (typeof pushNotice !== "function") return;

  var C = "#6E675F"; // единый тёмно-серый
  function line(parts, s) { parts.push({ text: String(s) + "\n", color: C }); }

  var parts = [];
  line(parts, "Торговля: пересчёт цен (Vic3-like, вариант A)");
  line(parts, "┌────────────────────────────────────────────────────────┐");

  if (noteLine) {
    line(parts, "┃ ➔ " + String(noteLine));
    line(parts, "┃");
  }

  line(parts, "┃ ➔ Рынков затронуто: " + String(rep.marketsTouched));
  line(parts, "┃ ➔ Товаров обработано: " + String(rep.goodsTouched));
  line(parts, "┃ ➔ Обновлений цены: " + String(rep.priceUpdates));
  line(parts, "┃");
  line(parts, "┃ ➔ Настройки цен: " + (rep.settingsCreated ? "созданы" : "найдены"));
  line(parts, "┃ ➔ Добавлено товаров в настройки: " + String(rep.goodsSettingsAdded));

  if (rep.topChanges && rep.topChanges.length) {
    line(parts, "┃");
    line(parts, "┃ ➔ TOP-изменения цены (до 10):");
    for (var i = 0; i < rep.topChanges.length; i++) {
      var t = rep.topChanges[i];
      var dp = (isFinite(t.deltaPct) ? t.deltaPct : 0);
      line(parts, "┃    - Рынок " + t.marketId + ": " + t.good +
        "  " + TRADE_round2_(t.oldPrice) + " → " + TRADE_round2_(t.newPrice) +
        "  (" + TRADE_round2_(dp) + "%)");
    }
  }

  line(parts, "└────────────────────────────────────────────────────────┘");

  pushNotice(data, {
    category: "Торговля",
    sub: "Цены",
    priority: 90,
    parts: parts
  });
}

/* =======================
   Small utils
   ======================= */

function TRADE_numOr_(v, dflt) {
  var n = Number(v);
  return (isNaN(n) ? dflt : n);
}

function TRADE_clamp_(x, a, b) {
  x = Number(x);
  if (isNaN(x)) return a;
  return Math.max(a, Math.min(b, x));
}

function TRADE_round2_(x) {
  x = Number(x);
  if (!isFinite(x)) return "0";
  return (Math.round(x * 100) / 100).toString();
}

/* =======================
   Helpers (из вашего текущего модуля — оставляю совместимые)
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