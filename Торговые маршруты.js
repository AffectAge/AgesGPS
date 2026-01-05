/* =========================================================
   ЛОГИСТИКА РЫНКОВ (ВАРИАНТ 3) + НОВОСТИ В СТИЛЕ "РЫНКА ТРУДА"
   Google Apps Script (V8)

   ✅ Считает пропускную способность только для наших провинций,
      но маршруты строит по ВСЕМ провинциям рынка (транзит по рынку разрешён).
   ✅ Столица рынка определяется по p.СтолицаРынка === true.
   ✅ Морские пути проходят только по провинциям с морским ландшафтом (SEA_TERRAINS).
   ✅ Пишет 1 новость на рынок в формате "ячейки" как рынок труда.

   Требования проекта:
   - normalizeToArray(value)  (у тебя уже есть)
   - ensureNews(data)         (у тебя уже есть)
   - pushNotice(data, {category, sub, priority, parts})  (у тебя уже есть)

   Вызов на ход:
   LOGISTICS_computeOurThroughputByMarkets(data);

   ========================================================= */


/* =======================
   НАСТРОЙКИ
   ======================= */

// Морские ландшафты: только по ним можно строить SEA-сеть
var SEA_TERRAINS = {
  "Океан": true,
  "Море": true,
  "Пролив": true,
  "Ледяное море": true
};

// Маппинг типов построек -> категория инфраструктуры
// Подстрой под свои названия "Тип" в data.Постройки
var INFRA_TYPE_TO_CAT = {
  "Дороги": "roads",
  "Железная дорога": "rails",
  "Порт": "ports",
  "Аэропорт": "airports",
  "Космопорт": "spaceports",
  "Логистический центр": "hubs"
};

// Коэффициент потерь при пересадке LAND <-> (SEA/AIR/SPACE)
var TRANSFER_FACTOR = 0.85;

// Ёмкость морского ребра между морскими провинциями (очень большая)
var SEA_EDGE_CAP = 1e12;


/* =======================
   ПОЛЯ ПРОВИНЦИЙ / НОРМАЛИЗАЦИЯ
   ======================= */

// Ключ провинции
function LOG_getProvKey_(p) {
  return p.Провинция || p.Название || p.id || null;
}

// Рынок провинции
function LOG_getMarketId_(p) {
  if (p.РынокId !== undefined && p.РынокId !== null) return String(p.РынокId);
  if (p.Рынок !== undefined && p.Рынок !== null) return String(p.Рынок);
  return null;
}

// Ландшафт: поддержка строка или массив ["Равнины"]
function LOG_getTerrain_(p) {
  var t = p.Ландшафт || p.Террейн || p.Terrain || null;
  if (Array.isArray(t)) return t.length ? String(t[0]) : "";
  return (t === null || t === undefined) ? "" : String(t);
}


/* =======================
   ГОСУДАРСТВО: stateId (как у тебя в рынке труда)
   ======================= */

function LOG_getStateIdFromStateData_(data) {
  var root = data ? data["Данные государства"] : null;
  if (root === null || root === undefined) return null;

  var flat = [];
  normalizeToArray(root).forEach(function (row) {
    normalizeToArray(row).forEach(function (cell) { flat.push(cell); });
  });

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


/* =======================
   ПРОВИНЦИИ: все (flat)
   ======================= */

function LOG_getAllProvincesFlat_(data) {
  if (!Array.isArray(data.Провинции)) return [];
  return normalizeToArray(data.Провинции)
    .reduce(function (acc, row) { return acc.concat(normalizeToArray(row)); }, [])
    .filter(function (p) { return p && typeof p === "object" && LOG_getProvKey_(p); });
}


/* =======================
   НАШИ ПРОВИНЦИИ -> markets
   ======================= */

function LOG_groupOurProvincesByMarket_(provinces, stateId) {
  var out = {}; // marketId -> [provKey]
  provinces.forEach(function (p) {
    var pk = LOG_getProvKey_(p);
    if (!pk) return;

    if (String(p.Владелец || "") !== String(stateId)) return;

    var marketId = LOG_getMarketId_(p);
    if (!marketId) return;

    if (!out[marketId]) out[marketId] = [];
    out[marketId].push(pk);
  });
  return out;
}

function LOG_getMarketScope_(provinces, marketId) {
  var keys = [];
  var capitalKey = null;
  var capitalCount = 0;

  provinces.forEach(function (p) {
    var pk = LOG_getProvKey_(p);
    if (!pk) return;

    var mid = LOG_getMarketId_(p);
    if (mid !== String(marketId)) return;

    keys.push(pk);

    if (p.СтолицаРынка === true) {
      capitalKey = pk;
      capitalCount++;
    }
  });

  return { marketProvKeys: keys, capitalKey: capitalKey, capitalCount: capitalCount };
}


/* =======================
   ИНФРАСТРУКТУРА ПО ПРОВИНЦИЯМ (только активные постройки)
   ======================= */

function LOG_buildInfraByProvince_(data) {
  var infra = {}; // pk -> {roads, rails, ports, airports, spaceports, hubs}

  function ensure(pk) {
    if (!infra[pk]) infra[pk] = { roads:0, rails:0, ports:0, airports:0, spaceports:0, hubs:0 };
    return infra[pk];
  }

  if (!Array.isArray(data.Постройки)) return infra;

  normalizeToArray(data.Постройки).forEach(function (row) {
    normalizeToArray(row).forEach(function (b) {
      if (!b || !b.Провинция || !b.Тип) return;
      if (b.Активно !== true) return;

      var pk = String(b.Провинция);
      var type = String(b.Тип);

      var cat = INFRA_TYPE_TO_CAT[type];
      if (!cat) return;

      var obj = ensure(pk);
      obj[cat] = (obj[cat] || 0) + 1;
    });
  });

  return infra;
}


/* =======================
   ЁМКОСТИ РЕЖИМОВ (балансируй)
   ======================= */

function LOG_capLand_(inf) {
  var base = 50;
  if (!inf) return base;
  return base + inf.roads*80 + inf.rails*160 + inf.hubs*120;
}
function LOG_capSea_(inf) {
  if (!inf || inf.ports <= 0) return 0;
  var base = 100;
  return base + inf.ports*300 + inf.hubs*80;
}
function LOG_capAir_(inf) {
  if (!inf || inf.airports <= 0) return 0;
  var base = 60;
  return base + inf.airports*220 + inf.hubs*60;
}
function LOG_capSpace_(inf) {
  if (!inf || inf.spaceports <= 0) return 0;
  var base = 40;
  return base + inf.spaceports*500 + inf.hubs*100;
}


/* =======================
   ГРАФ (multimodal)
   ======================= */

function LOG_nodeId_(provKey, mode) {
  return String(provKey) + ":" + String(mode);
}

function LOG_capitalStartNodes_(capitalKey, infraByProv) {
  var inf = infraByProv[capitalKey] || null;

  var nodes = [ LOG_nodeId_(capitalKey, "LAND") ];
  if (LOG_capSea_(inf) > 0) nodes.push(LOG_nodeId_(capitalKey, "SEA"));
  if (LOG_capAir_(inf) > 0) nodes.push(LOG_nodeId_(capitalKey, "AIR"));
  if (LOG_capSpace_(inf) > 0) nodes.push(LOG_nodeId_(capitalKey, "SPACE"));

  return nodes;
}

function LOG_buildMultimodalGraph_(provByKey, marketProvKeys, infraByProv) {
  var graph = {}; // node -> [{to, cap}]
  var marketSet = {};
  marketProvKeys.forEach(function (k) { marketSet[k] = true; });

  function addEdge(a, b, cap) {
    if (!(cap > 0)) return;
    if (!graph[a]) graph[a] = [];
    graph[a].push({ to: b, cap: cap });
  }
  function addUndir(a, b, cap) { addEdge(a,b,cap); addEdge(b,a,cap); }

  // Морские провинции внутри рынка
  var isSeaProv = {};
  marketProvKeys.forEach(function (pk) {
    var p = provByKey[pk];
    if (!p) return;
    var t = LOG_getTerrain_(p);
    isSeaProv[pk] = !!SEA_TERRAINS[String(t)];
  });

  // 1) Пересадки внутри провинции (LAND <-> SEA/AIR/SPACE)
  marketProvKeys.forEach(function (pk) {
    var inf = infraByProv[pk] || null;

    var land  = LOG_nodeId_(pk, "LAND");
    var sea   = LOG_nodeId_(pk, "SEA");
    var air   = LOG_nodeId_(pk, "AIR");
    var space = LOG_nodeId_(pk, "SPACE");

    var cLand  = LOG_capLand_(inf);
    var cSea   = LOG_capSea_(inf);
    var cAir   = LOG_capAir_(inf);
    var cSpace = LOG_capSpace_(inf);

    if (cSea > 0)   addUndir(land, sea,   Math.min(cLand, cSea) * TRANSFER_FACTOR);
    if (cAir > 0)   addUndir(land, air,   Math.min(cLand, cAir) * TRANSFER_FACTOR);
    if (cSpace > 0) addUndir(land, space, Math.min(cLand, cSpace) * TRANSFER_FACTOR);
  });

  // 2) LAND соседство внутри рынка
  marketProvKeys.forEach(function (pk) {
    var p = provByKey[pk];
    if (!p) return;

    var infA = infraByProv[pk] || null;
    var aLand = LOG_nodeId_(pk, "LAND");
    var aCap = LOG_capLand_(infA);

    var neigh = normalizeToArray(p.Соседи);
    neigh.forEach(function (nk) {
      if (!marketSet[nk]) return;
      var infB = infraByProv[nk] || null;
      var cap = Math.min(aCap, LOG_capLand_(infB));
      addUndir(aLand, LOG_nodeId_(nk, "LAND"), cap);
    });
  });

  // 3) SEA: море только по морским провинциям (ландшафт whitelist)
  // 3.1) Морские ребра: seaTile <-> seaTile по соседям
  marketProvKeys.forEach(function (pk) {
    if (!isSeaProv[pk]) return;
    var p = provByKey[pk];
    if (!p) return;

    var aSea = LOG_nodeId_(pk, "SEA");
    var neigh = normalizeToArray(p.Соседи);
    neigh.forEach(function (nk) {
      if (!marketSet[nk]) return;
      if (!isSeaProv[nk]) return;
      addUndir(aSea, LOG_nodeId_(nk, "SEA"), SEA_EDGE_CAP);
    });
  });

  // 3.2) Порты подключаем к соседним морским провинциям
  marketProvKeys.forEach(function (pk) {
    var inf = infraByProv[pk] || null;
    var portCap = LOG_capSea_(inf);
    if (!(portCap > 0)) return;

    var p = provByKey[pk];
    if (!p) return;

    var seaNode = LOG_nodeId_(pk, "SEA");
    var neigh = normalizeToArray(p.Соседи);

    neigh.forEach(function (nk) {
      if (!marketSet[nk]) return;
      if (!isSeaProv[nk]) return;
      addUndir(seaNode, LOG_nodeId_(nk, "SEA"), portCap);
    });
  });

  // 4) AIR: полносвязно внутри рынка между провинциями с аэропортом
  var airProvs = marketProvKeys.filter(function (pk) {
    return LOG_capAir_(infraByProv[pk] || null) > 0;
  });
  for (var i = 0; i < airProvs.length; i++) {
    for (var j = i + 1; j < airProvs.length; j++) {
      var a = airProvs[i], b = airProvs[j];
      var capAir = Math.min(
        LOG_capAir_(infraByProv[a] || null),
        LOG_capAir_(infraByProv[b] || null)
      );
      addUndir(LOG_nodeId_(a, "AIR"), LOG_nodeId_(b, "AIR"), capAir);
    }
  }

  // 5) SPACE: полносвязно внутри рынка между провинциями с космопортом
  var spaceProvs = marketProvKeys.filter(function (pk) {
    return LOG_capSpace_(infraByProv[pk] || null) > 0;
  });
  for (var i2 = 0; i2 < spaceProvs.length; i2++) {
    for (var j2 = i2 + 1; j2 < spaceProvs.length; j2++) {
      var a2 = spaceProvs[i2], b2 = spaceProvs[j2];
      var capSpace = Math.min(
        LOG_capSpace_(infraByProv[a2] || null),
        LOG_capSpace_(infraByProv[b2] || null)
      );
      addUndir(LOG_nodeId_(a2, "SPACE"), LOG_nodeId_(b2, "SPACE"), capSpace);
    }
  }

  return graph;
}


/* =======================
   WIDEST PATH (max bottleneck) — multi-source
   ======================= */

function LOG_MaxHeap_() { this.arr = []; }
LOG_MaxHeap_.prototype.push = function (item) {
  var a = this.arr;
  a.push(item);
  var i = a.length - 1;
  while (i > 0) {
    var p = (i - 1) >> 1;
    if (a[p].cap >= a[i].cap) break;
    var tmp = a[p]; a[p] = a[i]; a[i] = tmp;
    i = p;
  }
};
LOG_MaxHeap_.prototype.pop = function () {
  var a = this.arr;
  if (!a.length) return null;
  var top = a[0];
  var last = a.pop();
  if (a.length) {
    a[0] = last;
    var i = 0;
    while (true) {
      var l = i * 2 + 1, r = l + 1;
      var largest = i;
      if (l < a.length && a[l].cap > a[largest].cap) largest = l;
      if (r < a.length && a[r].cap > a[largest].cap) largest = r;
      if (largest === i) break;
      var tmp = a[i]; a[i] = a[largest]; a[largest] = tmp;
      i = largest;
    }
  }
  return top;
};
LOG_MaxHeap_.prototype.size = function () { return this.arr.length; };

function LOG_widestPathMultiSource_(graph, startNodes, startCaps) {
  var best = {}; // node -> cap
  var used = {};
  var heap = new LOG_MaxHeap_();

  for (var i = 0; i < startNodes.length; i++) {
    var n = startNodes[i];
    var c = (startCaps && startCaps[n] !== undefined) ? startCaps[n] : 0;

    if (!(c > 0)) continue;     // нет ёмкости — нет смысла стартовать
    best[n] = c;
    heap.push({ node: n, cap: c });
  }

  while (heap.size()) {
    var cur = heap.pop();
    if (!cur) break;

    var u = cur.node;
    if (used[u]) continue;
    used[u] = true;

    var edges = graph[u] || [];
    for (var k = 0; k < edges.length; k++) {
      var e = edges[k];
      var v = e.to;
      var cand = Math.min(best[u], e.cap);

      if (best[v] === undefined || cand > best[v]) {
        best[v] = cand;
        heap.push({ node: v, cap: cand });
      }
    }
  }

  return best;
}

/* =======================
   НОВОСТИ (как рынок труда): 1 карточка на рынок
   ======================= */

function LOG_fmtInt_(n) {
  n = Math.floor(Number(n) || 0);
  return String(n).replace(/\B(?=(\d{3})+(?!\d))/g, " ");
}

function LOG_modeLabel_(mode) {
  mode = String(mode || "LAND").toUpperCase();
  if (mode === "SEA") return "SEA (море)";
  if (mode === "AIR") return "AIR (авиа)";
  if (mode === "SPACE") return "SPACE (космос)";
  return "LAND (суша)";
}

function LOG_buildMarketReport_(marketId, capitalKey, ourKeys, provByKey) {
  var sum = 0, cnt = 0, isolated = 0;
  var modeCount = { LAND:0, SEA:0, AIR:0, SPACE:0 };
  var worst = [];
  var top = [];

  for (var i = 0; i < ourKeys.length; i++) {
    var pk = ourKeys[i];
    var p = provByKey[pk];
    if (!p) continue;

    var cap = Number(p.ПропускнаяСпособность) || 0;
    var mode = (p.Логистика && p.Логистика.mode) ? String(p.Логистика.mode).toUpperCase() : "LAND";

    sum += cap;
    cnt++;

    if (!modeCount[mode]) modeCount[mode] = 0;
    modeCount[mode]++;

    top.push({ prov: pk, cap: cap, mode: mode });

    if (cap <= 0) {
      isolated++;
      worst.push({ prov: pk, reason: "нет маршрута до столицы" });
    }
  }

  top.sort(function(a,b){ return b.cap - a.cap; });
  top = top.slice(0, 5);

  if (worst.length > 10) worst = worst.slice(0, 10);

  var mainMode = "LAND";
  var bestC = -1;
  for (var m in modeCount) {
    if (modeCount[m] > bestC) { bestC = modeCount[m]; mainMode = m; }
  }

  var avg = cnt ? (sum / cnt) : 0;

  return {
    marketId: String(marketId),
    capital: String(capitalKey || ""),
    ourCount: cnt,
    isolatedCount: isolated,
    avgCap: avg,
    mainMode: mainMode,
    worst: worst,
    top: top
  };
}

function LOG_pushMarketNotice_(data, report) {
  ensureNews(data);

  var ORANGE = "#FF8C00";
  var RED = "#E36A6A";
  var LABEL = "#CFC7BA";
  var VALUE = "#E6E6FA";

  var parts = [];

  parts.push({ text: "Логистика рынка\n", bold: true, color: ORANGE });
  parts.push({ text: "┌────────────────────────────────────────────────────────┐\n", color: ORANGE });

  parts.push({ text: "┃", bold: true, color: ORANGE });
  parts.push({ text: " ➔ Рынок: ", bold: true, color: LABEL });
  parts.push({ text: report.marketId + "\n", bold: true, color: VALUE });

  parts.push({ text: "┃", bold: true, color: ORANGE });
  parts.push({ text: " ➔ Столица рынка: ", bold: true, color: LABEL });
  parts.push({ text: report.capital + "\n", bold: true, color: VALUE });

  parts.push({ text: "┃", bold: true, color: ORANGE });
  parts.push({ text: " ➔ Наших провинций: ", bold: true, color: LABEL });
  parts.push({ text: String(report.ourCount) + "\n", bold: true, color: VALUE });

  parts.push({ text: "┃", bold: true, color: ORANGE });
  parts.push({ text: " ➔ Изолированных: ", bold: true, color: LABEL });
  parts.push({ text: String(report.isolatedCount) + "\n", bold: true, color: (report.isolatedCount > 0 ? RED : VALUE) });

  parts.push({ text: "┃", bold: true, color: ORANGE });
  parts.push({ text: " ➔ Средняя пропускная способность: ", bold: true, color: LABEL });
  parts.push({ text: LOG_fmtInt_(report.avgCap) + "\n", bold: true, color: VALUE });

  parts.push({ text: "┃", bold: true, color: ORANGE });
  parts.push({ text: " ➔ Основной режим: ", bold: true, color: LABEL });
  parts.push({ text: LOG_modeLabel_(report.mainMode) + "\n", bold: true, color: VALUE });

  if (report.worst && report.worst.length) {
    parts.push({ text: "┃\n", bold: true, color: ORANGE });

    parts.push({ text: "┃", bold: true, color: ORANGE });
    parts.push({ text: " ➔ Проблемные провинции:\n", bold: true, color: LABEL });

    for (var i = 0; i < report.worst.length; i++) {
      var w = report.worst[i];

      parts.push({ text: "┃", bold: true, color: ORANGE });
      parts.push({ text: "   • ", bold: true, color: LABEL });
      parts.push({ text: String(w.prov) + "\n", bold: true, color: VALUE });

      parts.push({ text: "┃", bold: true, color: ORANGE });
      parts.push({ text: "     причина: ", bold: true, color: LABEL });
      parts.push({ text: String(w.reason) + "\n", bold: true, color: RED });
    }
  }

  if (report.top && report.top.length) {
    parts.push({ text: "┃\n", bold: true, color: ORANGE });

    parts.push({ text: "┃", bold: true, color: ORANGE });
    parts.push({ text: " ➔ Ключевые узлы:\n", bold: true, color: LABEL });

    for (var j = 0; j < report.top.length; j++) {
      var t = report.top[j];

      parts.push({ text: "┃", bold: true, color: ORANGE });
      parts.push({ text: "   • ", bold: true, color: LABEL });
      parts.push({ text: String(t.prov) + " — ", bold: true, color: VALUE });
      parts.push({ text: LOG_fmtInt_(t.cap), bold: true, color: VALUE });
      parts.push({ text: " (" + String(t.mode) + ")\n", bold: true, color: VALUE });
    }
  }

  parts.push({ text: "└────────────────────────────────────────────────────────┘n", color: ORANGE }); // NB: \n ниже фикс
  // фикс переноса строки (в GAS иногда лучше явным push)
  parts[parts.length - 1].text = "└────────────────────────────────────────────────────────┘\n";

  pushNotice(data, {
    category: "Логистика",
    sub: "Пропускная способность рынка",
    priority: 90,
    parts: parts
  });
}


/* =======================
   ОСНОВНАЯ ФУНКЦИЯ: расчёт по рынкам
   ======================= */

function LOGISTICS_computeOurThroughputByMarkets(data) {
  if (!data || typeof data !== "object") return data;

  ensureNews(data);

  var provinces = LOG_getAllProvincesFlat_(data);
  if (!provinces.length) return data;

  var stateId = LOG_getStateIdFromStateData_(data);

  if (!stateId) {
    // Если хочешь — можешь заменить на свою ошибку как в рынке труда
    pushNotice(data, {
      category: "Логистика",
      sub: "Ошибка",
      priority: 999,
      parts: [
        { text: "Ошибка данных государства\n", bold: true, color: "#E36A6A" },
        { text: "┌────────────────────────────────────────────────────────┐\n", color: "#E36A6A" },
        { text: "┃", bold: true, color: "#E36A6A" },
        { text: " ➔ Причина: ", bold: true, color: "#CFC7BA" },
        { text: "не найден \"Идентификатор государства\" в data[\"Данные государства\"]\n", bold: true, color: "#E6E6FA" },
        { text: "└────────────────────────────────────────────────────────┘\n", color: "#E36A6A" }
      ]
    });
    return data;
  }
  stateId = String(stateId);

  // Индексы по провинциям
  var provByKey = {};
  provinces.forEach(function (p) {
    var pk = LOG_getProvKey_(p);
    if (pk) provByKey[pk] = p;
  });

  // Наши провинции по рынкам
  var ourByMarket = LOG_groupOurProvincesByMarket_(provinces, stateId);
  var marketIds = Object.keys(ourByMarket);
  if (!marketIds.length) return data;

  // Инфраструктура по провинциям
  var infraByProv = LOG_buildInfraByProvince_(data);

  // Для каждого рынка, где у нас есть провинции
  marketIds.forEach(function (marketId) {
    var ourKeys = ourByMarket[marketId] || [];
    if (!ourKeys.length) return;

    var scope = LOG_getMarketScope_(provinces, marketId);
    if (!scope.marketProvKeys.length) return;

    // столица рынка
    if (!scope.capitalKey || scope.capitalCount !== 1) {
      pushNotice(data, {
        category: "Логистика",
        sub: "Ошибка",
        priority: 800,
        parts: [
          { text: "Ошибка данных рынка\n", bold: true, color: "#E36A6A" },
          { text: "┌────────────────────────────────────────────────────────┐\n", color: "#E36A6A" },

          { text: "┃", bold: true, color: "#E36A6A" },
          { text: " ➔ Рынок: ", bold: true, color: "#CFC7BA" },
          { text: String(marketId) + "\n", bold: true, color: "#E6E6FA" },

          { text: "┃", bold: true, color: "#E36A6A" },
          { text: " ➔ Причина: ", bold: true, color: "#CFC7BA" },
          { text: "столица рынка не определена однозначно (СтолицаРынка:true)\n", bold: true, color: "#E6E6FA" },

          { text: "┃", bold: true, color: "#E36A6A" },
          { text: " ➔ Найдено столиц: ", bold: true, color: "#CFC7BA" },
          { text: String(scope.capitalCount) + "\n", bold: true, color: "#E36A6A" },

          { text: "└────────────────────────────────────────────────────────┘\n", color: "#E36A6A" }
        ]
      });
      return;
    }

    // граф рынка
    var graph = LOG_buildMultimodalGraph_(provByKey, scope.marketProvKeys, infraByProv);

// стартовые узлы столицы
var startNodes = LOG_capitalStartNodes_(scope.capitalKey, infraByProv);

// ✅ стартовые ёмкости зависят от построек в столице (как у остальных провинций)
var startCaps = {};
for (var si = 0; si < startNodes.length; si++) {
  var node = startNodes[si];
  var mode = String(node.split(":").pop()); // LAND/SEA/AIR/SPACE
  startCaps[node] = LOG_nodeBaseCap_(scope.capitalKey, mode, infraByProv);
}

// widest path
var best = LOG_widestPathMultiSource_(graph, startNodes, startCaps);

    // запись результата: только в НАШИ провинции этого рынка
    for (var i = 0; i < ourKeys.length; i++) {
      var pk = ourKeys[i];
      var p = provByKey[pk];
      if (!p) continue;

      // safety: рынок должен совпасть
      if (LOG_getMarketId_(p) !== String(marketId)) continue;

      var capLand  = best[LOG_nodeId_(pk, "LAND")]  || 0;
      var capSea   = best[LOG_nodeId_(pk, "SEA")]   || 0;
      var capAir   = best[LOG_nodeId_(pk, "AIR")]   || 0;
      var capSpace = best[LOG_nodeId_(pk, "SPACE")] || 0;

      var cap = Math.max(capLand, capSea, capAir, capSpace);

      p.ПропускнаяСпособность = cap;
      p.ДоступКСтолице = cap > 0;

      // mode
      var mode = "LAND";
      var bestCap = capLand;
      if (capSea > bestCap) { bestCap = capSea; mode = "SEA"; }
      if (capAir > bestCap) { bestCap = capAir; mode = "AIR"; }
      if (capSpace > bestCap) { bestCap = capSpace; mode = "SPACE"; }

      p.Логистика = {
        рынок: String(marketId),
        cap: cap,
        mode: mode,
        isolated: !(cap > 0)
      };
    }

    // push 1 news card per market
    var report = LOG_buildMarketReport_(marketId, scope.capitalKey, ourKeys, provByKey);
    LOG_pushMarketNotice_(data, report);
  });

  return data;
}


/* =======================
   Утилиты
   ======================= */

function LOG_nodeBaseCap_(provKey, mode, infraByProv) {
  var inf = infraByProv[provKey] || null;
  mode = String(mode || "LAND").toUpperCase();

  if (mode === "SEA") return LOG_capSea_(inf);
  if (mode === "AIR") return LOG_capAir_(inf);
  if (mode === "SPACE") return LOG_capSpace_(inf);
  return LOG_capLand_(inf); // LAND
}