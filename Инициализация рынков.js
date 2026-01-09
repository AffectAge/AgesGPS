/* =========================================================
   ТОРГОВЛЯ: рынки в СТОЛБЦЕ (1 рынок = 1 ячейка)
   ✅ Ключи рынков/товаров — НА РУССКОМ
   ✅ НЕ ТРОГАЕТ существующие рынки (не меняет структуру/товары/цены)
   ✅ Делает ТОЛЬКО:
      + создаёт рынок, если он есть у провинции, но НЕТ данных в столбце Торговля
      - удаляет рынок, если к нему НЕ принадлежит ни одна провинция

   Ожидаемая модель:
   - data.Торговля = Array (столбец по строкам), каждая ячейка:
       { "Идентификатор рынка": "Азиатский", "Товары": { ... } }
   - data.Провинции = Array (столбец), в одной из ячеек хранится Array провинций
   - data["Шаблоны зданий"] = Array (столбец), каждая ячейка объект/массив шаблонов
     Товары берём строго из Входы/Выходы

   Требования проекта:
   - normalizeToArray(value)
   ========================================================= */

function TRADE_syncMarkets(data) {
  if (!data || typeof data !== "object") return data;

  // ensure column exists
  if (!Array.isArray(data.Торговля)) data.Торговля = [];

  // 1) provinces: берём первую ячейку-массив из столбца "Провинции"
  var provinces = TRADE_pickFirstArrayCell_(data.Провинции);

  // 2) set рынков, которые реально присутствуют в провинциях
  var present = {}; // marketId -> true
  for (var i = 0; i < provinces.length; i++) {
    var p = provinces[i];
    if (!p || typeof p !== "object") continue;

    var mid = TRADE_marketIdFromProvince_(p);
    if (mid) present[mid] = true;
  }

  // 3) товары по шаблонам (нужны ТОЛЬКО для создания НОВЫХ рынков)
  var goodsSet = TRADE_collectGoodsSetFromTemplatesColumn_STRICT_(data["Шаблоны зданий"]);

  // 4) индекс существующих рынков в столбце Торговля
  //    ВАЖНО: существующие рынки НЕ МЕНЯЕМ — только индексируем
  var byId = {}; // marketId -> rowIndex
  for (var r = 0; r < data.Торговля.length; r++) {
    var cell = data.Торговля[r];
    if (!cell || typeof cell !== "object" || Array.isArray(cell)) continue;

    var id = TRADE_marketIdFromMarketCell_RU_(cell);
    if (!id) continue;

    byId[id] = r;
  }

  // 5) + создать рынки, которых нет в data.Торговля, но есть в провинциях
  Object.keys(present).forEach(function (mid) {
    if (byId[mid] !== undefined) return; // уже есть данные — не трогаем

    var marketObj = TRADE_makeDefaultMarket_RU_(data, mid, goodsSet);

    var slot = TRADE_findFirstFreeRow_(data.Торговля);
    if (slot === -1) {
      data.Торговля.push(marketObj);
      slot = data.Торговля.length - 1;
    } else {
      data.Торговля[slot] = marketObj;
    }

    byId[mid] = slot;
  });

  // 6) - удалить рынки, к которым не принадлежит ни одна провинция
  //    (ОЧИЩАЕМ ячейку, не схлопываем столбец)
  Object.keys(byId).forEach(function (mid) {
    if (present[mid]) return;

    var idx = byId[mid];
    data.Торговля[idx] = ""; // свободная ячейка
    delete byId[mid];
  });

  return data;
}

/* =======================
   Helpers
   ======================= */

// Берёт первую ячейку-ARRAY из колонки (data.Провинции = массив по строкам)
function TRADE_pickFirstArrayCell_(col) {
  if (!Array.isArray(col)) return [];
  for (var i = 0; i < col.length; i++) {
    if (Array.isArray(col[i])) return col[i];
  }
  return [];
}

// market id из провинции
function TRADE_marketIdFromProvince_(p) {
  if (!p || typeof p !== "object") return null;
  if (p.РынокId != null && String(p.РынокId).trim() !== "") return String(p.РынокId).trim();
  if (p.Рынок != null && String(p.Рынок).trim() !== "") return String(p.Рынок).trim();
  return null;
}

// market id из ячейки рынка (русские ключи)
function TRADE_marketIdFromMarketCell_RU_(m) {
  if (!m || typeof m !== "object") return null;

  // основной ключ
  if (m["Идентификатор рынка"] != null && String(m["Идентификатор рынка"]).trim() !== "") {
    return String(m["Идентификатор рынка"]).trim();
  }

  // мягкая совместимость со старым
  if (m.РынокId != null && String(m.РынокId).trim() !== "") return String(m.РынокId).trim();
  if (m.id != null && String(m.id).trim() !== "") return String(m.id).trim();

  return null;
}

// Свободная ячейка: null/undefined/"" или не-объект или массив
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

// Собираем товары строго из Входы/Выходы по всей колонке шаблонов
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

/* =======================
   Создание НОВОГО рынка (русские ключи)
   - existing рынки не трогаем, только новые инициализируем
   ======================= */

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

// Дефолт товара (русские ключи). Базовую цену можно позже централизовать.
function TRADE_defaultMarketGoodState_RU_(data, good) {
  var basePrice = 1;

  // Опционально: если заведёшь глобальный справочник цен:
  // data.ТорговляСправочникТоваров = { "Железо": { "Базовая цена": 12 }, ... }
  if (data && data.ТорговляСправочникТоваров && data.ТорговляСправочникТоваров[good]) {
    var bp = data.ТорговляСправочникТоваров[good]["Базовая цена"];
    if (bp !== undefined && bp !== null && !isNaN(Number(bp))) basePrice = Number(bp);
  }

  return {
    "Цена": basePrice,

    // stockpile
    "Запас": 0,

    // потоки за ход (ты будешь заполнять другими функциями)
    "Поток предложения": 0,
    "Поток спроса": 0,
    "Нехватка спроса": 0,

    // статистика (опционально)
    "Куплено": 0,
    "Продано": 0
  };
}