/* =========================================================
   PRODUCTION: Потребление -> Добыча -> Производство (V2, CANON)
   Google Apps Script (V8)

   ТВОИ ПРАВИЛА (канонично):
   0) Обрабатываем ТОЛЬКО здания:
      - b.Активно === true
      - b.Провинция принадлежит нашему государству (prov.Владелец == stateId)

   1) ЭффективностьТруда = доля рабочих от необходимых (0..1)
      - СНИЖАЕТ потребление (cap) и СНИЖАЕТ добычу/производство
      - Здание НЕ потребляет больше, чем (Входы * уровень * ЭффективностьТруда)
        даже если на складе больше.

   2) Потребление идёт ТОЛЬКО со склада здания b.Склад.
      Нехватка пишется в b.Нехватка (до cap).

   3) Мощность шага (stepEff) зависит от:
      - покрытия входов относительно FULL (без труда): coverage = min(consumed / requiredFull)
      - труда: laborEff
      stepEff = min(coverage, laborEff)

      => если laborEff=0.7 и склад полон, coverage=0.7 => stepEff=0.7

   4) Добыча:
      - qtyWant = floor(Добыча * уровень * stepEff)
      - ограничено prov.Ресурсы[ресурс]
      - prov.Ресурсы уменьшаем, добытое кладём в b.Склад

   5) Выходы:
      - qty = floor(Выходы * уровень * stepEff)
      - кладём в b.Склад

   6) Базовые Входы/Выходы/Добыча:
      - приоритет: поля здания (b.Входы/Выходы/Добыча)
      - fallback: tpl.Входы/Выходы/Добыча из data["Шаблоны зданий"]

   7) Новости:
      - если у тебя в проекте есть ensureNews/pushBoxNotice/uiTitle/uiTop/uiRow/uiBottom/UI — будет итоговая новость.
      - если нет — модуль тихо отработает без новостей.

   ПУБЛИЧНАЯ ТОЧКА ВХОДА:
     PROD_runTurn(data)

   ========================================================= */

var PROD_CFG = {
  CATEGORY: "Постройки",
  SUB_TURN: "Производство (ход)",

  // Как масштабировать по уровню:
  // "LINEAR": mult = level (уровень 3 => x3)
  // "GROWTH": mult = 1 + k*(level-1)  (k берём из tpl.LevelGrowthK или b.LevelGrowthK)
  LEVEL_MODE: "LINEAR",
  DEFAULT_GROWTH_K: 0.8,

  // Пишем диагностические поля в здания (удобно для отладки)
  WRITE_DEBUG_FIELDS: true
};

/* =======================
   PUBLIC ENTRY
   ======================= */

function PROD_runTurn(data) {
  if (!data || typeof data !== "object") return data;

  if (typeof ensureNews === "function") ensureNews(data);

  var stateId = (typeof getStateIdFromStateData === "function")
    ? getStateIdFromStateData(data)
    : null;

  if (!stateId) {
    PROD_pushSystem_(data, "STATE_ID_NOT_FOUND",
      "Идентификатор государства не найден в data['Данные государства'] (поле 'Идентификатор государства').");
    return data;
  }

  // Провинции + карта наших
  var provinces = (typeof getAllProvinces === "function") ? getAllProvinces(data) : [];
  var provByKey = {};
  var ourMap = {};

  for (var i = 0; i < provinces.length; i++) {
    var p = provinces[i];
    if (!p || typeof p !== "object") continue;
    var key = p.Провинция || p.Название || p.id;
    if (!key) continue;
    key = String(key);

    provByKey[key] = p;

    if (String(p.Владелец || "") === String(stateId)) {
      ourMap[key] = true;
    }

    // гарантируем форму ресурсов
    if (!p.Ресурсы || typeof p.Ресурсы !== "object" || Array.isArray(p.Ресурсы)) p.Ресурсы = {};
  }

  // Шаблоны
  var TEMPLATES = PROD_indexTemplates_(data);

  // Постройки
  var rows = (typeof normalizeToArray === "function")
    ? normalizeToArray(data.Постройки)
    : (Array.isArray(data.Постройки) ? data.Постройки : []);

  var processed = 0;
  var skippedInactive = 0;
  var skippedForeign = 0;

  for (var r = 0; r < rows.length; r++) {
    var row = (typeof normalizeToArray === "function")
      ? normalizeToArray(rows[r])
      : (Array.isArray(rows[r]) ? rows[r] : [rows[r]]);

    for (var c = 0; c < row.length; c++) {
      var b = row[c];
      if (!b || typeof b !== "object") continue;

      // 0) только активные
      if (b.Активно !== true) { skippedInactive++; continue; }

      // 0) только наши провинции
      var provKey = String(b.Провинция || "").trim();
      if (!provKey || !ourMap[provKey]) { skippedForeign++; continue; }

      var prov = provByKey[provKey] || ((typeof findProvince === "function") ? findProvince(provinces, provKey) : null);
      if (!prov) continue;

      // shape
      PROD_ensureBuildingShape_(b);

      // template (fallback)
      var tpl = (b.Тип && TEMPLATES[b.Тип]) ? TEMPLATES[b.Тип] : null;

      // базовые потоки
      var baseIn  = PROD_pickFlowObject_(b.Входы,  tpl && tpl.Входы);
      var baseOut = PROD_pickFlowObject_(b.Выходы, tpl && tpl.Выходы);
      var baseExt = PROD_pickFlowObject_(b.Добыча, tpl && tpl.Добыча);

      // уровень
      var level = PROD_getLevel_(b);
      var levelMult = PROD_levelMult_(b, tpl, level);

      // труд (0..1), по умолчанию 1
      var laborEff = PROD_clamp01_(b._ЭффективностьТруда == null ? 1 : Number(b._ЭффективностьТруда));

      // 1) потребление
      var cons = PROD_consumeInputsCanon_(b, baseIn, levelMult, laborEff);

      // 3) канонично: stepEff = min(coverage(full), laborEff)
      var stepEff = Math.min(cons.inputCoverage, laborEff);
      stepEff = PROD_clamp01_(stepEff);

      // 2) добыча
      var ext = PROD_extractFromProvince_(b, prov, baseExt, levelMult, stepEff);

      // 3) выходы
      var prod = PROD_produceOutputs_(b, baseOut, levelMult, stepEff);

      // запись нехватки (до CAP)
      b.Нехватка = cons.missing;

      if (PROD_CFG.WRITE_DEBUG_FIELDS) {
        b._LaborEff = laborEff;
        b._Level = level;
        b._LevelMult = levelMult;

        b._CoverageFull = cons.inputCoverage;     // consumed/full
        b._CoverageCap = cons.coverageCap;        // consumed/cap (удобно видеть, хватает ли на cap)
        b._StepEff = stepEff;

        b._Consumed = cons.consumed;
        b._RequiredFull = cons.requiredFull;
        b._RequiredCap = cons.requiredCap;

        b._Extracted = ext.extracted;
        b._Produced = prod.produced;
      }

      processed++;
    }
  }

  // Итоговая новость (если UI доступен)
  PROD_pushTurnSummary_(data, {
    processed: processed,
    skippedInactive: skippedInactive,
    skippedForeign: skippedForeign
  });

  return data;
}

/* =======================
   STEP 1: consume inputs (CANON)
   - full = base * levelMult
   - cap  = full * laborEff
   - take = min(stock, cap)
   - missing = cap - take
   - coverage(full) = take / full
   ======================= */

function PROD_consumeInputsCanon_(b, baseIn, levelMult, laborEff) {
  var stock = b.Склад;

  var requiredFull = {};
  var requiredCap = {};
  var consumed = {};
  var missing = {};

  var covFull = 1;   // min coverage vs FULL
  var covCap = 1;    // min coverage vs CAP

  var goods = Object.keys(baseIn || {});
  for (var i = 0; i < goods.length; i++) {
    var g = goods[i];
    var base = Math.floor(Number(baseIn[g]) || 0);
    if (base <= 0) continue;

    var full = Math.floor(base * levelMult);
    if (full < 0) full = 0;

    var cap = Math.floor(full * laborEff);
    if (cap < 0) cap = 0;

    requiredFull[g] = full;
    requiredCap[g] = cap;

    var have = Math.floor(Number(stock[g]) || 0);
    if (have < 0) have = 0;

    // НЕ больше cap
    var take = Math.min(have, cap);
    if (take < 0) take = 0;

    if (take > 0) stock[g] = have - take;

    consumed[g] = take;

    var miss = Math.max(0, cap - take);
    if (miss > 0) missing[g] = miss;

    // coverage vs FULL (для мощности)
    var cgFull = (full <= 0) ? 1 : (take / full);
    if (cgFull < covFull) covFull = cgFull;

    // coverage vs CAP (для отладки)
    var cgCap = (cap <= 0) ? 1 : (take / cap);
    if (cgCap < covCap) covCap = cgCap;
  }

  covFull = PROD_clamp01_(covFull);
  covCap = PROD_clamp01_(covCap);

  return {
    requiredFull: requiredFull,
    requiredCap: requiredCap,
    consumed: consumed,
    missing: missing,
    inputCoverage: covFull,
    coverageCap: covCap
  };
}

/* =======================
   STEP 2: extraction from province
   - want = floor(base * levelMult * stepEff)
   - take = min(want, prov.Ресурсы[g])
   - prov.Ресурсы уменьшаем, take кладём на b.Склад
   ======================= */

function PROD_extractFromProvince_(b, prov, baseExt, levelMult, stepEff) {
  var extracted = {};

  if (!baseExt || typeof baseExt !== "object" || Array.isArray(baseExt)) {
    return { extracted: extracted };
  }

  if (!prov.Ресурсы || typeof prov.Ресурсы !== "object" || Array.isArray(prov.Ресурсы)) {
    prov.Ресурсы = {};
  }

  var goods = Object.keys(baseExt);
  for (var i = 0; i < goods.length; i++) {
    var g = goods[i];
    var base = Math.floor(Number(baseExt[g]) || 0);
    if (base <= 0) continue;

    var want = Math.floor(base * levelMult * stepEff);
    if (want <= 0) continue;

    var haveProv = Math.floor(Number(prov.Ресурсы[g]) || 0);
    if (haveProv < 0) haveProv = 0;

    var take = Math.min(want, haveProv);
    if (take <= 0) continue;

    prov.Ресурсы[g] = haveProv - take;

    b.Склад[g] = Math.floor(Number(b.Склад[g]) || 0) + take;
    extracted[g] = take;
  }

  return { extracted: extracted };
}

/* =======================
   STEP 3: produce outputs
   - qty = floor(base * levelMult * stepEff)
   ======================= */

function PROD_produceOutputs_(b, baseOut, levelMult, stepEff) {
  var produced = {};

  if (!baseOut || typeof baseOut !== "object" || Array.isArray(baseOut)) {
    return { produced: produced };
  }

  var goods = Object.keys(baseOut);
  for (var i = 0; i < goods.length; i++) {
    var g = goods[i];
    var base = Math.floor(Number(baseOut[g]) || 0);
    if (base <= 0) continue;

    var qty = Math.floor(base * levelMult * stepEff);
    if (qty <= 0) continue;

    b.Склад[g] = Math.floor(Number(b.Склад[g]) || 0) + qty;
    produced[g] = qty;
  }

  return { produced: produced };
}

/* =======================
   Templates index (optional)
   ======================= */

function PROD_indexTemplates_(data) {
  var T = {};
  var arr = (typeof normalizeToArray === "function")
    ? normalizeToArray(data["Шаблоны зданий"])
    : (Array.isArray(data["Шаблоны зданий"]) ? data["Шаблоны зданий"] : []);

  for (var i = 0; i < arr.length; i++) {
    var row = (typeof normalizeToArray === "function")
      ? normalizeToArray(arr[i])
      : (Array.isArray(arr[i]) ? arr[i] : [arr[i]]);

    for (var j = 0; j < row.length; j++) {
      var t = row[j];
      if (t && typeof t === "object" && t.Тип) {
        T[String(t.Тип)] = t;
      }
    }
  }
  return T;
}

/* =======================
   Level helpers
   ======================= */

function PROD_getLevel_(b) {
  var lvl = 1;
  if (b.Уровень != null) lvl = Number(b.Уровень);
  else if (b["Ур."] != null) lvl = Number(b["Ур."]);
  lvl = Math.floor(lvl || 1);
  if (lvl < 1) lvl = 1;
  return lvl;
}

function PROD_levelMult_(b, tpl, level) {
  if (PROD_CFG.LEVEL_MODE === "GROWTH") {
    var k = null;
    if (tpl && tpl.LevelGrowthK != null) k = Number(tpl.LevelGrowthK);
    else if (b.LevelGrowthK != null) k = Number(b.LevelGrowthK);
    if (isNaN(k) || k == null) k = PROD_CFG.DEFAULT_GROWTH_K;

    var mult = 1 + k * (level - 1);
    if (!isFinite(mult) || mult < 0) mult = 1;
    return mult;
  }
  // LINEAR
  return level;
}

/* =======================
   Shapes / utils
   ======================= */

function PROD_pickFlowObject_(primary, fallback) {
  if (primary && typeof primary === "object" && !Array.isArray(primary)) return primary;
  if (fallback && typeof fallback === "object" && !Array.isArray(fallback)) return fallback;
  return {};
}

function PROD_ensureBuildingShape_(b) {
  if (!b.Склад || typeof b.Склад !== "object" || Array.isArray(b.Склад)) b.Склад = {};
  if (!b.Нехватка || typeof b.Нехватка !== "object" || Array.isArray(b.Нехватка)) b.Нехватка = {};
}

function PROD_clamp01_(x) {
  x = Number(x);
  if (!isFinite(x)) return 0;
  if (x < 0) return 0;
  if (x > 1) return 1;
  return x;
}

/* =======================
   News / reporting (optional)
   ======================= */

function PROD_pushTurnSummary_(data, s) {
  if (typeof pushBoxNotice !== "function" ||
      typeof uiTitle !== "function" ||
      typeof uiTop !== "function" ||
      typeof uiRow !== "function" ||
      typeof uiBottom !== "function" ||
      typeof UI !== "object") {
    return;
  }

  var parts = [];
  uiTitle(parts, "Производство: итог хода", UI.BORDER);
  uiTop(parts, UI.BORDER);

  uiRow(parts, "Обработано зданий", String(s.processed || 0), UI.VALUE, UI.BORDER);
  uiRow(parts, "Пропущено (Активно=false)", String(s.skippedInactive || 0), UI.VALUE, UI.BORDER);
  uiRow(parts, "Пропущено (чужие провинции)", String(s.skippedForeign || 0), UI.VALUE, UI.BORDER);

  uiBottom(parts, UI.BORDER);

  pushBoxNotice(data, {
    category: PROD_CFG.CATEGORY,
    sub: PROD_CFG.SUB_TURN,
    priority: 160,
    parts: parts
  });
}

function PROD_pushSystem_(data, code, message) {
  // если есть твой pushErrorNotice — используем
  if (typeof pushErrorNotice === "function") {
    pushErrorNotice(data, code, message);
    return;
  }

  // иначе попробуем через box notice
  if (typeof pushBoxNotice !== "function" || typeof UI !== "object") return;

  var parts = [];
  var bc = UI.BAD || "#E36A6A";

  if (typeof uiTitle === "function") uiTitle(parts, "Ошибка", bc);
  if (typeof uiTop === "function") uiTop(parts, bc);

  if (typeof uiRow === "function") {
    uiRow(parts, "Код", code, UI.VALUE, bc);
    uiRow(parts, "Причина", message, UI.VALUE, bc);
  }

  if (typeof uiBottom === "function") uiBottom(parts, bc);

  pushBoxNotice(data, {
    category: "Система",
    sub: "Производство",
    priority: 999,
    parts: parts
  });
}