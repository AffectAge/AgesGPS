/* =========================================================
   TURNS: Gate + Commit (для runGame) + МИРОВОЙ ХОД В "Данные мира"
   Google Apps Script (V8)

   ИСТОЧНИК ИСТИНЫ:
   ✅ data["Данные мира"] содержит {"Ход": N}

   ПО СТРАНАМ:
   ✅ data["Последний ход"][i] — последний завершённый мировой ход для страны i
   ✅ если "Последний ход" пусто/нет -> дефолт = (ХодМира - 1) => страна ДОЛЖНА сходить
   ✅ если "Активность стран" пусто/нет -> true
   ✅ игнорируем строки, где нет ID или Названия

   Требования:
   - data["ID Страны"]    = [1,2,3,...]
   - data["Список стран"] = ["Франция","Германия",...]
   - data["Данные мира"]  = JSON (объект/массив объектов/строка JSON/2D) или пусто

   Зависимости (должны быть в проекте):
   - safeParseJSONCell_(v)
   - flattenCells_(value)
   - findFieldObj_(arr, key)
   - getFieldValue_(arr, key)
   - setFieldValue_(arr, key, value)
   - getStateDataArray_(data)  // для чтения "Идентификатор государства"
   ========================================================= */


/* =======================
   NUM HELPERS
   ======================= */

function TURN_numOrNaN_(v) {
  if (v === "" || v == null) return NaN; // пустое = нет значения
  var n = Number(v);
  return isFinite(n) ? n : NaN;
}


/* =======================
   WORLD JSON ARRAY: "Данные мира"
   ======================= */

function getWorldDataArray_(data) {
  var raw = data["Данные мира"];
  raw = safeParseJSONCell_(raw);

  // массив (1D/2D)
  if (Array.isArray(raw)) {
    var flat = flattenCells_(raw).filter(function (x) {
      return x && typeof x === "object" && !Array.isArray(x);
    });
    data["Данные мира"] = flat;
    return data["Данные мира"];
  }

  // одиночный объект
  if (raw && typeof raw === "object") {
    data["Данные мира"] = [raw];
    return data["Данные мира"];
  }

  // пусто/невалидно
  data["Данные мира"] = [];
  return data["Данные мира"];
}


/* =======================
   WORLD TURN SOURCE
   1) "Данные мира"."Ход"
   2) иначе: max(Последний ход активных) + 1
   3) иначе: 1
   ======================= */

function TURN_getWorldTurn_(data) {
  if (!data || typeof data !== "object") return 1;

  // 1) из "Данные мира"
  var worldArr = getWorldDataArray_(data);
  var t = Math.floor(TURN_numOrNaN_(getFieldValue_(worldArr, "Ход")));
  if (isFinite(t) && t > 0) return t;

  // 2) иначе выводим из массива последних ходов
  var idsRaw   = Array.isArray(data["ID Страны"]) ? data["ID Страны"] : [];
  var namesRaw = Array.isArray(data["Список стран"]) ? data["Список стран"] : [];
  var lastRaw  = Array.isArray(data["Последний ход"]) ? data["Последний ход"] : [];
  var actRaw   = Array.isArray(data["Активность стран"]) ? data["Активность стран"] : [];

  if (!idsRaw.length || idsRaw.length !== namesRaw.length) return 1;

  var maxLast = -1;

  for (var i = 0; i < idsRaw.length; i++) {
    var idStr = (idsRaw[i] == null) ? "" : String(idsRaw[i]).trim();
    var nmStr = (namesRaw[i] == null) ? "" : String(namesRaw[i]).trim();
    if (idStr === "" || nmStr === "") continue;

    var a = (i < actRaw.length) ? actRaw[i] : null;
    a = (a === false) ? false : true;
    if (a === false) continue;

    var lt = (i < lastRaw.length) ? Math.floor(TURN_numOrNaN_(lastRaw[i])) : NaN;
    if (!isFinite(lt)) continue;

    if (lt > maxLast) maxLast = lt;
  }

  if (maxLast >= 0) return maxLast + 1;

  return 1;
}


/* =======================
   ENSURE: мировой ход + списки стран
   - LastTurn default = worldTurn - 1 (страна должна сходить)
   - Active default = true
   - игнорировать строки без ID/Название
   ======================= */

function TURN_ensureWorldAndCountryTurns_(data) {
  if (!data || typeof data !== "object") return;

  var worldTurn = TURN_getWorldTurn_(data);

  // гарантируем "Ход" в "Данные мира"
  var worldArr = getWorldDataArray_(data);
  setFieldValue_(worldArr, "Ход", worldTurn);

  // нормализация массивов стран
  TURN_normalizeCountryLists_(data, worldTurn);
}


/* =======================
   PRIVATE: normalize/align country arrays
   ======================= */

function TURN_normalizeCountryLists_(data, worldTurn) {
  var idsRaw   = Array.isArray(data["ID Страны"]) ? data["ID Страны"] : [];
  var namesRaw = Array.isArray(data["Список стран"]) ? data["Список стран"] : [];

  if (!idsRaw.length || idsRaw.length !== namesRaw.length) return;

  var lastRaw = Array.isArray(data["Последний ход"]) ? data["Последний ход"] : [];
  var actRaw  = Array.isArray(data["Активность стран"]) ? data["Активность стран"] : [];

  var ids = [];
  var names = [];
  var last = [];
  var act = [];

  for (var i = 0; i < idsRaw.length; i++) {
    var idVal = idsRaw[i];
    var nameVal = namesRaw[i];

    var idStr = (idVal == null) ? "" : String(idVal).trim();
    if (idStr === "") continue;

    var nameStr = (nameVal == null) ? "" : String(nameVal).trim();
    if (nameStr === "") continue;

    // LastTurn: пустое/нет -> worldTurn - 1
    var ltRaw = (i < lastRaw.length) ? lastRaw[i] : null;
    var lt = Math.floor(TURN_numOrNaN_(ltRaw));
    if (!isFinite(lt)) lt = worldTurn - 1;
    if (lt < 0) lt = 0;

    // Activity: пустое/нет -> true
    var aRaw = (i < actRaw.length) ? actRaw[i] : null;
    var a = (aRaw === false) ? false : true;

    ids.push(idVal);
    names.push(nameStr);
    last.push(lt);
    act.push(a);
  }

  data["ID Страны"] = ids;
  data["Список стран"] = names;
  data["Последний ход"] = last;
  data["Активность стран"] = act;
}


/* =======================
   INDEX: страна по ID
   ======================= */

function TURN_findCountryIndexById_(data, countryId) {
  var ids = Array.isArray(data["ID Страны"]) ? data["ID Страны"] : [];
  var sid = String(countryId).trim();
  for (var i = 0; i < ids.length; i++) {
    if (String(ids[i]).trim() === sid) return i;
  }
  return -1;
}


/* =======================
   CAN ACT: проверка хода страны
   ======================= */

function TURN_canCountryAct_(data, countryId) {
  TURN_ensureWorldAndCountryTurns_(data);

  var worldArr = getWorldDataArray_(data);
  var turn = Math.floor(TURN_numOrNaN_(getFieldValue_(worldArr, "Ход")));
  if (!isFinite(turn) || turn <= 0) turn = 1;

  var idx = TURN_findCountryIndexById_(data, countryId);
  if (idx < 0) return { ok: false, reason: "страна не найдена", turn: turn };

  var actArr = data["Активность стран"];
  if (actArr && actArr[idx] === false) {
    return { ok: false, reason: "страна неактивна", idx: idx, turn: turn };
  }

  var lastArr = data["Последний ход"];
  var last = Math.floor(TURN_numOrNaN_(lastArr[idx]));
  if (!isFinite(last)) last = turn - 1;

  if (last >= turn) {
    return { ok: false, reason: "страна уже сделала ход " + turn, idx: idx, turn: turn, last: last };
  }

  return { ok: true, idx: idx, turn: turn, last: last };
}


/* =======================
   COMMIT: страна завершила текущий ход
   ======================= */

function TURN_markCountryDone_(data, countryId) {
  var chk = TURN_canCountryAct_(data, countryId);
  if (!chk.ok) return chk;

  data["Последний ход"][chk.idx] = chk.turn;
  return { ok: true, idx: chk.idx, turn: chk.turn };
}


/* =======================
   ADVANCE: если все активные сделали ход -> ХодМира++
   ======================= */

function TURN_tryAdvanceWorldTurn_(data) {
  TURN_ensureWorldAndCountryTurns_(data);

  var worldArr = getWorldDataArray_(data);
  var turn = Math.floor(TURN_numOrNaN_(getFieldValue_(worldArr, "Ход")));
  if (!isFinite(turn) || turn <= 0) turn = 1;

  var ids  = Array.isArray(data["ID Страны"]) ? data["ID Страны"] : [];
  var names = Array.isArray(data["Список стран"]) ? data["Список стран"] : [];
  var lastArr = Array.isArray(data["Последний ход"]) ? data["Последний ход"] : [];
  var actArr  = Array.isArray(data["Активность стран"])
    ? data["Активность стран"]
    : ids.map(function () { return true; });

  if (!ids.length || ids.length !== names.length || ids.length !== lastArr.length || ids.length !== actArr.length) {
    return { advanced: false, reason: "Списки стран/последних ходов/активности невалидны" };
  }

  for (var i = 0; i < ids.length; i++) {
    if (actArr[i] === false) continue;

    var lt = Math.floor(TURN_numOrNaN_(lastArr[i]));
    if (!isFinite(lt)) lt = turn - 1;

    if (lt < turn) return { advanced: false, reason: "Не все активные страны завершили ход " + turn };
  }

  setFieldValue_(worldArr, "Ход", turn + 1);

  if (!Array.isArray(data.Новости)) data.Новости = [];
  data.Новости.push("🌍 Общий ход повышен: " + turn + " → " + (turn + 1));

  return { advanced: true, from: turn, to: turn + 1 };
}


/* =======================
   SUMMARY
   ======================= */

function TURN_buildTurnStatusSummary_(data) {
  TURN_ensureWorldAndCountryTurns_(data);

  var worldArr = getWorldDataArray_(data);
  var turn = Math.floor(TURN_numOrNaN_(getFieldValue_(worldArr, "Ход")));
  if (!isFinite(turn) || turn <= 0) turn = 1;

  var ids   = Array.isArray(data["ID Страны"]) ? data["ID Страны"] : [];
  var names = Array.isArray(data["Список стран"]) ? data["Список стран"] : [];
  var lastArr = Array.isArray(data["Последний ход"]) ? data["Последний ход"] : [];
  var actArr  = Array.isArray(data["Активность стран"])
    ? data["Активность стран"]
    : ids.map(function () { return true; });

  var done = [], todo = [], off = [];

  for (var i = 0; i < ids.length; i++) {
    var name = (names[i] != null && String(names[i]).trim() !== "") ? String(names[i]) : ("ID " + ids[i]);

    if (actArr[i] === false) { off.push(name); continue; }

    var lt = Math.floor(TURN_numOrNaN_(lastArr[i]));
    if (!isFinite(lt)) lt = turn - 1;

    if (lt >= turn) done.push(name);
    else todo.push(name);
  }

  return {
    turn: turn,
    done: done,
    todo: todo,
    inactive: off,
    text:
      "Ход " + turn + " | " +
      "Сделали: " + (done.length ? done.join(", ") : "—") +
      " | Остались: " + (todo.length ? todo.join(", ") : "—") +
      (off.length ? " | Неактивны: " + off.join(", ") : "")
  };
}

function TURN_pushTurnSummaryNews_(data) {
  var s = TURN_buildTurnStatusSummary_(data);
  if (!Array.isArray(data.Новости)) data.Новости = [];
  data.Новости.push("🕒 " + s.text);
}


/* =======================
   RUNGAME STEPS
   ======================= */

/**
 * Шаг 0: гейт (поставь ПЕРВЫМ в списке runGame).
 * Если страна уже ходила — прерываем пайплайн: ctx.__abort = true
 */
function TURN_gateCountryTurn(data, ctx) {
  ctx = ctx || {};
  TURN_ensureWorldAndCountryTurns_(data);

  // текущая страна берётся из "Данные государства"
  var stateArr = getStateDataArray_(data);
  var countryId = getFieldValue_(stateArr, "Идентификатор государства");

  var chk = TURN_canCountryAct_(data, countryId);
  if (!chk.ok) {
    if (!Array.isArray(data.Новости)) data.Новости = [];
    data.Новости.push("⛔ Ход отклонён: " + chk.reason);
    ctx.__abort = true;
    return;
  }

  ctx.__turn_countryId = countryId;
  ctx.__turn_turn = chk.turn;
  ctx.__turn_idx = chk.idx;
}

/**
 * Шаг LAST: commit + advance + summary (поставь ПОСЛЕДНИМ).
 */
function TURN_commitAndAdvance(data, ctx) {
  ctx = ctx || {};
  if (ctx.__abort) return;

  var countryId = ctx.__turn_countryId;
  if (!countryId) {
    var stateArr = getStateDataArray_(data);
    countryId = getFieldValue_(stateArr, "Идентификатор государства");
  }

  TURN_markCountryDone_(data, countryId);
  TURN_tryAdvanceWorldTurn_(data);
  TURN_pushTurnSummaryNews_(data);
}