/* =========================================================
   TURNS: Gate + Commit (для runGame, без глобального data)
   ========================================================= */

// --- ENSURE: общий ход + массивы стран ---
/* =======================
   ENSURE (V3): общий ход + списки стран
   - по дефолту LastTurn = (Ход - 1)  ✅ страна должна сходить
   - по дефолту Active = true
   - игнорируем строки без ID или без Названия
   ======================= */

/* =======================
   WORLD TURN SOURCE (robust)
   1) data["Данные государства"]."Ход" (если есть)
   2) иначе: max(Последний ход активных) + 1
   3) иначе: 1
   ======================= */

function TURN_getWorldTurn_(data) {
  if (!data || typeof data !== "object") return 1;

  // 1) пробуем из "Данные государства"
  var stateArr = getStateDataArray_(data);
  var t = Math.floor(Number(getFieldValue_(stateArr, "Ход")) || 0);
  if (t > 0) return t;

  // 2) иначе выводим из массива последних ходов
  var idsRaw   = Array.isArray(data["ID Страны"]) ? data["ID Страны"] : [];
  var namesRaw = Array.isArray(data["Список стран"]) ? data["Список стран"] : [];
  var lastRaw  = Array.isArray(data["Последний ход"]) ? data["Последний ход"] : [];
  var actRaw   = Array.isArray(data["Активность стран"]) ? data["Активность стран"] : [];

  if (!idsRaw.length || idsRaw.length !== namesRaw.length) return 1;

  var maxLast = -1;

  for (var i = 0; i < idsRaw.length; i++) {
    // игнорим строки без ID/имени (как ты требовал)
    var idStr = (idsRaw[i] == null) ? "" : String(idsRaw[i]).trim();
    var nmStr = (namesRaw[i] == null) ? "" : String(namesRaw[i]).trim();
    if (idStr === "" || nmStr === "") continue;

    // активность: по дефолту true
    var a = (i < actRaw.length) ? actRaw[i] : null;
    a = (a === false) ? false : true;
    if (a === false) continue;

    var lt = (i < lastRaw.length) ? Math.floor(TURN_numOrNaN_(lastRaw[i])) : NaN;
    if (!isFinite(lt)) continue;

    if (lt > maxLast) maxLast = lt;
  }

  // если нашли хоть что-то
  if (maxLast >= 0) return maxLast + 1;

  return 1;
}


/* =======================
   ENSURE (V4): использует TURN_getWorldTurn_
   - по дефолту LastTurn = (Ход - 1) ✅ страна должна сходить
   - по дефолту Active = true
   - игнорирует строки без ID/Название
   ======================= */

function TURN_ensureWorldAndCountryTurns_(data) {
  if (!data || typeof data !== "object") return;

  // ✅ берём общий ход корректно
  var turn = TURN_getWorldTurn_(data);

  // фиксируем/публикуем его в "Данные государства" (чтобы дальше всё работало одинаково)
  var stateArr = getStateDataArray_(data);
  setFieldValue_(stateArr, "Ход", turn);

  // нормализация стран (твоя V3-логика)
  TURN_normalizeCountryLists_(data, turn);
}

/* =======================
   PRIVATE: normalize/align country arrays
   - filters out rows with missing ID or Name
   - aligns: ID Страны / Список стран / Последний ход / Активность стран
   - default LastTurn = worldTurn - 1  ✅ must act
   - default Active = true
   ======================= */

function TURN_normalizeCountryLists_(data, worldTurn) {
  var idsRaw   = Array.isArray(data["ID Страны"]) ? data["ID Страны"] : [];
  var namesRaw = Array.isArray(data["Список стран"]) ? data["Список стран"] : [];

  // если списки пустые/разной длины — ничего не делаем
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

    // --- ID: должен существовать и не быть пустым ---
    var idStr = (idVal == null) ? "" : String(idVal).trim();
    if (idStr === "") continue;

    // --- Name: должен существовать и не быть пустым ---
    var nameStr = (nameVal == null) ? "" : String(nameVal).trim();
    if (nameStr === "") continue;

    // --- LastTurn: по дефолту = worldTurn - 1 (страна ДОЛЖНА сходить) ---
    var ltRaw = (i < lastRaw.length) ? lastRaw[i] : null;
    var lt = Math.floor(TURN_numOrNaN_(ltRaw));
if (!isFinite(lt)) lt = worldTurn - 1;   // ✅ теперь сработает и для ""
    if (lt < 0) lt = 0;

    // --- Activity: по дефолту true ---
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

function TURN_findCountryIndexById_(data, countryId) {
  var ids = Array.isArray(data["ID Страны"]) ? data["ID Страны"] : [];
  var sid = String(countryId).trim();
  for (var i = 0; i < ids.length; i++) {
    if (String(ids[i]).trim() === sid) return i;
  }
  return -1;
}

function TURN_canCountryAct_(data, countryId) {
  TURN_ensureWorldAndCountryTurns_(data);

  var stateArr = getStateDataArray_(data);
  var turn = Math.floor(TURN_numOrNaN_(getFieldValue_(stateArr, "Ход")));
if (!isFinite(turn) || turn <= 0) turn = 1;
  var idx = TURN_findCountryIndexById_(data, countryId);
  if (idx < 0) return { ok: false, reason: "страна не найдена", turn: turn };

  var actArr = data["Активность стран"];
  if (actArr && actArr[idx] === false) {
    return { ok: false, reason: "страна неактивна", idx: idx, turn: turn };
  }

  var lastArr = data["Последний ход"];
  var last = Math.floor(Number(lastArr[idx]) || 0);

  if (last >= turn) {
    return { ok: false, reason: "страна уже сделала ход " + turn, idx: idx, turn: turn, last: last };
  }

  return { ok: true, idx: idx, turn: turn, last: last };
}

function TURN_markCountryDone_(data, countryId) {
  var chk = TURN_canCountryAct_(data, countryId);
  if (!chk.ok) return chk;

  data["Последний ход"][chk.idx] = chk.turn;
  return { ok: true, idx: chk.idx, turn: chk.turn };
}

function TURN_tryAdvanceWorldTurn_(data) {
  TURN_ensureWorldAndCountryTurns_(data);

  var stateArr = getStateDataArray_(data);
  var turn = Math.floor(TURN_numOrNaN_(getFieldValue_(stateArr, "Ход")));
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
    var lt = Math.floor(Number(lastArr[i]) || 0);
    if (lt < turn) return { advanced: false, reason: "Не все активные страны завершили ход " + turn };
  }

  setFieldValue_(stateArr, "Ход", turn + 1);

  if (!Array.isArray(data.Новости)) data.Новости = [];
  data.Новости.push("🌍 Общий ход повышен: " + turn + " → " + (turn + 1));

  return { advanced: true, from: turn, to: turn + 1 };
}

function TURN_buildTurnStatusSummary_(data) {
  TURN_ensureWorldAndCountryTurns_(data);

  var stateArr = getStateDataArray_(data);
  var turn = Math.floor(TURN_numOrNaN_(getFieldValue_(stateArr, "Ход")));
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

    var lt = Math.floor(Number(lastArr[i]) || 0);
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
    ctx.__abort = true; // ✅ остановить остальные модули
    return;
  }

  // сохраним в ctx, чтобы не пересчитывать
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
    // на случай если не было gate (или ID пустой)
    var stateArr = getStateDataArray_(data);
    countryId = getFieldValue_(stateArr, "Идентификатор государства");
  }

  // commit
  TURN_markCountryDone_(data, countryId);

  // advance if all active done
  TURN_tryAdvanceWorldTurn_(data);

  // summary
  TURN_pushTurnSummaryNews_(data);
}

function TURN_numOrNaN_(v) {
  if (v === "" || v == null) return NaN;     // ✅ пустое = нет значения
  var n = Number(v);
  return isFinite(n) ? n : NaN;
}