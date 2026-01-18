/* =========================================================
   TURNS: Gate + Commit (для runGame, без глобального data)
   ========================================================= */

// --- ENSURE: общий ход + массивы стран ---
function TURN_ensureWorldAndCountryTurns_(data) {
  if (!data || typeof data !== "object") return;

  var stateArr = getStateDataArray_(data);

  var turn = Math.floor(Number(getFieldValue_(stateArr, "Ход")) || 0);
  if (turn <= 0) turn = 1;
  setFieldValue_(stateArr, "Ход", turn);

  var ids = Array.isArray(data["ID Страны"]) ? data["ID Страны"] : [];
  var names = Array.isArray(data["Список стран"]) ? data["Список стран"] : [];

  if (!ids.length || ids.length !== names.length) return;

  if (!Array.isArray(data["Последний ход"]) || data["Последний ход"].length !== ids.length) {
    data["Последний ход"] = ids.map(function () { return turn - 1; });
  }

  for (var i = 0; i < data["Последний ход"].length; i++) {
    var lt = Math.floor(Number(data["Последний ход"][i]) || 0);
    if (lt < 0) lt = 0;
    data["Последний ход"][i] = lt;
  }

  if (!Array.isArray(data["Активность стран"]) || data["Активность стран"].length !== ids.length) {
    data["Активность стран"] = ids.map(function () { return true; });
  } else {
    for (var j = 0; j < data["Активность стран"].length; j++) {
      data["Активность стран"][j] = (data["Активность стран"][j] === false) ? false : true;
    }
  }
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
  var turn = Math.floor(Number(getFieldValue_(stateArr, "Ход")) || 1);

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
  var turn = Math.floor(Number(getFieldValue_(stateArr, "Ход")) || 1);

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
  var turn = Math.floor(Number(getFieldValue_(stateArr, "Ход")) || 1);

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