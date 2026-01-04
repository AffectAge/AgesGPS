/* =========================================================
   СИНХРОНИЗАЦИЯ ГОСУДАРСТВА ЧЕРЕЗ JSON (ТОЛЬКО НОВЫЙ ВАРИАНТ)
   Google Apps Script (V8)

   Ожидается:
   data["Данные государства"] = [
     {"Идентификатор государства": 1},
     {"Коэффициент рабочей силы": 0.4},
     {"Название государства": "..." } // будет обновляться
   ];

   Также нужны:
   data["ID Страны"]     = [1,2,3,...]
   data["Список стран"]  = ["Франция","Германия",...]
   ========================================================= */

function synchronizeCountryData(data) {
  console.log('🔁 Синхронизация государства (JSON) со списком стран');

  // --- Входные справочники ---
  var countryIds = data['ID Страны'] || [];
  var countryNames = data['Список стран'] || [];

  if (!Array.isArray(countryIds) || !Array.isArray(countryNames) || countryIds.length === 0 || countryIds.length !== countryNames.length) {
    console.log('❌ Список стран пустой или несоответствующий (ID Страны / Список стран)');
    return data;
  }

  // --- Данные государства (ТОЛЬКО JSON) ---
  var stateArr = getStateDataArray_(data);

  var currentId = getFieldValue_(stateArr, 'Идентификатор государства'); // string|null
  var currentName = getFieldValue_(stateArr, 'Название государства');    // string|null

  var changed = false;
  var logMessage = '';

  // 1) Если есть ID -> обновляем/записываем Название государства в JSON
  if (currentId) {
    var searchId = String(currentId).trim();

    var idx = countryIds.findIndex(function (id) {
      var listId = (typeof id === 'string') ? id.trim() : id;
      return String(listId) === String(searchId);
    });

    if (idx !== -1) {
      var listName = countryNames[idx];

      if (String(currentName || '') !== String(listName)) {
        setFieldValue_(stateArr, 'Название государства', listName);
        changed = true;
        logMessage = 'Обновлено название государства в JSON: "' + (currentName || 'пусто') + '" → "' + listName + '" (ID ' + currentId + ')';
        console.log('✅ ' + logMessage);
      } else {
        console.log('✅ Название в JSON уже актуально: ' + currentName);
      }
    } else {
      console.log('⚠️ ID ' + currentId + ' не найден в списке стран');
    }
  }

  // 2) Если ID нет, но есть Название -> обновляем/записываем ID в JSON
  if (!changed && !currentId && currentName) {
    var trimmedName = String(currentName).trim();

    var idx2 = countryNames.findIndex(function (name) {
      return (typeof name === 'string') && name.trim() === trimmedName;
    });

    if (idx2 !== -1) {
      var listId2 = countryIds[idx2];

      setFieldValue_(stateArr, 'Идентификатор государства', listId2);
      changed = true;
      logMessage = 'Обновлён идентификатор государства в JSON: ' + listId2 + ' по названию "' + currentName + '"';
      console.log('✅ ' + logMessage);
    } else {
      console.log('⚠️ Название "' + currentName + '" не найдено в списке стран');
    }
  }

  // Новости/лог
  if (changed) {
    if (!Array.isArray(data.Новости)) data.Новости = [];
    data.Новости.push(logMessage);
  } else {
    console.log('ℹ️ JSON синхронизация не потребовалась — данные уже актуальны');
  }

  return data;
}

/* =======================
   JSON helpers (private)
   ======================= */

function safeParseJSONCell_(v) {
  if (v === "" || v == null) return v;
  if (typeof v === "object") return v;
  if (typeof v !== "string") return v;

  var t = v.trim();
  if (!(t.startsWith("{") || t.startsWith("["))) return v;

  try {
    return JSON.parse(t);
  } catch (e) {
    return v;
  }
}

function flattenCells_(value) {
  var out = [];
  if (value == null) return out;

  if (!Array.isArray(value)) return [value];

  value.forEach(function (row) {
    if (Array.isArray(row)) out = out.concat(row);
    else out.push(row);
  });

  return out;
}

function getStateDataArray_(data) {
  var raw = data["Данные государства"];
  raw = safeParseJSONCell_(raw);

  // массив (1D/2D)
  if (Array.isArray(raw)) {
    var flat = flattenCells_(raw).filter(function (x) {
      return x && typeof x === "object" && !Array.isArray(x);
    });
    data["Данные государства"] = flat;
    return data["Данные государства"];
  }

  // одиночный объект
  if (raw && typeof raw === "object") {
    data["Данные государства"] = [raw];
    return data["Данные государства"];
  }

  // пусто/невалидно
  data["Данные государства"] = [];
  return data["Данные государства"];
}

function findFieldObj_(arr, key) {
  for (var i = 0; i < arr.length; i++) {
    var o = arr[i];
    if (o && typeof o === "object" && Object.prototype.hasOwnProperty.call(o, key)) return o;
  }
  return null;
}

function getFieldValue_(arr, key) {
  var o = findFieldObj_(arr, key);
  if (!o) return null;

  var v = o[key];
  if (v === undefined || v === null) return null;

  var s = String(v).trim();
  return s === "" ? null : s;
}

function setFieldValue_(arr, key, value) {
  var o = findFieldObj_(arr, key);
  if (!o) {
    o = {};
    o[key] = value;
    arr.push(o);
  } else {
    o[key] = value;
  }
}