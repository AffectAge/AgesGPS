/**
 * JSON_OBJECT_MATRIX(json; rootPath; rowName; sortRows; sortCols)
 *
 * Строит матрицу из объекта объектов:
 * rowName | col1 | col2 | ...
 *
 * Пример:
 * =JSON_OBJECT_MATRIX(A1;"Товары";"Товар";VERO;VERO)
 */
function JSON_OBJECT_MATRIX(jsonText, rootPath, rowName, sortRows, sortCols) {
  var text = jsonText;
  if (Array.isArray(text)) text = (text[0] && text[0][0] != null) ? text[0][0] : "";
  text = (text == null) ? "" : String(text).trim();
  if (!text) return [[rowName || "Row"]];

  var obj;
  try { obj = JSON.parse(text); }
  catch (e) { return [["Ошибка JSON", String(e)]]; }

  var root = obj;
  if (rootPath != null && String(rootPath).trim() !== "") {
    root = JSON_pathGet_(obj, String(rootPath));
  }

  if (!root || typeof root !== "object" || Array.isArray(root)) {
    return [["Ошибка", "rootPath не указывает на объект объектов"]];
  }

  var rows = Object.keys(root);
  var doSortRows = (sortRows === true || String(sortRows).toLowerCase() === "true" || String(sortRows).toLowerCase() === "vero");
  var doSortCols = (sortCols === true || String(sortCols).toLowerCase() === "true" || String(sortCols).toLowerCase() === "vero");
  if (doSortRows) rows.sort();

  // union колонок
  var colSet = Object.create(null);
  for (var i = 0; i < rows.length; i++) {
    var rec = root[rows[i]];
    if (rec && typeof rec === "object" && !Array.isArray(rec)) {
      Object.keys(rec).forEach(function(k){ colSet[k] = true; });
    }
  }
  var cols = Object.keys(colSet);
  if (doSortCols) cols.sort();

  var header = [rowName || "Row"].concat(cols);
  var out = [header];

  for (var r = 0; r < rows.length; r++) {
    var name = rows[r];
    var rec2 = root[name];
    var line = [name];
    for (var c = 0; c < cols.length; c++) {
      var key = cols[c];
      var v = (rec2 && typeof rec2 === "object" && !Array.isArray(rec2)) ? rec2[key] : null;
      line.push(v == null ? 0 : JSON_normValue_(v));
    }
    out.push(line);
  }

  return out;
}