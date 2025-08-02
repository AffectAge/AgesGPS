/**
 * Функция, которая запускается при любом редактировании таблицы.
 * Проверяет, был ли изменен чекбокс в ячейке A1 на листе "Ход" и запускает scanNamedRanges при необходимости.
 * @param {Object} e - Объект события редактирования
 */
function onEdit(e) {
  if (!e) return;

  const range = e.range;
  const sheet = range.getSheet();

  // Проверка: редактируется ли лист "Ход" и ячейка A1
  if (sheet.getName() === 'Ход' && range.getA1Notation() === 'A1') {
    const isChecked = range.getValue();

    if (isChecked === true) {
      // Запускаем основную функцию
      scanNamedRanges();

      // Применяем изменения и сбрасываем чекбокс
      SpreadsheetApp.flush();
      range.setValue(false);
    }
  }
}