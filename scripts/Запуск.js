/**
 * Основная функция для чтения, обработки и записи данных
 */
function scanNamedRanges() {
  const spreadsheet = SpreadsheetApp.getActiveSpreadsheet();
  const sheet = spreadsheet.getActiveSheet(); // Получаем активный лист

  // Список именованных диапазонов, которые нужно прочитать (исключаем Журнал_Событий)
  const rangeNamesToRead = [
    'Переменные',
    'Провинции',
    'Постройки',
    'Настройки'
  ];

  // Получаем все именованные диапазоны за один вызов
  const allNamedRanges = spreadsheet.getNamedRanges();
  
  // Создаём карту для быстрого доступа: {имяДиапазона: объектRange}
  const namedRangeMap = {};
  allNamedRanges.forEach(namedRange => {
    const name = namedRange.getName();
    namedRangeMap[name] = namedRange.getRange();
  });

  // Объект для хранения данных из диапазонов
  let data = {};

  try {
    // Читаем данные из каждого именованного диапазона из списка
    rangeNamesToRead.forEach(rangeName => {
      const range = namedRangeMap[rangeName];
      if (range) {
        data[rangeName] = range.getValues();
      } else {
        const errorMsg = `Диапазон с именем "${rangeName}" не найден.`;
        // Логируем ошибку в Журнал_Событий и завершаем
        addMessagesToRange4([`[Ошибка] ${errorMsg}`], spreadsheet);
        throw new Error(errorMsg);
      }
    });

    // Передача данных во вторую функцию для обработки
    processTurn(data, sheet, spreadsheet);

  } catch (error) {
    // Добавление сообщения об ошибке в Журнал_Событий
    const errorMessage = `[Ошибка] scanNamedRanges: ${error.message}`;
    addMessagesToRange4([errorMessage], spreadsheet);
    // Дополнительная обработка ошибки при необходимости
  }
}

/**
 * Основная функция обработки хода
 */
function processTurn(data, sheet, spreadsheet) {
  let allNewMessages = [];

  try {
    // Массив с описанием функций для вызова
    const functionsToRun = [
      { name: 'Обработка приветствия GNN', func: () => processConstructionQueue(data, buildings) },
    ];

    // Выполнение всех функций по порядку
    for (let i = 0; i < functionsToRun.length; i++) {
      const { name, func } = functionsToRun[i];
      try {
        const start = Date.now();
        const result = func();
        const duration = ((Date.now() - start) / 1000).toFixed(2);
        allNewMessages.push(`[Уведомление] 🛠️ ${name} выполнена за ⏳${duration} сек.`);

        if (Array.isArray(result)) {
          allNewMessages.push(...result);
        }
      } catch (error) {
        allNewMessages.push(`[Ошибка] В функции ${name}: ${error.message}`);
      }
    }

    // Разделение сообщений на два журнала
    const standardMessages = [];
    const gnnMessages = [];

    allNewMessages.forEach(msg => {
      if (typeof msg === 'string') {
        if (msg.startsWith('[GNN]')) {
          const cleanMsg = msg.slice(5).trim(); // Удаляем [GNN]
          gnnMessages.push(cleanMsg);
        } else {
          standardMessages.push(msg);
        }
      }
    });

    // Запись в журналы
    if (standardMessages.length > 0) {
      addMessagesToRange4(standardMessages, spreadsheet);
    }
    if (gnnMessages.length > 0) {
      addMessagesToRange5(gnnMessages, spreadsheet);
    }

    // Обновление данных
    updateRanges(data, spreadsheet);

  } catch (error) {
    const errorMessage = `[Критическая Ошибка] processTurn: ${error.message}`;
    addMessagesToRange4([errorMessage], spreadsheet);
  }
}

/**
 * Функция для записи обновленных данных обратно в именованные диапазоны
 *
 * @param {Object} updatedData - Объект с обновленными данными
 * @param {Spreadsheet} spreadsheet - Объект активной таблицы
 */
function updateRanges(updatedData, spreadsheet) {
  // Определяем чёрный список диапазонов
  const blacklist = new Set(['Журнал_Событий']);

  // Получаем все именованные диапазоны за один вызов API
  const namedRanges = spreadsheet.getNamedRanges();

  // Создаём карту: {имяДиапазона: объектRange}
  const namedRangeMap = {};
  namedRanges.forEach(namedRange => {
    namedRangeMap[namedRange.getName()] = namedRange.getRange();
  });

  // Массив для сбора сообщений об ошибках
  const errorMessages = [];

  // Итерация по обновленным данным
  for (const [rangeName, values] of Object.entries(updatedData)) {
    // Пропускаем диапазоны из чёрного списка
    if (blacklist.has(rangeName)) {
      continue;
    }

    const range = namedRangeMap[rangeName];
    if (range) {
      try {
        // Устанавливаем значения в диапазон
        range.setValues(values);
      } catch (error) {
        // Сохраняем сообщение об ошибке для последующей обработки
        errorMessages.push(`Ошибка при записи в диапазон "${rangeName}": ${error.message}`);
      }
    } else {
      // Диапазон не найден, добавляем сообщение об ошибке
      errorMessages.push(`Диапазон с именем "${rangeName}" не найден при записи данных.`);
    }
  }

  // После завершения всех операций, если есть ошибки, логируем их
  if (errorMessages.length > 0) {
    const combinedErrorMsg = errorMessages.join('\n');
    logErrorToEventLog(combinedErrorMsg, spreadsheet);
  }

  // Принудительно отправляем все ожидающие изменения
  SpreadsheetApp.flush();
}
