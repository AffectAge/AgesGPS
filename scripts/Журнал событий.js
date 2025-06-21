/**
 * ПРОДВИНУТЫЙ СКРИПТ УПРАВЛЕНИЯ ЖУРНАЛАМИ СОБЫТИЙ
 * Версия 3.0 - Встроенное форматирование и оптимизация API
 */

/**
 * Загружает настройки из именованного диапазона "Настройки".
 */
function loadSettings(spreadsheet) {
  const rangeName = 'Настройки';
  const range = spreadsheet.getRangeByName(rangeName);
  
  if (!range) {
    throw new Error(`Диапазон с именем "${rangeName}" не найден.`);
  }
  
  const values = range.getValues();
  const settings = {};
  
  values.forEach(row => {
    const identifier = row[0];
    const data = row[1];
    
    if (identifier && data !== undefined && data !== null) {
      try {
        settings[identifier] = JSON.parse(data);
      } catch (e) {
        if (typeof data === 'boolean') {
          settings[identifier] = data;
        } else if (!isNaN(data) && data !== '') {
          settings[identifier] = Number(data);
        } else {
          settings[identifier] = data;
        }
      }
    }
  });
  
  return settings;
}

/**
 * КОНФИГУРАЦИЯ СИСТЕМЫ
 */
const spreadsheet = SpreadsheetApp.getActiveSpreadsheet();
const settings = loadSettings(spreadsheet);

// Базовые настройки
const MAX_TOTAL_MESSAGES = settings['Максимальное количество сообщений'] || 1000;
const MAX_CHARACTERS_PER_CELL = settings['Максимальное количество символов на ячейку'] || 50000;
const ENABLE_TIMESTAMPS = settings['Включить временные метки'] || false;
const TIMESTAMP_FORMAT = settings['Формат временных меток'] || 'dd.MM.yyyy HH:mm:ss';
const ENABLE_DEDUPLICATION = settings['Включить дедупликацию'] || true;
const MESSAGE_RETENTION_RULES = settings['Правила хранения сообщений'] || {};
const ENABLE_BACKGROUND_COLORS = settings['Включить цвета фона'] || true;

// Правила хранения по умолчанию (в количестве сообщений)
const DEFAULT_RETENTION = {
  "Критическая ошибка": { maxCount: 100 },   // Максимум 100 сообщений
  "Ошибка": { maxCount: 50 },                // Максимум 50 сообщений
  "Предупреждение": { maxCount: 30 },        // Максимум 30 сообщений
  "Информация": { maxCount: 20 },            // Максимум 20 сообщений
  "Отладка": { maxCount: 15 },               // Максимум 15 сообщений
  "Постройки": { maxCount: 40 },             // Максимум 40 сообщений
  "default": { maxCount: 25 }                // По умолчанию 25 сообщений
};

// Объединяем с пользовательскими настройками
const RETENTION_RULES = { ...DEFAULT_RETENTION, ...MESSAGE_RETENTION_RULES };

// Настройки приоритетов и цветов (как в предыдущей версии)
const CATEGORY_PRIORITY = settings['Приоритет категорий'] || {
  "Критическая ошибка": 0, "Ошибка": 1, "Предупреждение": 2,
  "Информация": 3, "Отладка": 4, "Постройки": 5
};

const DISABLED_CATEGORIES = settings['Отключённые категории'] || [];

let CATEGORY_COLORS = settings['Цвета категорий'] || {
  "Критическая ошибка": "#8B0000", "Ошибка": "#FF0000", "Предупреждение": "#FFA500",
  "Информация": "#0000FF", "Отладка": "#808080", "Постройки": "#00008B", "Успех": "#008000"
};

const DEFAULT_CATEGORY_COLOR = CATEGORY_COLORS['default'] || "#000000";
delete CATEGORY_COLORS['default'];

// Цвета фона для категорий
const CATEGORY_BACKGROUNDS = settings['Цвета фона категорий'] || {
  "Критическая ошибка": "#FFE4E1",
  "Ошибка": "#FFEEEE",
  "Предупреждение": "#FFF8DC",
  "Информация": "#E6F3FF",
  "Отладка": "#F5F5F5",
  "Строительство": "#F0FFF0"
};

/**
 * ПАРСЕР ВСТРОЕННОГО ФОРМАТИРОВАНИЯ В СООБЩЕНИЯХ
 */
class MessageFormatter {
  /**
   * Парсит встроенные теги форматирования в сообщении
   * Поддерживаемые теги:
   * <color:#FF0000>текст</color> - цвет текста
   * <bg:#FFFF00>текст</bg> - цвет фона
   * <b>текст</b> - жирный
   * <i>текст</i> - курсив
   * <u>текст</u> - подчеркивание
   * <size:12>текст</size> - размер шрифта
   * <font:Arial>текст</font> - шрифт
   * <cellbg:#FFFF00> - цвет фона всей ячейки (в начале сообщения)
   */
  static parse(message) {
    const result = {
      cleanText: message,
      formats: [],
      cellBackground: null
    };
    
    // Извлекаем цвет фона ячейки
    const cellBgMatch = message.match(/^<cellbg:(#[A-Fa-f0-9]{6})>/);
    if (cellBgMatch) {
      result.cellBackground = cellBgMatch[1];
      result.cleanText = message.replace(cellBgMatch[0], '');
    }
    
    // Находим все теги форматирования
    const formatTags = [
      { pattern: /<color:(#[A-Fa-f0-9]{6})>(.*?)<\/color>/g, type: 'color' },
      { pattern: /<bg:(#[A-Fa-f0-9]{6})>(.*?)<\/bg>/g, type: 'background' },
      { pattern: /<b>(.*?)<\/b>/g, type: 'bold' },
      { pattern: /<i>(.*?)<\/i>/g, type: 'italic' },
      { pattern: /<u>(.*?)<\/u>/g, type: 'underline' },
      { pattern: /<size:(\d+)>(.*?)<\/size>/g, type: 'fontSize' },
      { pattern: /<font:([^>]+)>(.*?)<\/font>/g, type: 'fontFamily' }
    ];
    
    let cleanText = result.cleanText;
    
    // Сначала собираем все совпадения без изменения текста
    const allMatches = [];
    formatTags.forEach(tag => {
      let match;
      const regex = new RegExp(tag.pattern.source, tag.pattern.flags);
      while ((match = regex.exec(result.cleanText)) !== null) {
        allMatches.push({
          match: match,
          type: tag.type,
          fullMatch: match[0],
          value: match[1],
          content: match[2] || match[1],
          start: match.index
        });
      }
    });
    
    // Сортируем по позиции (от конца к началу для корректного удаления)
    allMatches.sort((a, b) => b.start - a.start);
    
    // Обрабатываем каждое совпадение
    let offset = 0;
    allMatches.reverse().forEach(item => {
      const originalStart = item.start;
      const cleanStart = originalStart - offset;
      const cleanEnd = cleanStart + item.content.length;
      
      // Добавляем формат
      const format = {
        start: cleanStart,
        end: cleanEnd,
        type: item.type,
        value: item.value,
        content: item.content
      };
      
      result.formats.push(format);
      
      // Заменяем тег на контент в cleanText
      cleanText = cleanText.substring(0, originalStart - offset) + 
                 item.content + 
                 cleanText.substring(originalStart - offset + item.fullMatch.length);
      
      // Обновляем смещение
      offset += item.fullMatch.length - item.content.length;
    });
    
    result.cleanText = cleanText;
    return result;
  }
  
  /**
   * Применяет форматирование к RichTextValueBuilder
   */
  static applyFormatting(builder, formats, cleanText, baseColor = "#000000") {
    // Применяем базовый цвет ко всему тексту
    const baseStyle = SpreadsheetApp.newTextStyle()
      .setForegroundColor(baseColor)
      .build();
    
    builder.setTextStyle(0, cleanText.length, baseStyle);
    
    // Применяем каждый формат
    formats.forEach(format => {
      let style = SpreadsheetApp.newTextStyle();
      
      switch (format.type) {
        case 'color':
          style = style.setForegroundColor(format.value);
          break;
        case 'background':
          style = style.setBackgroundColor(format.value);
          break;
        case 'bold':
          style = style.setBold(true);
          break;
        case 'italic':
          style = style.setItalic(true);
          break;
        case 'underline':
          style = style.setUnderline(true);
          break;
        case 'fontSize':
          style = style.setFontSize(parseInt(format.value));
          break;
        case 'fontFamily':
          style = style.setFontFamily(format.value);
          break;
      }
      
      try {
        // Проверяем границы перед применением
        const start = Math.max(0, Math.min(format.start, cleanText.length));
        const end = Math.max(start, Math.min(format.end, cleanText.length));
        
        if (start < end) {
          builder.setTextStyle(start, end, style.build());
        }
      } catch (error) {
        console.warn(`Ошибка применения форматирования: ${error.message}`, {
          start: format.start,
          end: format.end,
          textLength: cleanText.length
        });
      }
    });
    
    return builder;
  }
}

/**
 * СИСТЕМА УПРАВЛЕНИЯ ДУБЛИКАТАМИ И ЛИМИТОВ
 */
class MessageManager {
  /**
   * Создает уникальный хеш для сообщения
   */
  static createHash(message) {
    const cleanMsg = MessageFormatter.parse(message).cleanText;
    // Убираем временные метки для создания хеша
    const msgWithoutMeta = cleanMsg.replace(/^\[[\d\.\s\:]+\]\s*/, '');
    return Utilities.computeDigest(Utilities.DigestAlgorithm.MD5, msgWithoutMeta)
      .map(b => (b < 0 ? b + 256 : b).toString(16).padStart(2, '0'))
      .join('');
  }
  
  /**
   * Удаляет дубликаты и применяет лимиты по количеству сообщений в категории
   */
  static deduplicate(messages) {
    if (!ENABLE_DEDUPLICATION) return messages;
    
    const uniqueMessages = new Map();
    
    messages.forEach(msg => {
      const hash = this.createHash(msg);
      
      // Если сообщение уже есть, сохраняем более новое (по временной метке)
      if (uniqueMessages.has(hash)) {
        const existing = uniqueMessages.get(hash);
        const existingTime = this.extractTimestamp(existing);
        const currentTime = this.extractTimestamp(msg);
        
        if (currentTime > existingTime) {
          uniqueMessages.set(hash, msg);
        }
      } else {
        uniqueMessages.set(hash, msg);
      }
    });
    
    const result = Array.from(uniqueMessages.values());
    
    if (messages.length !== result.length) {
      console.log(`🧹 Удалено ${messages.length - result.length} дублированных сообщений`);
    }
    
    return result;
  }
  
  /**
   * Применяет лимиты по количеству сообщений для каждой категории
   */
  static applyRetentionLimits(categorizedMessages) {
    const result = {};
    
    for (const [category, messages] of Object.entries(categorizedMessages)) {
      const categoryKey = category.replace(/[\[\]]/g, '');
      const retention = RETENTION_RULES[categoryKey] || RETENTION_RULES.default;
      
      let limitedMessages = messages;
      
      // Если есть лимит по количеству сообщений
      if (retention.maxCount !== undefined && messages.length > retention.maxCount) {
        // Сортируем по времени (новые первыми) и берем только нужное количество
        limitedMessages = messages
          .sort((a, b) => this.extractTimestamp(b) - this.extractTimestamp(a))
          .slice(0, retention.maxCount);
        
        console.log(`📝 Категория ${category}: оставлено ${retention.maxCount} из ${messages.length} сообщений`);
      }
      
      result[category] = limitedMessages;
    }
    
    return result;
  }
  
  static extractCategory(message) {
    const match = message.match(/^\[([^\]]+)\]/);
    return match ? `[${match[1]}]` : '[Без категории]';
  }
  
  static extractTimestamp(message) {
    if (!ENABLE_TIMESTAMPS) return new Date(0);
    const match = message.match(/^\[([^\]]+)\]/);
    try {
      return match ? new Date(match[1]) : new Date(0);
    } catch {
      return new Date(0);
    }
  }
}

/**
 * СИСТЕМА ВРЕМЕННЫХ МЕТОК
 */
class TimestampManager {
  static format(date = new Date()) {
    return Utilities.formatDate(date, Session.getScriptTimeZone(), TIMESTAMP_FORMAT);
  }
  
  static addToMessage(message) {
    if (!ENABLE_TIMESTAMPS) return message;
    
    // Проверяем, есть ли уже временная метка
    if (message.match(/^\[\d{2}\.\d{2}\.\d{4}/)) {
      return message;
    }
    
    const timestamp = this.format();
    return `[${timestamp}] ${message}`;
  }
}

/**
 * ОПТИМИЗИРОВАННАЯ ФУНКЦИЯ КАТЕГОРИЗАЦИИ
 */
function categorizeMessages(messages) {
  const categorized = {};
  
  // Дедупликация перед обработкой
  const uniqueMessages = MessageManager.deduplicate(messages);
  
  uniqueMessages.forEach(msg => {
    let cleanMsg = msg.toString().trim();
    if (!cleanMsg) return;
    
    // Добавляем временную метку
    cleanMsg = TimestampManager.addToMessage(cleanMsg);
    
    // Извлечение категории
    const match = cleanMsg.match(/^(\[[^\]]+\])\s*(.*)$/);
    let category = "Без категории";
    let text = cleanMsg;
    
    if (match) {
      category = match[1];
      text = match[2] || cleanMsg;
    } else {
      // Автоматическое определение категории
      category = detectCategory(text);
    }
    
    if (!categorized[category]) {
      categorized[category] = [];
    }
    
    categorized[category].push(text);
  });
  
  return categorized;
}

/**
 * АВТОМАТИЧЕСКОЕ ОПРЕДЕЛЕНИЕ КАТЕГОРИИ
 */
function detectCategory(text) {
  const lowText = text.toLowerCase();
  
  const patterns = {
    "[Критическая ошибка]": ["критическая", "fatal", "critical", "crash", "💥", "🚨"],
    "[Ошибка]": ["ошибка", "error", "failed", "exception", "❌", "🔴"],
    "[Предупреждение]": ["предупреждение", "warning", "warn", "внимание", "⚠️", "🟡"],
    "[Успех]": ["успех", "success", "completed", "done", "готово", "✅", "🟢"],
    "[Информация]": ["информация", "info", "notice", "уведомление", "ℹ️", "🔵"],
    "[Отладка]": ["отладка", "debug", "trace", "тест", "🔧", "🛠️"],
    "[Постройки]": ["здание", "постройка", "Строительство", "🏗️", "🏢", "🗺️"]
  };
  
  for (const [category, keywords] of Object.entries(patterns)) {
    if (keywords.some(keyword => lowText.includes(keyword))) {
      return category;
    }
  }
  
  return "[Без категории]";
}

/**
 * ОБЪЕДИНЕНИЕ СООБЩЕНИЙ С УМНОЙ ДЕДУПЛИКАЦИЕЙ
 */
function mergeCategorizedMessages(existing, newMsgs) {
  const merged = { ...existing };
  
  for (const category in newMsgs) {
    if (newMsgs.hasOwnProperty(category)) {
      if (DISABLED_CATEGORIES.includes(category)) {
        continue;
      }
      
      if (!merged[category]) {
        merged[category] = [];
      }
      
      // Объединяем все сообщения и применяем дедупликацию
      const allMessages = merged[category].concat(newMsgs[category]);
      merged[category] = MessageManager.deduplicate(allMessages);
    }
  }
  
  // Применяем лимиты по количеству сообщений для каждой категории
  return MessageManager.applyRetentionLimits(merged);
}

/**
 * ГРУППИРОВКА С СОХРАНЕНИЕМ ФОРМАТИРОВАНИЯ
 */
function groupMessagesByCategory(categorizedMessages) {
  const finalMessages = [];
  
  const sortedCategories = Object.keys(categorizedMessages).sort((a, b) => {
    const priorityA = CATEGORY_PRIORITY[a.replace(/[\[\]]/g, '')] || Number.MAX_SAFE_INTEGER;
    const priorityB = CATEGORY_PRIORITY[b.replace(/[\[\]]/g, '')] || Number.MAX_SAFE_INTEGER;
    return priorityA - priorityB;
  });
  
  sortedCategories.forEach(category => {
    const messages = categorizedMessages[category];
    const formattedCategory = category.includes('[') ? category : `[${category}]`;
    let currentCellText = formattedCategory;
    
    // Сортировка по времени
    const sortedMessages = messages.sort((a, b) => {
      const timeA = MessageManager.extractTimestamp(a);
      const timeB = MessageManager.extractTimestamp(b);
      return timeB - timeA; // Новые сначала
    });
    
    sortedMessages.forEach(msg => {
      const additionalText = `\n${msg}`;
      
      // Проверяем лимит символов с учетом тегов форматирования
      const estimatedLength = (currentCellText + additionalText).length * 1.5; // Запас на теги
      
      if (estimatedLength <= MAX_CHARACTERS_PER_CELL) {
        currentCellText += additionalText;
      } else {
        finalMessages.push(currentCellText);
        currentCellText = `${formattedCategory}\n${msg}`;
      }
    });
    
    if (currentCellText.length > 0) {
      finalMessages.push(currentCellText);
    }
  });
  
  return finalMessages;
}

/**
 * ПРИМЕНЕНИЕ ЛИМИТОВ С АРХИВИРОВАНИЕМ
 */
function enforceTotalMessageLimit(finalMessages, rangeName) {
  if (finalMessages.length <= MAX_TOTAL_MESSAGES) {
    return finalMessages;
  }
  
  // Сортировка по приоритету
  finalMessages.sort((a, b) => {
    const categoryA = a.match(/^\[(.*?)\]/)?.[1] || 'Без категории';
    const categoryB = b.match(/^\[(.*?)\]/)?.[1] || 'Без категории';
    const priorityA = CATEGORY_PRIORITY[categoryA] || Number.MAX_SAFE_INTEGER;
    const priorityB = CATEGORY_PRIORITY[categoryB] || Number.MAX_SAFE_INTEGER;
    return priorityA - priorityB;
  });
  
  const limitedMessages = finalMessages.slice(0, MAX_TOTAL_MESSAGES);
  
  const infoMessage = `<cellbg:#FFFFCC>[Система] <color:#FF6600><b>Достигнут лимит в ${MAX_TOTAL_MESSAGES} сообщений.</b></color> ` +
    `Опущено ${finalMessages.length - MAX_TOTAL_MESSAGES} сообщений. ` +
    `Время: ${TimestampManager.format()}`;
  
  limitedMessages.push(infoMessage);
  return limitedMessages;
}

/**
 * ПРОДВИНУТОЕ ПРИМЕНЕНИЕ ФОРМАТИРОВАНИЯ
 */
function applyAdvancedFormatting(message) {
  try {
    // Парсим встроенное форматирование
    const parsed = MessageFormatter.parse(message);
    
    // Определяем категорию для базового цвета
    const categoryMatch = parsed.cleanText.match(/^\[(.*?)\]/);
    const category = categoryMatch ? categoryMatch[1] : null;
    const baseColor = CATEGORY_COLORS[category] || DEFAULT_CATEGORY_COLOR;
    
    // Создаем RichTextValue
    let builder = SpreadsheetApp.newRichTextValue().setText(parsed.cleanText);
    
    // Применяем встроенное форматирование
    builder = MessageFormatter.applyFormatting(builder, parsed.formats, parsed.cleanText, baseColor);
    
    return {
      richText: builder.build(),
      cellBackground: parsed.cellBackground || CATEGORY_BACKGROUNDS[category] || null
    };
  } catch (error) {
    console.warn(`Ошибка форматирования сообщения: ${error.message}`);
    
    // Возвращаем простой текст в случае ошибки
    const cleanText = message.replace(/<[^>]*>/g, ''); // Удаляем все теги
    const categoryMatch = cleanText.match(/^\[(.*?)\]/);
    const category = categoryMatch ? categoryMatch[1] : null;
    
    return {
      richText: SpreadsheetApp.newRichTextValue()
        .setText(cleanText)
        .setTextStyle(0, cleanText.length, 
          SpreadsheetApp.newTextStyle()
            .setForegroundColor(CATEGORY_COLORS[category] || DEFAULT_CATEGORY_COLOR)
            .build())
        .build(),
      cellBackground: CATEGORY_BACKGROUNDS[category] || null
    };
  }
}

/**
 * ОПТИМИЗИРОВАННАЯ ГЛАВНАЯ ФУНКЦИЯ
 */
function addMessagesToRange(messagesToAdd, spreadsheet, isGnn = false) {
  const RANGE_NAME = isGnn ? 'Журнал_Событий_GNN' : 'Журнал_Событий';
  const range = spreadsheet.getRangeByName(RANGE_NAME);
  
  if (!range) {
    console.error(`Диапазон "${RANGE_NAME}" не найден!`);
    return;
  }
  
  try {
    // Очищаем журнал перед записью новых сообщений
    range.clearContent();
    range.clearFormat();
    
    // Обрабатываем только новые сообщения
    const categorized = categorizeMessages(messagesToAdd);
    const grouped = groupMessagesByCategory(categorized);
    const limited = enforceTotalMessageLimit(grouped, RANGE_NAME);
    
    if (limited.length === 0) return;
    
    // Подготовка данных для массовой записи
    const outputData = [];
    const backgroundColors = [];
    
    limited.forEach((msg, index) => {
      const formatted = applyAdvancedFormatting(msg);
      outputData.push([formatted.richText]);
      backgroundColors.push([formatted.cellBackground || '#FFFFFF']);
      
      // Добавляем пустую строку между сообщениями (кроме последнего)
      if (index < limited.length - 1) {
        outputData.push([SpreadsheetApp.newRichTextValue().setText('').build()]);
        backgroundColors.push(['#FFFFFF']);
      }
    });
    
    if (outputData.length > 0) {
      // Определяем нужный размер диапазона
      const sheet = range.getSheet();
      const startRow = range.getRow();
      const startCol = range.getColumn();
      
      // Записываем данные
      const dataRange = sheet.getRange(startRow, startCol, outputData.length, 1);
      dataRange.setRichTextValues(outputData);
      
      // Применяем цвета фона (если включены)
      if (ENABLE_BACKGROUND_COLORS) {
        dataRange.setBackgrounds(backgroundColors);
      }
      
      // Устанавливаем перенос текста
      dataRange.setWrap(true);
      
      // Автоподбор высоты строк (если включен)
      if (settings['Автоподбор высоты строк']) {
        sheet.autoResizeRows(startRow, outputData.length);
      }
    }
    
    console.log(`✅ Записано ${messagesToAdd.length} новых сообщений в ${RANGE_NAME}. ` +
               `Отображено: ${limited.length} (с разделителями: ${outputData.length})`);
    
  } catch (error) {
    console.error(`❌ Ошибка записи в ${RANGE_NAME}:`, error);
    
    // Логируем ошибку с форматированием
    const errorMsg = `<cellbg:#FFCCCC>[Ошибка] <color:#CC0000><b>Ошибка записи в ${RANGE_NAME}:</b></color> ${error.message}`;
    
    try {
      // Пытаемся записать ошибку в альтернативный способ
      const errorRange = range.offset(0, 0, 1, 1);
      const formatted = applyAdvancedFormatting(errorMsg);
      errorRange.setRichTextValue(formatted.richText);
      if (formatted.cellBackground) {
        errorRange.setBackground(formatted.cellBackground);
      }
    } catch (secondError) {
      console.error('Критическая ошибка логирования:', secondError);
    }
  }
}

/**
 * ФУНКЦИИ ОБРАТНОЙ СОВМЕСТИМОСТИ
 */
function addMessagesToRange4(messages, spreadsheet) {
  addMessagesToRange(messages, spreadsheet, false);
}

function addMessagesToRange5(messages, spreadsheet) {
  addMessagesToRange(messages, spreadsheet, true);
}

function logErrorToEventLog(message, spreadsheet) {
  const errorMessage = `<cellbg:#FFEEEE>[Ошибка] <color:#CC0000><b>⚠️ ${message}</b></color>`;
  addMessagesToRange4([errorMessage], spreadsheet);
}

/**
 * НОВЫЕ ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
 */

/**
 * Очистка журнала с сохранением определенных категорий
 */
function clearEventLogExcept(keepCategories = [], rangeName = 'Журнал_Событий') {
  const range = spreadsheet.getRangeByName(rangeName);
  if (!range) return false;
  
  try {
    const messages = range.getValues().flat().filter(msg => msg.toString().trim() !== '');
    const toKeep = messages.filter(msg => {
      const category = MessageManager.extractCategory(msg);
      return keepCategories.some(keep => category.includes(keep));
    });
    
    range.clearContent();
    if (toKeep.length > 0) {
      addMessagesToRange(toKeep, spreadsheet, rangeName.includes('GNN'));
    }
    
    return true;
  } catch (error) {
    console.error('Ошибка очистки журнала:', error);
    return false;
  }
}

/**
 * ДОПОЛНИТЕЛЬНЫЕ ФУНКЦИИ ДЛЯ УПРАВЛЕНИЯ ЖУРНАЛОМ
 */

/**
 * Очистка журнала
 */
function clearEventLog(rangeName = 'Журнал_Событий') {
  try {
    const range = spreadsheet.getRangeByName(rangeName);
    if (range) {
      range.clearContent();
      range.clearFormat();
      console.log(`✅ Журнал ${rangeName} очищен`);
      return true;
    }
  } catch (error) {
    console.error('Ошибка очистки журнала:', error);
    return false;
  }
}

/**
 * Получение статистики с форматированием
 */
function getFormattedStatistics(rangeName = 'Журнал_Событий') {
  const range = spreadsheet.getRangeByName(rangeName);
  if (!range) return null;
  
  const messages = range.getValues().flat().filter(msg => msg.toString().trim() !== '');
  const categorized = categorizeMessages(messages);
  
  let report = `<cellbg:#F0F8FF><b>📊 Статистика журнала ${rangeName}</b>\n`;
  report += `<color:#666666>Время: ${TimestampManager.format()}</color>\n\n`;
  
  report += `<color:#0066CC><b>Общее количество записей:</b></color> ${messages.length}\n\n`;
  
  for (const [category, msgs] of Object.entries(categorized)) {
    const percentage = ((msgs.length / messages.length) * 100).toFixed(1);
    const color = CATEGORY_COLORS[category.replace(/[\[\]]/g, '')] || DEFAULT_CATEGORY_COLOR;
    report += `<color:${color}><b>${category}:</b></color> ${msgs.length} (${percentage}%)\n`;
  }
  
  return report;
}

/**
 * Добавление единичного сообщения (удобная функция)
 */
function addSingleMessage(message, spreadsheet, isGnn = false) {
  addMessagesToRange([message], spreadsheet, isGnn);
}

/**
 * ПРИМЕР ИСПОЛЬЗОВАНИЯ ВСТРОЕННОГО ФОРМАТИРОВАНИЯ:
 * 
 * messages.push(`<cellbg:#FFEEEE>[Ошибка] <color:#CC0000><b>🗺️ Провинция "${queueItem.province_id}" не найдена</b></color> для здания <i>"${buildingCriteria.name}"</i> (${queueItem.building_id}), позиция <u>${queueItem.queue_position || index + 1}</u>. <color:#009900>Здание удалено из очереди.</color>`);
 * 
 * Поддерживаемые теги:
 * - <cellbg:#FFEEEE> - цвет фона ячейки (в начале)
 * - <color:#CC0000>текст</color> - цвет текста
 * - <bg:#FFFF00>текст</bg> - цвет фона текста
 * - <b>текст</b> - жирный текст
 * - <i>текст</i> - курсив
 * - <u>текст</u> - подчеркивание
 * - <size:14>текст</size> - размер шрифта
 * - <font:Arial>текст</font> - шрифт
 * 
 * СИСТЕМА УПРАВЛЕНИЯ ЛИМИТАМИ:
 * 
 * Добавьте в настройки:
 * - "Включить дедупликацию" | true
 * - "Правила хранения сообщений" | {"Ошибка": {"maxCount": 50}, "Информация": {"maxCount": 20}}
 * 
 * Правила хранения (в количестве сообщений на категорию):
 * - "Критическая ошибка": максимум 100 сообщений
 * - "Ошибка": максимум 50 сообщений
 * - "Предупреждение": максимум 30 сообщений
 * - "Информация": максимум 20 сообщений
 * - "Отладка": максимум 15 сообщений
 * - "Постройки": максимум 40 сообщений
 * 
 * НОВОЕ ПОВЕДЕНИЕ:
 * 1. При каждом запуске старые сообщения УДАЛЯЮТСЯ
 * 2. Записываются только НОВЫЕ сообщения из текущего запуска
 * 3. Между сообщениями добавляются пустые строки для читаемости
 * 4. Каждая категория отображается как заголовок + список сообщений
 * 5. Автоматическое удаление дубликатов в рамках одного запуска
 * 
 * Пример результата в журнале:
 * [Ошибка] (3 сообщения)
 * • Сообщение об ошибке 1
 * 
 * • Сообщение об ошибке 2
 * 
 * • Сообщение об ошибке 3
 * 
 * [Информация] (2 сообщения)  
 * • Информационное сообщение 1
 * 
 * • Информационное сообщение 2
 */