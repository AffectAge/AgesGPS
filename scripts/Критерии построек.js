/**
 * Безопасный парсинг JSON с логированием ошибок
 */
function safeParseJSON(str, context, rowIndex, messages) {
  try {
    return JSON.parse(str);
  } catch (e) {
    messages.push(`[Ошибка] 🚫 Не удалось распарсить JSON (${context}, строка ${rowIndex + 1}): ${e.message}`);
    return null;
  }
}

/**
 * Извлечение данных из именованного диапазона "Переменные"
 */
function extractVariableData(data, identifier, messages) {
  const targetRow = data['Переменные']?.find(row => row[0] === identifier);
  if (!targetRow?.[1]) {
    messages.push(`[Ошибка] 🔍 Идентификатор "${identifier}" не найден в Переменные.`);
    return null;
  }

  const jsonMatch = targetRow[1].match(/\{.*\}/);
  if (!jsonMatch) {
    messages.push(`[Ошибка] 📝 Не удалось извлечь JSON из строки "${identifier}" в Переменные.`);
    return null;
  }

  return safeParseJSON(jsonMatch[0], `Переменные:${identifier}`, 0, messages);
}

/**
 * Получение значения из объекта по пути (например, "climate.temperature" или "resources.iron")
 */
function getValueByPath(obj, path) {
  return path.split('.').reduce((current, key) => current?.[key], obj);
}

/**
 * Основная функция обработки очереди строительства
 */
function processConstructionQueue(data, buildings) {
  const messages = [];

  try {
    // Получение базовых данных
    const stateData = extractVariableData(data, 'Основные данные государства', messages);
    if (!stateData) return messages;

    const stateName = stateData.state_id;
    if (!stateName) {
      messages.push(`[Ошибка] 🏛️ Ключ "state_id" не найден в основных данных государства.`);
      return messages;
    }

    const queueData = extractVariableData(data, 'Очередь строительства', messages);
    if (!queueData?.construction_queue || !Array.isArray(queueData.construction_queue)) {
      messages.push(`[Ошибка] 🏗️ Очередь строительства не найдена или не является массивом.`);
      return messages;
    }

    const constructionQueue = queueData.construction_queue;
    messages.push(`[Строительство] 📋 Обработка очереди: ${constructionQueue.length} элементов.`);

    // Обработка провинций
    const provincesMap = {};
    let validProvinces = 0;
    
    (data['Провинции'] || []).forEach((row, index) => {
      if (!row[0]) return;
      
      const provinceData = safeParseJSON(row[0], 'Провинции', index, messages);
      if (!provinceData || typeof provinceData !== 'object') return;

      if (provinceData.id) {
        provincesMap[provinceData.id] = provinceData;
        validProvinces++;
      } else {
        // Формат {"province_001": {...}}
        Object.entries(provinceData).forEach(([key, value]) => {
          if (value?.id) {
            provincesMap[value.id] = value;
            validProvinces++;
          }
        });
      }
    });

    messages.push(`[Строительство] 🗺️ Загружено ${validProvinces} провинций.`);

    // Обработка существующих зданий
    const existingBuildings = {};
    let totalBuildings = 0;

    (data['Постройки'] || []).forEach((row, index) => {
      if (!row[0]) return;
      
      const buildingData = safeParseJSON(row[0], 'Постройки', index, messages);
      if (!buildingData) return;

      const buildingsArray = Array.isArray(buildingData) ? buildingData : [buildingData];
      buildingsArray.forEach(building => {
        if (!building.province_id || !building.building_id) return;
        
        existingBuildings[building.province_id] = existingBuildings[building.province_id] || {};
        existingBuildings[building.province_id][building.building_id] = 
          (existingBuildings[building.province_id][building.building_id] || 0) + 1;
        totalBuildings++;
      });
    });

    messages.push(`[GNN] 🏢 Учтено ${totalBuildings} существующих зданий.`);

    // Создание контекста для оценки критериев
    const evaluationContext = {
      provincesMap,
      existingBuildings,
      stateName,
      messages
    };

    // Обработка очереди
    const updatedQueue = [];
    let processedCount = 0, promotedCount = 0, removedCount = 0;
    
    constructionQueue.forEach((queueItem, index) => {
      if (queueItem.status !== 'Подготовка' && queueItem.status !== 'Строительство') {
        updatedQueue.push(queueItem);
        return;
      }

      processedCount++;
      const buildingCriteria = buildings.find(b => b.id === queueItem.building_id);
      
      if (!buildingCriteria) {
        messages.push(`[Предупреждение] ⚠️ Здание "${queueItem.building_id}" не найдено в критериях. Удалено из очереди.`);
        removedCount++;
        return;
      }

      const province = provincesMap[queueItem.province_id];
      if (!province) {
        messages.push(`<cellbg:#FFEEEE> [Ошибка] <color:#CC0000> 🗺️ Провинция "${queueItem.province_id}" не найдена </color> для здания <i>"${buildingCriteria.name}"</i>. <color:#009900> Удалено из очереди.</color>`);
        removedCount++;
        return;
      }

      if (province.state_id !== stateName) {
        messages.push(`[Предупреждение] 🏛️ Провинция "${province.name}" не принадлежит государству "${stateName}". Удалено из очереди.`);
        removedCount++;
        return;
      }

      const requirementResult = evaluateBuildingRequirements(
        buildingCriteria.requirements || buildingCriteria.criteria, // поддержка обоих вариантов
        province, 
        evaluationContext,
        buildingCriteria.name, 
        queueItem.queue_position || index + 1
      );

      if (requirementResult.meets) {
        if (queueItem.status === 'Подготовка') {
          queueItem.status = 'Строительство';
          messages.push(`[Строительство] 🏗️ "${buildingCriteria.name}" в "${province.name}" переведено в статус "Строительство".`);
          promotedCount++;
        }
        updatedQueue.push(queueItem);
      } else {
        messages.push(`[Строительство] ❌ "${buildingCriteria.name}" в "${province.name}" не соответствует требованиям и удалено. Причина: ${requirementResult.reason}`);
        removedCount++;
      }
    });

    messages.push(`[Строительство] 📊 Обработано ${processedCount}, переведено в строительство ${promotedCount}, удалено ${removedCount}.`);

    // Обновление данных
    const updatedQueueData = { construction_queue: updatedQueue };
    
    if (!Array.isArray(data['Переменные'])) {
      data['Переменные'] = [];
      messages.push(`[Предупреждение] ⚠️ "Переменные" не найден — создан новый массив.`);
    }

    const queueRowIndex = data['Переменные'].findIndex(row => row[0] === 'Очередь строительства');
    
    if (queueRowIndex !== -1) {
      data['Переменные'][queueRowIndex][1] = JSON.stringify(updatedQueueData);
      messages.push(`[Строительство] 💾 Очередь обновлена: ${updatedQueue.length} элементов.`);
    } else {
      data['Переменные'].push(['Очередь строительства', JSON.stringify(updatedQueueData)]);
      messages.push(`[Предупреждение] ℹ️ Добавлена новая строка "Очередь строительства" в "Переменные".`);
    }

  } catch (error) {
    messages.push(`[Ошибка] 🚨 processConstructionQueue: ${error.message}`);
  }

  return messages;
}

/**
 * Система обработки кастомных критериев
 */
const CriteriaProcessor = {
  /**
   * Регистрация кастомных процессоров критериев
   */
  processors: new Map(),

  /**
   * Добавление нового процессора критерия
   */
  addProcessor(name, processor) {
    this.processors.set(name, processor);
  },

  /**
   * Получение процессора по имени
   */
  getProcessor(name) {
    return this.processors.get(name);
  },

  /**
   * Инициализация базовых процессоров
   */
  init() {
    // Базовые процессоры
    this.addProcessor('province_property', (criterion, province, context) => {
      const value = getValueByPath(province, criterion.path);
      return this.evaluateCondition(value, criterion, `province.${criterion.path}`);
    });

    this.addProcessor('province_buildings', (criterion, province, context) => {
      const provinceBuildings = context.existingBuildings[province.id] || {};
      return this.evaluateBuildingCriteria(criterion, provinceBuildings);
    });

    this.addProcessor('province_resources', (criterion, province, context) => {
      const resources = province.resources || {};
      return this.evaluateResourceCriteria(criterion, resources);
    });

    this.addProcessor('state_buildings', (criterion, province, context) => {
      const stateBuildingCounts = {};
      Object.values(context.provincesMap).forEach(p => {
        if (p.state_id === context.stateName) {
          const pBuildings = context.existingBuildings[p.id] || {};
          Object.entries(pBuildings).forEach(([buildingId, count]) => {
            stateBuildingCounts[buildingId] = (stateBuildingCounts[buildingId] || 0) + count;
          });
        }
      });
      return this.evaluateBuildingCriteria(criterion, stateBuildingCounts);
    });

    this.addProcessor('world_buildings', (criterion, province, context) => {
      const worldBuildingCounts = {};
      Object.values(context.provincesMap).forEach(p => {
        const pBuildings = context.existingBuildings[p.id] || {};
        Object.entries(pBuildings).forEach(([buildingId, count]) => {
          worldBuildingCounts[buildingId] = (worldBuildingCounts[buildingId] || 0) + count;
        });
      });
      return this.evaluateBuildingCriteria(criterion, worldBuildingCounts);
    });

    this.addProcessor('planet_property', (criterion, province, context) => {
      const value = getValueByPath(province, criterion.path);
      return this.evaluateCondition(value, criterion, `planet.${criterion.path}`);
    });

    // Дополнительные процессоры для удобства
    this.addProcessor('climate', (criterion, province, context) => {
      const value = getValueByPath(province, 'climate');
      return this.evaluateCondition(value, criterion, 'climate');
    });

    this.addProcessor('terrain', (criterion, province, context) => {
      const value = getValueByPath(province, 'terrain');
      return this.evaluateCondition(value, criterion, 'terrain');
    });

    this.addProcessor('population', (criterion, province, context) => {
      const value = getValueByPath(province, 'population');
      return this.evaluateCondition(value, criterion, 'population');
    });

    this.addProcessor('custom_formula', (criterion, province, context) => {
      // Для сложных пользовательских формул
      try {
        const formula = criterion.formula;
        // Простой интерпретатор формул (можно расширить)
        const result = this.evaluateFormula(formula, province, context);
        return this.evaluateCondition(result, criterion, 'custom_formula');
      } catch (error) {
        return { meets: false, reason: `Ошибка в формуле: ${error.message}` };
      }
    });
  },

  /**
   * Оценка формулы (упрощенная версия)
   */
  evaluateFormula(formula, province, context) {
    // Замена переменных в формуле
    let processedFormula = formula
      .replace(/province\.(\w+)/g, (match, prop) => getValueByPath(province, prop) || 0)
      .replace(/population/g, province.population || 0)
      .replace(/climate/g, `"${province.climate || 'unknown'}"`)
      .replace(/terrain/g, `"${province.terrain || 'unknown'}"`);

    // Безопасная оценка (только числовые операции)
    if (/^[\d+\-*\/\s().]+$/.test(processedFormula)) {
      return eval(processedFormula);
    }
    
    throw new Error('Небезопасная формула');
  },

  /**
   * Оценка зданий с поддержкой вложенной логики
   */
  evaluateBuildingCriteria(criterion, buildingCounts) {
    const conditions = criterion.conditions || [];
    const logic = criterion.logic || 'AND';
    
    const results = conditions.map((condition, index) => {
      // Поддержка вложенных sub_conditions
      if (condition.sub_conditions) {
        return this.evaluateSubConditions(condition, buildingCounts, 'building');
      }
      
      // Обычное условие здания
      if (condition.building_id) {
        const count = buildingCounts[condition.building_id] || 0;
        return this.evaluateCondition(count, condition, `building.${condition.building_id}`);
      }
      
      return { meets: false, reason: `Условие ${index + 1}: отсутствует building_id` };
    });

    return this.combineResults(results, logic, 'здания');
  },

  /**
   * Оценка ресурсов с поддержкой вложенной логики
   */
  evaluateResourceCriteria(criterion, resources) {
    const conditions = criterion.conditions || [];
    const logic = criterion.logic || 'AND';
    
    const results = conditions.map((condition, index) => {
      // Поддержка вложенных sub_conditions
      if (condition.sub_conditions) {
        return this.evaluateSubConditions(condition, resources, 'resource');
      }
      
      // Обычное условие ресурса
      if (condition.resource_id) {
        const amount = resources[condition.resource_id] || 0;
        return this.evaluateCondition(amount, condition, `resource.${condition.resource_id}`);
      }
      
      return { meets: false, reason: `Условие ${index + 1}: отсутствует resource_id` };
    });

    return this.combineResults(results, logic, 'ресурсы');
  },

  /**
   * Оценка вложенных sub_conditions
   */
  evaluateSubConditions(condition, dataMap, prefix) {
    const subConditions = condition.sub_conditions || [];
    const subLogic = condition.logic || 'AND';
    
    const subResults = subConditions.map((subCondition, subIndex) => {
      // Рекурсивная поддержка вложенности
      if (subCondition.sub_conditions) {
        return this.evaluateSubConditions(subCondition, dataMap, prefix);
      }
      
      // Обработка в зависимости от типа
      if (prefix === 'building' && subCondition.building_id) {
        const count = dataMap[subCondition.building_id] || 0;
        return this.evaluateCondition(count, subCondition, `${prefix}.${subCondition.building_id}`);
      } else if (prefix === 'resource' && subCondition.resource_id) {
        const amount = dataMap[subCondition.resource_id] || 0;
        return this.evaluateCondition(amount, subCondition, `${prefix}.${subCondition.resource_id}`);
      } else if (prefix === 'magic' && subCondition.magic_type) {
        const amount = dataMap[subCondition.magic_type] || 0;
        return this.evaluateCondition(amount, subCondition, `${prefix}.${subCondition.magic_type}`);
      }
      
      return { meets: false, reason: `Подусловие ${subIndex + 1}: неверный формат для типа ${prefix}` };
    });

    return this.combineResults(subResults, subLogic, `подусловия (${prefix})`);
  },

  /**
   * Оценка простого условия
   */
  evaluateCondition(actualValue, condition, target) {
    const { operator, value: expectedValue } = condition;

    if (actualValue === undefined || actualValue === null) {
      return { meets: false, reason: `Значение "${target}" не определено` };
    }

    const operators = {
      '>=': () => actualValue >= expectedValue,
      '<=': () => actualValue <= expectedValue,
      '>': () => actualValue > expectedValue,
      '<': () => actualValue < expectedValue,
      '==': () => actualValue === expectedValue,
      'equals': () => actualValue === expectedValue,
      '!=': () => actualValue !== expectedValue,
      'not_equals': () => actualValue !== expectedValue,
      'in': () => Array.isArray(expectedValue) && expectedValue.includes(actualValue),
      'contains': () => Array.isArray(expectedValue) && expectedValue.includes(actualValue),
      'not_in': () => Array.isArray(expectedValue) && !expectedValue.includes(actualValue),
      'not_contains': () => Array.isArray(expectedValue) && !expectedValue.includes(actualValue),
      'starts_with': () => typeof actualValue === 'string' && typeof expectedValue === 'string' && actualValue.startsWith(expectedValue),
      'ends_with': () => typeof actualValue === 'string' && typeof expectedValue === 'string' && actualValue.endsWith(expectedValue),
      'regex': () => {
        try {
          return new RegExp(expectedValue).test(String(actualValue));
        } catch (e) {
          throw new Error(`ошибка в regex "${expectedValue}": ${e.message}`);
        }
      },
      'matches': () => operators.regex(),
      'between': () => Array.isArray(expectedValue) && expectedValue.length === 2 && actualValue >= expectedValue[0] && actualValue <= expectedValue[1],
      'divisible_by': () => typeof actualValue === 'number' && typeof expectedValue === 'number' && expectedValue !== 0 && actualValue % expectedValue === 0,
      'is_empty': () => {
        const isEmpty = actualValue === '' || actualValue === null || actualValue === undefined || 
                       (Array.isArray(actualValue) && actualValue.length === 0) ||
                       (typeof actualValue === 'object' && Object.keys(actualValue).length === 0);
        return expectedValue ? isEmpty : !isEmpty;
      }
    };

    try {
      const meets = operators[operator] ? operators[operator]() : false;
      
      if (!operators[operator]) {
        return { meets: false, reason: `Неизвестный оператор "${operator}"` };
      }

      const reason = `${target} (${actualValue}) ${operator} ${Array.isArray(expectedValue) ? `[${expectedValue.join(', ')}]` : expectedValue} ${meets ? '✅' : '❌'}`;

      return { meets, reason };
    } catch (error) {
      return { meets: false, reason: error.message };
    }
  },

  /**
   * Объединение результатов по логике
   */
  combineResults(results, logic, type) {
    const logicMap = {
      'AND': () => results.every(r => r.meets),
      'OR': () => results.some(r => r.meets),
      'NOT': () => !results.some(r => r.meets),
      'XOR': () => results.filter(r => r.meets).length === 1,
      'NAND': () => !results.every(r => r.meets),
      'NOR': () => !results.some(r => r.meets)
    };

    const meets = logicMap[logic] ? logicMap[logic]() : false;
    let reason = '';

    if (!meets) {
      const failedReasons = results.filter(r => !r.meets).map(r => r.reason);
      reason = logic === 'OR' ? 
        `Ни одно условие не выполнено (OR): ${results.map(r => r.reason).join(', ')}` :
        `Не выполнены условия ${type} (${logic}): ${failedReasons.join(', ')}`;
    } else {
      reason = `${type} соответствуют требованиям (${logic})`;
    }

    return { meets, reason };
  }
};

// Инициализация процессоров
CriteriaProcessor.init();

/**
 * Оценка требований здания с новой системой критериев
 */
function evaluateBuildingRequirements(requirements, province, context, buildingName, queuePosition) {
  if (!requirements || typeof requirements !== 'object') {
    return { meets: true, reason: 'Нет требований' };
  }

  // Поддержка старого формата
  if (requirements.conditions) {
    return evaluateLegacyRequirements(requirements, province, context);
  }

  // Новый формат с кастомными критериями
  const { logic = 'AND', criteria = [] } = requirements;
  if (!criteria.length) return { meets: true, reason: 'Нет критериев' };

  const results = criteria.map((criterion, index) => {
    const processor = CriteriaProcessor.getProcessor(criterion.type);
    
    if (!processor) {
      return { 
        meets: false, 
        reason: `Неизвестный тип критерия: "${criterion.type}"` 
      };
    }

    try {
      return processor(criterion, province, context);
    } catch (error) {
      return { 
        meets: false, 
        reason: `Ошибка в критерии ${criterion.type}: ${error.message}` 
      };
    }
  });

  return CriteriaProcessor.combineResults(results, logic, 'критерии');
}

/**
 * Обработка старого формата требований (для обратной совместимости)
 */
function evaluateLegacyRequirements(requirements, province, context) {
  const { logic = 'AND', conditions = [] } = requirements;
  if (!conditions.length) return { meets: true, reason: 'Пустые условия' };

  const results = conditions.map((condition, index) => 
    evaluateLegacyCondition(condition, province, context, index)
  );

  return CriteriaProcessor.combineResults(results, logic, 'условия');
}

/**
 * Оценка старого формата условий
 */
function evaluateLegacyCondition(condition, province, context, conditionIndex) {
  if (!condition.target) {
    return { meets: false, reason: `Условие ${conditionIndex + 1}: отсутствует target` };
  }

  const [targetType, ...targetPath] = condition.target.split('.');
  
  try {
    const evaluators = {
      'province': () => {
        const property = targetPath[0];
        if (property === 'buildings') {
          return CriteriaProcessor.evaluateBuildingCriteria(condition, context.existingBuildings[province.id] || {});
        }
        if (property === 'resources') {
          return CriteriaProcessor.evaluateResourceCriteria(condition, province.resources || {});
        }
        const value = province[property];
        if (value === undefined) {
          return { meets: false, reason: `Свойство "${property}" не найдено в провинции` };
        }
        return CriteriaProcessor.evaluateCondition(value, condition, `province.${property}`);
      },
      'state': () => {
        // Реализация state логики...
        return { meets: true, reason: 'State условия пока не реализованы в legacy режиме' };
      },
      'planet': () => {
        const value = getValueByPath(province, targetPath.join('.'));
        return CriteriaProcessor.evaluateCondition(value, condition, `planet.${targetPath.join('.')}`);
      },
      'world': () => {
        // Реализация world логики...
        return { meets: true, reason: 'World условия пока не реализованы в legacy режиме' };
      }
    };

    return evaluators[targetType] ? evaluators[targetType]() : 
      { meets: false, reason: `Неизвестный target "${targetType}"` };
  } catch (error) {
    return { meets: false, reason: `Ошибка оценки: ${error.message}` };
  }
}