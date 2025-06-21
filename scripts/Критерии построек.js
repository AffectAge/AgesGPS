

/**
 * Функция для безопасного парсинга JSON с контекстным логированием
 * @param {string} str - Строка для парсинга
 * @param {string} contextLabel - Контекст для логирования
 * @param {number} rowIndex - Индекс строки
 * @param {Array} messages - Массив сообщений для добавления ошибок
 * @returns {Object|null} - Распарсенный объект или null при ошибке
 */
function safeParseJSON(str, contextLabel, rowIndex, messages) {
  try {
    return JSON.parse(str);
  } catch (e) {
    messages.push(`[Ошибка] 🚫 Не удалось распарсить JSON (${contextLabel}, строка ${rowIndex + 1}): ${e.message}`);
    return null;
  }
}

/**
 * Функция для извлечения данных из именованного диапазона "Переменные"
 * @param {Object} data - Данные из именованных диапазонов
 * @param {string} identifier - Идентификатор для поиска
 * @param {Array} messages - Массив сообщений
 * @returns {Object|null} - Извлеченные данные или null при ошибке
 */
function extractVariableData(data, identifier, messages) {
  const targetRow = data['Переменные']?.find(row => row[0] === identifier);
  if (!targetRow || !targetRow[1]) {
    messages.push(`[Ошибка] 🔍 Идентификатор "${identifier}" не найден в Переменные.`);
    return null;
  }

  const jsonMatch = targetRow[1].match(/\{.*\}/);
  if (!jsonMatch) {
    messages.push(`[Ошибка] 📝 Не удалось извлечь JSON из строки "${identifier}" в Переменные.`);
    return null;
  }

  const parsedData = safeParseJSON(jsonMatch[0], `Переменные:${identifier}`, 0, messages);
  return parsedData;
}

/**
 * Функция для проверки и обновления очереди строительства
 * @param {Object} data - Объект с данными из именованных диапазонов
 * @param {Array} buildings - Массив с критериями зданий
 * @returns {Array} messages - Массив сообщений для журнала событий
 */
function processConstructionQueue(data, buildings) {
  let messages = [];

  try {
    // 1. Получение данных о государстве
    const stateData = extractVariableData(data, 'Основные данные государства', messages);
    if (!stateData) return messages;

    const stateName = stateData.state_id;
    if (!stateName) {
      messages.push(`[Ошибка] 🏛️ Ключ "state_id" не найден в основных данных государства.`);
      return messages;
    }

    // 2. Получение очереди строительства
    const queueData = extractVariableData(data, 'Очередь строительства', messages);
    if (!queueData) return messages;

    if (!queueData.construction_queue || !Array.isArray(queueData.construction_queue)) {
      messages.push(`[Ошибка] 🏗️ Ключ "construction_queue" не найден или не является массивом в данных очереди.`);
      return messages;
    }

    const constructionQueue = queueData.construction_queue;
    messages.push(`[Строительство] 📋 Обработка очереди строительства: найдено ${constructionQueue.length} элементов.`);

    // 3. Получение данных о провинциях
    const provincesData = data['Провинции'];
    if (!provincesData || provincesData.length === 0) {
      messages.push(`[Ошибка] 🗺️ Провинции пуст или не содержит данных.`);
      return messages;
    }

    const provincesMap = {};
    let validProvinces = 0;
    provincesData.forEach((row, index) => {
      const cell = row[0];
      if (cell) {
        const provinceData = safeParseJSON(cell, 'Провинции', index, messages);
        if (provinceData && typeof provinceData === 'object') {
          // Поддержка разных форматов данных провинций
          if (provinceData.id) {
            provincesMap[provinceData.id] = provinceData;
            validProvinces++;
          } else {
            // Если данные в формате {"province_001": {...}}
            const keys = Object.keys(provinceData);
            keys.forEach(key => {
              if (provinceData[key] && provinceData[key].id) {
                provincesMap[provinceData[key].id] = provinceData[key];
                validProvinces++;
              }
            });
          }
        }
      }
    });
    messages.push(`[Строительство] 🗺️ Загружено ${validProvinces} провинций для проверки.`);

    // 4. Получение данных о существующих постройках
    const buildingsData = data['Постройки'];
    const existingBuildings = {};
    let totalBuildings = 0;

    if (buildingsData && buildingsData.length > 0) {
      buildingsData.forEach((row, index) => {
        const cell = row[0];
        if (cell) {
          const buildingData = safeParseJSON(cell, 'Постройки', index, messages);
          if (buildingData) {
            const buildingsArray = Array.isArray(buildingData) ? buildingData : [buildingData];
            buildingsArray.forEach(building => {
              if (building.province_id && building.building_id) {
                if (!existingBuildings[building.province_id]) {
                  existingBuildings[building.province_id] = {};
                }
                if (!existingBuildings[building.province_id][building.building_id]) {
                  existingBuildings[building.province_id][building.building_id] = 0;
                }
                existingBuildings[building.province_id][building.building_id]++;
                totalBuildings++;
              }
            });
          }
        }
      });
    }
    messages.push(`[GNN] 🏢 Учтено ${totalBuildings} существующих зданий при проверке требований.`);

    // 5. Обработка зданий в очереди со статусом "Подготовка" или "Строительство"
    const updatedQueue = [];
    let processedCount = 0;
    let promotedCount = 0;
    let removedCount = 0;
    
    constructionQueue.forEach((queueItem, index) => {
      if (queueItem.status === 'Подготовка' || queueItem.status === 'Строительство') {
        processedCount++;
        
        // Найти критерии здания
        const buildingCriteria = buildings.find(b => b.id === queueItem.building_id);
        
        if (!buildingCriteria) {
          messages.push(`[Предупреждение] ⚠️ Здание "${queueItem.building_id}" в провинции "${queueItem.province_id}" (позиция ${queueItem.queue_position || index + 1}) не найдено в критериях. Здание удалено из очереди.`);
          removedCount++;
          return; // Пропускаем это здание
        }

        // Найти провинцию
        const province = provincesMap[queueItem.province_id];
        
        if (!province) {
          messages.push(`<cellbg:#FFEEEE> [Ошибка] <color:#CC0000> 🗺️ Провинция "${queueItem.province_id}" не найдена </color> для здания <i>"${buildingCriteria.name}"</i> (${queueItem.building_id}), позиция <u>${queueItem.queue_position || index + 1}</u>. <color:#009900> Здание удалено из очереди.</color>`);
          removedCount++;
          return; // Пропускаем это здание
        }

        // Проверить, принадлежит ли провинция нашему государству
        if (province.state_id !== stateName) {
          messages.push(`[Предупреждение] 🏛️ Провинция "${province.name}" (${queueItem.province_id}) не принадлежит государству "${stateName}". Здание "${buildingCriteria.name}" (позиция ${queueItem.queue_position || index + 1}) удалено из очереди.`);
          removedCount++;
          return; // Пропускаем это здание
        }

        // Проверить критерии здания
        const requirementResult = evaluateBuildingRequirements(
          buildingCriteria.requirements,
          province,
          provincesMap,
          existingBuildings,
          stateName,
          messages,
          buildingCriteria.name,
          queueItem.queue_position || index + 1
        );

        if (requirementResult.meets) {
          // Критерии выполнены
          if (queueItem.status === 'Подготовка') {
            queueItem.status = 'Строительство';
            messages.push(`[Строительство] 🏗️ Здание "${buildingCriteria.name}" (${queueItem.building_id}) в провинции "${province.name}" (${queueItem.province_id}), позиция ${queueItem.queue_position || index + 1}, переведено в статус "Строительство". Владелец: ${queueItem.owner_id || 'Не указан'}.`);
            promotedCount++;
          } else {
            messages.push(`[Строительство] ✅ Здание "${buildingCriteria.name}" (${queueItem.building_id}) в провинции "${province.name}" продолжает строительство.`);
          }
          updatedQueue.push(queueItem);
        } else {
          // Критерии не выполнены
          messages.push(`[Строительство] ❌ Здание "${buildingCriteria.name}" (${queueItem.building_id}) в провинции "${province.name}" (${queueItem.province_id}), позиция ${queueItem.queue_position || index + 1}, не соответствует требованиям и удалено из очереди. Причина: ${requirementResult.reason || 'Не указана'}.`);
          removedCount++;
        }
      } else {
        // Здания с другими статусами остаются без изменений
        updatedQueue.push(queueItem);
      }
    });

    // Сводная статистика
    messages.push(`[Строительство] 📊 Обработка завершена: проверено ${processedCount} зданий, переведено в строительство ${promotedCount}, удалено ${removedCount}.`);

    // 6. Обновление очереди строительства в данных
    const updatedQueueData = { construction_queue: updatedQueue };
    
    // Найти и обновить строку с очередью строительства
    const queueIdentifier = 'Очередь строительства';
    
   if (!Array.isArray(data['Переменные'])) {
  data['Переменные'] = []; // если отсутствует, инициализируем как массив
  messages.push(`[Предупреждение] ⚠️ "Переменные" не найден — создан новый массив.`);
}

let queueRowIndex = data['Переменные'].findIndex(row => row[0] === queueIdentifier);

if (queueRowIndex !== -1) {
  data['Переменные'][queueRowIndex][1] = JSON.stringify(updatedQueueData);
  messages.push(`[Строительство] 💾 Очередь строительства обновлена: ${updatedQueue.length} элементов.`);
} else {
  data['Переменные'].push([queueIdentifier, JSON.stringify(updatedQueueData)]);
  messages.push(`[Предупреждение] ℹ️ Строка "${queueIdentifier}" не найдена — добавлена новая строка в "Переменные".`);
}

  } catch (error) {
    messages.push(`[Ошибка] 🚨 processConstructionQueue: ${error.message}`);
  }

  return messages;
}

/**
 * Функция для оценки требований здания
 * @param {Object} requirements - Требования здания
 * @param {Object} province - Данные провинции
 * @param {Object} provincesMap - Карта всех провинций
 * @param {Object} existingBuildings - Существующие здания
 * @param {string} stateName - Название государства
 * @param {Array} messages - Массив для логирования
 * @param {string} buildingName - Название здания для логирования
 * @param {number} queuePosition - Позиция в очереди
 * @returns {Object} - {meets: boolean, reason: string}
 */
function evaluateBuildingRequirements(requirements, province, provincesMap, existingBuildings, stateName, messages, buildingName, queuePosition) {
  if (!requirements || typeof requirements !== 'object') {
    return { meets: true, reason: 'Нет требований' };
  }

  const logic = requirements.logic || 'AND';
  const conditions = requirements.conditions || [];

  if (!conditions.length) {
    return { meets: true, reason: 'Пустые условия' };
  }

  const results = conditions.map((condition, index) => 
    evaluateCondition(condition, province, provincesMap, existingBuildings, stateName, messages, buildingName, queuePosition, index)
  );

  let meets = false;
  let reason = '';

  if (logic === 'AND') {
    meets = results.every(result => result.meets);
    if (!meets) {
      const failedReasons = results.filter(r => !r.meets).map(r => r.reason);
      reason = `Не выполнены условия (AND): ${failedReasons.join(', ')}`;
    }
  } else if (logic === 'OR') {
    meets = results.some(result => result.meets);
    if (!meets) {
      reason = `Ни одно условие не выполнено (OR): ${results.map(r => r.reason).join(', ')}`;
    }
  } else if (logic === 'NOT') {
    meets = !results.some(result => result.meets);
    reason = meets ? 'Условие NOT выполнено' : `Условие NOT нарушено: ${results.filter(r => r.meets).map(r => r.reason).join(', ')}`;
  } else if (logic === 'XOR') {
    const trueCount = results.filter(r => r.meets).length;
    meets = trueCount === 1;
    reason = meets ? 'Условие XOR выполнено' : `XOR требует ровно одно истинное условие, получено ${trueCount}`;
  } else if (logic === 'NAND') {
    meets = !results.every(result => result.meets);
    reason = meets ? 'Условие NAND выполнено' : 'NAND нарушено: все условия истинны';
  } else if (logic === 'NOR') {
    meets = !results.some(result => result.meets);
    reason = meets ? 'Условие NOR выполнено' : 'NOR нарушено: есть истинные условия';
  } else {
    meets = false;
    reason = `Неизвестный логический оператор: ${logic}`;
  }

  return { meets, reason };
}

/**
 * Функция для оценки отдельного условия
 * @param {Object} condition - Условие для проверки
 * @param {Object} province - Данные провинции
 * @param {Object} provincesMap - Карта всех провинций
 * @param {Object} existingBuildings - Существующие здания
 * @param {string} stateName - Название государства
 * @param {Array} messages - Массив для логирования
 * @param {string} buildingName - Название здания
 * @param {number} queuePosition - Позиция в очереди
 * @param {number} conditionIndex - Индекс условия
 * @returns {Object} - {meets: boolean, reason: string}
 */
function evaluateCondition(condition, province, provincesMap, existingBuildings, stateName, messages, buildingName, queuePosition, conditionIndex) {
  if (!condition.target) {
    return { meets: false, reason: `Условие ${conditionIndex + 1}: отсутствует target` };
  }

  // Разбор target (например, "province.agriculturalLand", "province.climate")
  const targetParts = condition.target.split('.');
  
  try {
    if (targetParts[0] === 'province') {
      return evaluateProvinceCondition(condition, province, existingBuildings, conditionIndex);
    } else if (targetParts[0] === 'state') {
      return evaluateStateCondition(condition, provincesMap, existingBuildings, stateName, conditionIndex);
    } else if (targetParts[0] === 'planet') {
      return evaluatePlanetCondition(condition, province, provincesMap, conditionIndex);
    } else if (targetParts[0] === 'world') {
      return evaluateWorldCondition(condition, provincesMap, existingBuildings, conditionIndex);
    } else {
      return { meets: false, reason: `Условие ${conditionIndex + 1}: неизвестный target "${targetParts[0]}"` };
    }
  } catch (error) {
    return { meets: false, reason: `Условие ${conditionIndex + 1}: ошибка оценки - ${error.message}` };
  }
}

/**
 * Функция для оценки условий провинции
 * @param {Object} condition - Условие для проверки
 * @param {Object} province - Данные провинции
 * @param {Object} existingBuildings - Существующие здания
 * @param {number} conditionIndex - Индекс условия
 * @returns {Object} - {meets: boolean, reason: string}
 */
function evaluateProvinceCondition(condition, province, existingBuildings, conditionIndex) {
  const targetParts = condition.target.split('.');
  const property = targetParts[1];

  if (property === 'buildings') {
    return evaluateBuildingsCondition(condition, province.id, existingBuildings, conditionIndex);
  } else if (property === 'resources') {
    return evaluateResourcesCondition(condition, province.resources || {}, conditionIndex);
  } else {
    // Простое свойство провинции
    const value = province[property];
    if (value === undefined) {
      return { meets: false, reason: `Условие ${conditionIndex + 1}: свойство "${property}" не найдено в провинции` };
    }
    return evaluateSimpleCondition(condition, value, conditionIndex, `province.${property}`);
  }
}

/**
 * Функция для оценки условий зданий в провинции
 * @param {Object} condition - Условие для проверки
 * @param {string} provinceId - ID провинции
 * @param {Object} existingBuildings - Существующие здания
 * @param {number} conditionIndex - Индекс условия
 * @returns {Object} - {meets: boolean, reason: string}
 */
function evaluateBuildingsCondition(condition, provinceId, existingBuildings, conditionIndex) {
  const logic = condition.logic || 'AND';
  const conditions = condition.conditions || [];

  if (!conditions.length) {
    return { meets: true, reason: `Условие ${conditionIndex + 1}: нет подусловий для зданий` };
  }

  const results = conditions.map((subCondition, subIndex) => {
    const buildingId = subCondition.buildingId;
    const count = existingBuildings[provinceId]?.[buildingId] || 0;
    const result = evaluateSimpleCondition(subCondition, count, subIndex, `building.${buildingId}`);
    return {
      ...result,
      buildingId,
      count,
      expectedValue: subCondition.value,
      operator: subCondition.operator
    };
  });

  let meets = false;
  let reason = '';

  if (logic === 'AND') {
    meets = results.every(result => result.meets);
    if (!meets) {
      const failedBuildings = results.filter(r => !r.meets);
      reason = `Условие ${conditionIndex + 1}: здания не соответствуют (AND): ${failedBuildings.map(r => 
        `${r.buildingId} (${r.count} ${r.operator} ${r.expectedValue})`
      ).join(', ')}`;
    } else {
      reason = `Условие ${conditionIndex + 1}: все здания соответствуют требованиям`;
    }
  } else if (logic === 'OR') {
    meets = results.some(result => result.meets);
    if (!meets) {
      reason = `Условие ${conditionIndex + 1}: ни одно здание не соответствует (OR): ${results.map(r => 
        `${r.buildingId} (${r.count} ${r.operator} ${r.expectedValue})`
      ).join(', ')}`;
    } else {
      const successBuildings = results.filter(r => r.meets);
      reason = `Условие ${conditionIndex + 1}: здания соответствуют (OR): ${successBuildings.map(r => r.buildingId).join(', ')}`;
    }
  }

  return { meets, reason };
}

/**
 * Функция для оценки условий ресурсов
 * @param {Object} condition - Условие для проверки
 * @param {Object} resources - Ресурсы провинции
 * @param {number} conditionIndex - Индекс условия
 * @returns {Object} - {meets: boolean, reason: string}
 */
function evaluateResourcesCondition(condition, resources, conditionIndex) {
  const logic = condition.logic || 'AND';
  const conditions = condition.conditions || [];

  if (!conditions.length) {
    return { meets: true, reason: `Условие ${conditionIndex + 1}: нет подусловий для ресурсов` };
  }

  const results = conditions.map((subCondition, subIndex) => {
    const resourceId = subCondition.resourceId;
    const amount = resources[resourceId] || 0;
    const result = evaluateSimpleCondition(subCondition, amount, subIndex, `resource.${resourceId}`);
    return {
      ...result,
      resourceId,
      amount,
      expectedValue: subCondition.value,
      operator: subCondition.operator
    };
  });

  let meets = false;
  let reason = '';

  if (logic === 'AND') {
    meets = results.every(result => result.meets);
    if (!meets) {
      const failedResources = results.filter(r => !r.meets);
      reason = `Условие ${conditionIndex + 1}: ресурсы недостаточны (AND): ${failedResources.map(r => 
        `${r.resourceId} (${r.amount} ${r.operator} ${r.expectedValue})`
      ).join(', ')}`;
    } else {
      reason = `Условие ${conditionIndex + 1}: все ресурсы соответствуют требованиям`;
    }
  } else if (logic === 'OR') {
    meets = results.some(result => result.meets);
    if (!meets) {
      reason = `Условие ${conditionIndex + 1}: ни один ресурс не соответствует (OR): ${results.map(r => 
        `${r.resourceId} (${r.amount} ${r.operator} ${r.expectedValue})`
      ).join(', ')}`;
    } else {
      const successResources = results.filter(r => r.meets);
      reason = `Условие ${conditionIndex + 1}: ресурсы соответствуют (OR): ${successResources.map(r => r.resourceId).join(', ')}`;
    }
  }

  return { meets, reason };
}

/**
 * Функция для оценки условий государства
 * @param {Object} condition - Условие для проверки
 * @param {Object} provincesMap - Карта всех провинций
 * @param {Object} existingBuildings - Существующие здания
 * @param {string} stateName - Название государства
 * @param {number} conditionIndex - Индекс условия
 * @returns {Object} - {meets: boolean, reason: string}
 */
function evaluateStateCondition(condition, provincesMap, existingBuildings, stateName, conditionIndex) {
  const targetParts = condition.target.split('.');
  const property = targetParts[1];

  if (property === 'buildings') {
    // Подсчет зданий во всех провинциях государства
    const stateBuildingCounts = {};
    let stateProvinceCount = 0;
    
    Object.values(provincesMap).forEach(province => {
      if (province.state_id === stateName) {
        stateProvinceCount++;
        const provinceBuildings = existingBuildings[province.id] || {};
        Object.entries(provinceBuildings).forEach(([buildingId, count]) => {
          stateBuildingCounts[buildingId] = (stateBuildingCounts[buildingId] || 0) + count;
        });
      }
    });

    const conditions = condition.conditions || [];
    const results = conditions.map((subCondition, subIndex) => {
      const buildingId = subCondition.buildingId;
      const count = stateBuildingCounts[buildingId] || 0;
      const result = evaluateSimpleCondition(subCondition, count, subIndex, `state.building.${buildingId}`);
      return {
        ...result,
        buildingId,
        count,
        expectedValue: subCondition.value,
        operator: subCondition.operator
      };
    });

    const meets = results.every(result => result.meets);
    let reason = '';
    
    if (!meets) {
      const failedBuildings = results.filter(r => !r.meets);
      reason = `Условие ${conditionIndex + 1}: здания в государстве не соответствуют: ${failedBuildings.map(r => 
        `${r.buildingId} (${r.count} ${r.operator} ${r.expectedValue})`
      ).join(', ')} (провинций в государстве: ${stateProvinceCount})`;
    } else {
      reason = `Условие ${conditionIndex + 1}: все здания в государстве соответствуют требованиям`;
    }

    return { meets, reason };
  }

  return { meets: false, reason: `Условие ${conditionIndex + 1}: неподдерживаемое свойство state.${property}` };
}

/**
 * Функция для оценки условий планеты
 * @param {Object} condition - Условие для проверки
 * @param {Object} province - Данные провинции
 * @param {Object} provincesMap - Карта всех провинций
 * @param {number} conditionIndex - Индекс условия
 * @returns {Object} - {meets: boolean, reason: string}
 */
function evaluatePlanetCondition(condition, province, provincesMap, conditionIndex) {
  const targetParts = condition.target.split('.');
  const property = targetParts[1];

  if (property === 'name') {
    const result = evaluateSimpleCondition(condition, province.planet, conditionIndex, 'planet.name');
    return {
      ...result,
      reason: result.meets ? 
        `Условие ${conditionIndex + 1}: планета "${province.planet}" соответствует требованиям` :
        `Условие ${conditionIndex + 1}: планета "${province.planet}" не соответствует требованиям`
    };
  }

  return { meets: false, reason: `Условие ${conditionIndex + 1}: неподдерживаемое свойство planet.${property}` };
}

/**
 * Функция для оценки условий мира (всех планет)
 * @param {Object} condition - Условие для проверки
 * @param {Object} provincesMap - Карта всех провинций
 * @param {Object} existingBuildings - Существующие здания
 * @param {number} conditionIndex - Индекс условия
 * @returns {Object} - {meets: boolean, reason: string}
 */
function evaluateWorldCondition(condition, provincesMap, existingBuildings, conditionIndex) {
  const targetParts = condition.target.split('.');
  const property = targetParts[1];

  if (property === 'buildings') {
    // Подсчет зданий во всем мире
    const worldBuildingCounts = {};
    
    Object.values(provincesMap).forEach(province => {
      const provinceBuildings = existingBuildings[province.id] || {};
      Object.entries(provinceBuildings).forEach(([buildingId, count]) => {
        worldBuildingCounts[buildingId] = (worldBuildingCounts[buildingId] || 0) + count;
      });
    });

    const conditions = condition.conditions || [];
    const results = conditions.map((subCondition, subIndex) => {
      const buildingId = subCondition.buildingId;
      const count = worldBuildingCounts[buildingId] || 0;
      return {
        ...evaluateSimpleCondition(subCondition, count, subIndex, `world.building.${buildingId}`),
        buildingId,
        count,
        expectedValue: subCondition.value,
        operator: subCondition.operator
      };
    });

    const meets = results.every(result => result.meets);
    let reason = '';
    
    if (!meets) {
      const failedBuildings = results.filter(r => !r.meets);
      reason = `Условие ${conditionIndex + 1}: здания в мире не соответствуют: ${failedBuildings.map(r => 
        `${r.buildingId} (${r.count} ${r.operator} ${r.expectedValue})`
      ).join(', ')}`;
    } else {
      reason = `Условие ${conditionIndex + 1}: все здания в мире соответствуют требованиям`;
    }

    return { meets, reason };
  }

  return { meets: false, reason: `Условие ${conditionIndex + 1}: неподдерживаемое свойство world.${property}` };
}

/**
 * Функция для оценки простых условий с операторами
 * @param {Object} condition - Условие с оператором и значением
 * @param {*} actualValue - Фактическое значение для сравнения
 * @param {number} conditionIndex - Индекс условия для логирования
 * @param {string} target - Целевое свойство для логирования
 * @returns {Object} - {meets: boolean, reason: string}
 */
function evaluateSimpleCondition(condition, actualValue, conditionIndex, target) {
  const operator = condition.operator;
  const expectedValue = condition.value;
  let meets = false;

  if (actualValue === undefined || actualValue === null) {
    return { 
      meets: false, 
      reason: `Условие ${conditionIndex + 1}: значение "${target}" не определено` 
    };
  }

  switch (operator) {
    case '>=':
      meets = actualValue >= expectedValue;
      break;
    case '<=':
      meets = actualValue <= expectedValue;
      break;
    case '>':
      meets = actualValue > expectedValue;
      break;
    case '<':
      meets = actualValue < expectedValue;
      break;
    case '==':
    case 'equals':
      meets = actualValue === expectedValue;
      break;
    case '!=':
    case 'not_equals':
      meets = actualValue !== expectedValue;
      break;
    case 'in':
    case 'contains':
      meets = Array.isArray(expectedValue) && expectedValue.includes(actualValue);
      break;
    case 'not_in':
    case 'not_contains':
      meets = Array.isArray(expectedValue) && !expectedValue.includes(actualValue);
      break;
    case 'starts_with':
      meets = typeof actualValue === 'string' && typeof expectedValue === 'string' && 
              actualValue.startsWith(expectedValue);
      break;
    case 'ends_with':
      meets = typeof actualValue === 'string' && typeof expectedValue === 'string' && 
              actualValue.endsWith(expectedValue);
      break;
    case 'regex':
    case 'matches':
      try {
        const regex = new RegExp(expectedValue);
        meets = regex.test(String(actualValue));
      } catch (e) {
        return { 
          meets: false, 
          reason: `Условие ${conditionIndex + 1}: ошибка в regex "${expectedValue}": ${e.message}` 
        };
      }
      break;
    case 'between':
      if (Array.isArray(expectedValue) && expectedValue.length === 2) {
        meets = actualValue >= expectedValue[0] && actualValue <= expectedValue[1];
      } else {
        return { 
          meets: false, 
          reason: `Условие ${conditionIndex + 1}: оператор "between" требует массив из 2 элементов` 
        };
      }
      break;
    case 'divisible_by':
      meets = typeof actualValue === 'number' && typeof expectedValue === 'number' && 
              expectedValue !== 0 && actualValue % expectedValue === 0;
      break;
    case 'is_empty':
      meets = expectedValue ? 
        (actualValue === '' || actualValue === null || actualValue === undefined || 
         (Array.isArray(actualValue) && actualValue.length === 0) ||
         (typeof actualValue === 'object' && Object.keys(actualValue).length === 0)) :
        !(actualValue === '' || actualValue === null || actualValue === undefined || 
         (Array.isArray(actualValue) && actualValue.length === 0) ||
         (typeof actualValue === 'object' && Object.keys(actualValue).length === 0));
      break;
    case 'length':
      const length = Array.isArray(actualValue) ? actualValue.length : 
                    typeof actualValue === 'string' ? actualValue.length : 0;
      meets = evaluateSimpleCondition(
        { operator: expectedValue.operator, value: expectedValue.value }, 
        length, 
        conditionIndex, 
        `${target}.length`
      ).meets;
      break;
    default:
      return { 
        meets: false, 
        reason: `Условие ${conditionIndex + 1}: неизвестный оператор "${operator}"` 
      };
  }

  const reason = meets ? 
    `Условие ${conditionIndex + 1}: ${target} (${actualValue}) ${operator} ${Array.isArray(expectedValue) ? `[${expectedValue.join(', ')}]` : expectedValue} ✅` :
    `Условие ${conditionIndex + 1}: ${target} (${actualValue}) ${operator} ${Array.isArray(expectedValue) ? `[${expectedValue.join(', ')}]` : expectedValue} ❌`;

  return { meets, reason };
}