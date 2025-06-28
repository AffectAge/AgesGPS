/**
 * Генерация списка подходящих провинций для всех зданий
 * и запись в "Переменные" с идентификатором "Подходящие для строительства провинции"
 */
function generateEligibleProvinces(data, buildings) {
  const messages = [];

  try {
    const stateData = extractVariableData(data, 'Основные данные государства', messages);
    if (!stateData?.state_id) {
      messages.push('[Ошибка] ❗ Не удалось получить идентификатор государства');
      return messages;
    }

    const stateName = stateData.state_id;

    // Загрузка технологий государства (НОВОЕ!)
    const technologiesData = extractVariableData(data, 'Технологии государства', messages);
    const stateTechnologies = technologiesData?.technologies || [];
    messages.push(`[Подготовка] 🔬 Загружено ${stateTechnologies.length} технологий государства для анализа провинций.`);

    // Загрузка провинций
    const provincesMap = {};
    const provinceRows = Array.isArray(data['Провинции']) ? data['Провинции'] : [];

    provinceRows.forEach((row, index) => {
      if (!row || !row[0] || typeof row[0] !== 'string' || row[0].trim() === '') return;

      const parsed = safeParseJSON(row[0], 'Провинции', index, messages);
      if (!parsed || typeof parsed !== 'object') return;

      if (parsed.id) {
        provincesMap[parsed.id] = parsed;
      } else {
        Object.entries(parsed).forEach(([key, val]) => {
          if (val?.id) {
            provincesMap[val.id] = val;
          }
        });
      }
    });

    // Загрузка построек
    const existingBuildings = {};
    const buildingRows = Array.isArray(data['Постройки']) ? data['Постройки'] : [];

    buildingRows.forEach((row, index) => {
      if (!row || !row[0] || typeof row[0] !== 'string' || row[0].trim() === '') return;

      const parsed = safeParseJSON(row[0], 'Постройки', index, messages);
      if (!parsed) return;

      const arr = Array.isArray(parsed) ? parsed : [parsed];
      arr.forEach(building => {
        if (!building.province_id || !building.building_id) return;
        existingBuildings[building.province_id] = existingBuildings[building.province_id] || {};
        existingBuildings[building.province_id][building.building_id] =
          (existingBuildings[building.province_id][building.building_id] || 0) + 1;
      });
    });

    // Создание полного контекста с технологиями (ОБНОВЛЕНО!)
    const context = {
      provincesMap,
      existingBuildings,
      stateName,
      stateTechnologies, // Добавляем технологии в контекст
      messages
    };

    const result = {};
    let totalSuitableCount = 0;

    buildings.forEach(building => {
      const suitable = [];

      Object.values(provincesMap).forEach(province => {
        if (!province?.state_id || province.state_id !== stateName) return;

        const check = evaluateBuildingRequirements(
          building.requirements || building.criteria,
          province,
          context,
          building.name,
          0
        );

        if (check.meets) {
          suitable.push(province.id);
        }
      });

      result[building.id] = suitable;
      totalSuitableCount += suitable.length;
      
      // Детальная информация о каждом здании (НОВОЕ!)
      if (suitable.length === 0) {
        messages.push(`[Анализ] ❌ "${building.name}": подходящих провинций не найдено`);
      } else {
        messages.push(`[Анализ] ✅ "${building.name}": ${suitable.length} подходящих провинций`);
      }
    });

    // Обновление переменной
    if (!Array.isArray(data['Переменные'])) {
      data['Переменные'] = [];
    }

    const id = 'Подходящие для строительства провинции';
    const jsonStr = JSON.stringify(result);
    const rowIndex = data['Переменные'].findIndex(row => row[0] === id);

    if (rowIndex !== -1) {
      data['Переменные'][rowIndex][1] = jsonStr;
    } else {
      data['Переменные'].push([id, jsonStr]);
    }

    messages.push(`[✅] Сохранены подходящие провинции для ${buildings.length} зданий. Всего найдено ${totalSuitableCount} подходящих комбинаций.`);

  } catch (error) {
    messages.push(`[Ошибка] ⚠️ generateEligibleProvinces: ${error.message}`);
  }

  return messages;
}