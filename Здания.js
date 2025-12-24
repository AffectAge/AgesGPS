/* =======================
   ПРОИЗВОДСТВО (ХОД) — ФИНАЛЬНАЯ ВЕРСИЯ С ЧИСТОЙ ОБРАБОТКОЙ ПУСТЫХ ЯЧЕЕК
   ======================= */

function processProduction(data) {

  if (!data.Новости) data.Новости = [];

  // Проверка массива построек
  if (!data.Постройки || !Array.isArray(data.Постройки)) {
    data.Новости.push("Ошибка: список Постройки отсутствует или не является массивом.");
    return;
  }

  let skippedCount = 0;
  let processedCount = 0;

  for (const item of data.Постройки) {
    // === Тихо пропускаем всё "мусорное" ===
    if (!item || 
        typeof item !== 'object' || 
        Array.isArray(item) || 
        Object.keys(item).length === 0) {
      skippedCount++;
      continue;
    }

    // Проверяем обязательные поля здания
    if (!item.Тип || !item.Провинция) {
      skippedCount++;
      continue; // молча пропускаем, но считаем
    }

    // По умолчанию — активно и уровень 1
    if (item.Активно === undefined) item.Активно = true;
    if (!item.Уровень || typeof item.Уровень !== 'number' || item.Уровень < 1) {
      item.Уровень = 1;
    }

    if (!item.Активно) continue;

    processedCount++;

    const template = BUILDING_TEMPLATES[item.Тип];
    if (!template) {
      item.Активно = false;
      data.Новости.push(`Ошибка: неизвестный тип здания "\( {item.Тип}" в провинции " \){item.Провинция}".`);
      continue;
    }

    // === Проверка провинции ===
    const provKey = item.Провинция;
    const provinces = data.Провинции?.[provKey];

    if (!provinces || !Array.isArray(provinces) || provinces.length === 0) {
      item.Активно = false;
      data.Новости.push(`Ошибка: провинция "\( {provKey}" не найдена (здание " \){item.Тип}").`);
      continue;
    }

    if (provinces.length > 1) {
      data.Новости.push(`Предупреждение: провинция "${provKey}" имеет несколько записей — используется первая.`);
    }

    const province = provinces[0];

    // Проверка наличия ключевых полей в провинции
    if (!province.Провинция) {
      data.Новости.push(`Предупреждение: в провинции "${provKey}" отсутствует поле "Провинция" (используется ключ как имя).`);
    }

    if (!province.Ресурсы || !Array.isArray(province.Ресурсы) || province.Ресурсы.length === 0) {
      item.Активно = false;
      data.Новости.push(`Ошибка: в провинции "\( {province.Провинция || provKey}" отсутствует массив Ресурсы (здание " \){item.Тип}" остановлено).`);
      continue;
    }

    if (province.Ресурсы.length > 1) {
      data.Новости.push(`Предупреждение: в провинции "${province.Провинция || provKey}" несколько наборов ресурсов — используется первый.`);
    }

    const resources = province.Ресурсы[0];

    // Проверка критериев
    if (!checkProvinceCriteria(province, template.КритерииПровинции)) {
      item.Активно = false;
      data.Новости.push(`${province.Провинция || provKey}: ${item.Тип} остановлена — не выполняются критерии провинции.`);
      continue;
    }

    // Эффективность
    const eff = calculateEfficiency(province, template.Эффективность);

    // Проверка входных ресурсов
    let enough = true;
    for (const r in template.Вход) {
      const need = template.Вход[r] * item.Уровень * eff;
      if ((resources[r] || 0) < need) {
        enough = false;
        break;
      }
    }

    if (!enough) {
      item.Активно = false;
      data.Новости.push(`${province.Провинция || provKey}: ${item.Тип} остановлена — недостаточно входных ресурсов.`);
      continue;
    }

    // Списание и производство
    for (const r in template.Вход) {
      const consumed = template.Вход[r] * item.Уровень * eff;
      resources[r] = Math.max(0, (resources[r] || 0) - consumed);
    }

    for (const p in template.Выход) {
      const produced = template.Выход[p] * item.Уровень * eff;
      resources[p] = (resources[p] || 0) + produced;
    }

    data.Новости.push(
      `${province.Провинция || provKey}: ${item.Тип} (ур. ${item.Уровень}) успешно работала (эфф. ${eff.toFixed(2)}).`
    );
  }

  // === Итоговое сообщение о пропущенных записях ===
  if (skippedCount > 0) {
    data.Новости.push(`Инфо: пропущено ${skippedCount} пустых или некорректных записей в списке построек.`);
  }

  if (processedCount === 0 && skippedCount > 0) {
    data.Новости.push("Внимание: ни одно здание не было обработано (возможно, все записи пустые).");
  }
}