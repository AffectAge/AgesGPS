function nextTurn() {
  runGame(
    ['Глобальные_данные', 'Данные_государства', 'Глобальные_переменные', 'Приказы'],

    TURN_gateCountryTurn,              // ✅ САМЫЙ ПЕРВЫЙ

    generateDefaultProvinces,
    generateDefaultPopulation,
    synchronizeCountryData,
    processCriteriaCheck,
    processTurnLaborOurOnOnly,
    LOG_marketsAssignCapitalsByThroughput,
    LOGISTICS_computeOurThroughputByMarkets,
    TRADE_syncMarkets,
    TRADE_computePrices,
    TRADE_runBuildingCommerce,
    ORDERS_processBuildOrders,
    BUILD_runConstructionPointsTurn,
    ORDERS_processDemolishOrders,

    TURN_commitAndAdvance              // ✅ САМЫЙ ПОСЛЕДНИЙ
  );
}