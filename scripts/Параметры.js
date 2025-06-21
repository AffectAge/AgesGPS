// Критерии зданий (упрощенные для демонстрации)
const buildings = [
  {
    id: "fusion_reactor",
    name: "Термоядерный Реактор",
    requirements: {
      logic: "AND",
      conditions: [
        { target: "province.agriculturalLand", operator: ">=", value: 50 },
        { target: "province.climate", operator: "in", value: ["умеренный", "субтропический"] }
      ]
    }
  },
  {
    id: "orbital_launcher",
    name: "Орбитальный Пусковой Комплекс", 
    requirements: {
      logic: "AND",
      conditions: [
        { target: "province.landscape", operator: "in", value: ["равнины", "плато"] },
        { target: "province.resources", logic: "AND", conditions: [
          { resourceId: "titanium", operator: ">=", value: 100 },
          { resourceId: "fuel", operator: ">=", value: 50 }
        ]}
      ]
    }
  },
  {
    id: "cloning_facility",
    name: "Центр Клонирования",
    requirements: {
      logic: "AND", 
      conditions: [
        { target: "province.pollutionLevel", operator: "<=", value: 20 },
        { target: "province.climate", operator: "==", value: "тропический" }
      ]
    }
  }
];