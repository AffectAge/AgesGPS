# 🏗️ Система критериев для зданий

## 📋 Новый формат критериев

Вместо сложной структуры `requirements` теперь можно использовать более простую и гибкую систему `criteria`:

```json
{
  "id": "building_farm",
  "name": "Ферма",
  "requirements": {
    "logic": "AND",
    "criteria": [
      {
        "type": "climate",
        "operator": "in",
        "value": ["умеренный", "тропический", "субтропический"]
      },
      {
        "type": "terrain", 
        "operator": "equals",
        "value": "равнина"
      },
      {
        "type": "population",
        "operator": ">=",
        "value": 1000
      }
    ]
  }
}
```

## 🔧 Доступные типы критериев

### 1. **Простые свойства провинции**

#### `climate` - Проверка климата
```json
{
  "type": "climate",
  "operator": "equals",
  "value": "умеренный"
}
```

#### `terrain` - Проверка типа местности
```json
{
  "type": "terrain", 
  "operator": "in",
  "value": ["равнина", "холмы", "лес"]
}
```

#### `population` - Проверка населения
```json
{
  "type": "population",
  "operator": ">=",
  "value": 5000
}
```

#### `province_property` - Любое свойство провинции по пути
```json
{
  "type": "province_property",
  "path": "infrastructure.roads",
  "operator": ">=",
  "value": 3
}
```

### 2. **Здания**

#### `province_buildings` - Здания в провинции
```json
{
  "type": "province_buildings",
  "logic": "AND",
  "conditions": [
    {
      "building_id": "farm",
      "operator": ">=",
      "value": 2
    },
    {
      "building_id": "warehouse",
      "operator": ">=", 
      "value": 1
    }
  ]
}
```

#### `state_buildings` - Здания в государстве
```json
{
  "type": "state_buildings",
  "logic": "OR",
  "conditions": [
    {
      "building_id": "university",
      "operator": ">=",
      "value": 1
    },
    {
      "building_id": "research_lab",
      "operator": ">=",
      "value": 3
    }
  ]
}
```

#### `world_buildings` - Здания в мире
```json
{
  "type": "world_buildings",
  "conditions": [
    {
      "building_id": "nuclear_reactor",
      "operator": "<",
      "value": 10
    }
  ]
}
```

### 3. **Ресурсы**

#### `province_resources` - Ресурсы в провинции
```json
{
  "type": "province_resources",
  "logic": "AND",
  "conditions": [
    {
      "resource_id": "iron",
      "operator": ">=",
      "value": 100
    },
    {
      "resource_id": "coal",
      "operator": ">=",
      "value": 50
    }
  ]
}
```

### 4. **Планета**

#### `planet_property` - Свойства планеты
```json
{
  "type": "planet_property",
  "path": "planet",
  "operator": "equals",
  "value": "Земля"
}
```

### 5. **Кастомные формулы**

#### `custom_formula` - Сложные вычисления
```json
{
  "type": "custom_formula",
  "formula": "population + infrastructure.level * 100",
  "operator": ">=",
  "value": 2000
}
```

## 📝 Полные примеры зданий

### Простая ферма
```json
{
  "id": "simple_farm",
  "name": "Простая ферма",
  "requirements": {
    "logic": "AND",
    "criteria": [
      {
        "type": "climate",
        "operator": "in",
        "value": ["умеренный", "тропический"]
      },
      {
        "type": "terrain",
        "operator": "equals", 
        "value": "равнина"
      }
    ]
  }
}
```

### Продвинутая фабрика
```json
{
  "id": "advanced_factory",
  "name": "Продвинутая фабрика",
  "requirements": {
    "logic": "AND",
    "criteria": [
      {
        "type": "population",
        "operator": ">=",
        "value": 10000
      },
      {
        "type": "province_buildings",
        "logic": "AND",
        "conditions": [
          {
            "building_id": "power_plant",
            "operator": ">=",
            "value": 1
          },
          {
            "building_id": "warehouse",
            "operator": ">=",
            "value": 2
          }
        ]
      },
      {
        "type": "province_resources",
        "logic": "OR",
        "conditions": [
          {
            "resource_id": "iron",
            "operator": ">=",
            "value": 500
          },
          {
            "resource_id": "steel",
            "operator": ">=",
            "value": 200
          }
        ]
      },
      {
        "type": "state_buildings",
        "conditions": [
          {
            "building_id": "university",
            "operator": ">=",
            "value": 1
          }
        ]
      }
    ]
  }
}
```

### Уникальное чудо света
```json
{
  "id": "space_elevator",
  "name": "Космический лифт",
  "requirements": {
    "logic": "AND", 
    "criteria": [
      {
        "type": "world_buildings",
        "conditions": [
          {
            "building_id": "space_elevator",
            "operator": "==",
            "value": 0
          }
        ]
      },
      {
        "type": "state_buildings",
        "conditions": [
          {
            "building_id": "research_lab",
            "operator": ">=",
            "value": 5
          },
          {
            "building_id": "space_center",
            "operator": ">=",
            "value": 1
          }
        ]
      },
      {
        "type": "province_property",
        "path": "infrastructure.spaceport",
        "operator": ">=",
        "value": 10
      },
      {
        "type": "custom_formula",
        "formula": "population * 0.1 + infrastructure.level",
        "operator": ">=",
        "value": 15000
      }
    ]
  }
}
```

## 🔧 Операторы сравнения

### Числовые операторы
- `>=` - больше или равно
- `<=` - меньше или равно  
- `>` - больше
- `<` - меньше
- `==` или `equals` - равно
- `!=` или `not_equals` - не равно
- `between` - в диапазоне `[min, max]`
- `divisible_by` - делится нацело

### Строковые операторы
- `in` или `contains` - содержится в списке
- `not_in` или `not_contains` - не содержится в списке  
- `starts_with` - начинается с
- `ends_with` - заканчивается на
- `regex` или `matches` - соответствует регулярному выражению

### Специальные операторы
- `is_empty` - пустое значение (true/false)

## 🎯 Логические операторы

- `AND` - все условия должны быть выполнены
- `OR` - хотя бы одно условие должно быть выполнено
- `NOT` - ни одно условие не должно быть выполнено
- `XOR` - ровно одно условие должно быть выполнено
- `NAND` - не все условия выполнены
- `NOR` - ни одно условие не выполнено

## 🆕 Добавление новых критериев

### 1. Через конфигурацию в коде

```javascript
// Добавление нового процессора
CriteriaProcessor.addProcessor('my_custom_check', (criterion, province, context) => {
  // Ваша логика проверки
  const value = province.myCustomProperty;
  return CriteriaProcessor.evaluateCondition(value, criterion, 'my_custom_check');
});
```

### 2. Расширение существующих типов

```javascript
// Добавление проверки магических ресурсов
CriteriaProcessor.addProcessor('magic_resources', (criterion, province, context) => {
  const magicResources = province.magic || {};
  const conditions = criterion.conditions || [];
  
  const results = conditions.map(condition => {
    const amount = magicResources[condition.magic_type] || 0;
    return CriteriaProcessor.evaluateCondition(amount, condition, `magic.${condition.magic_type}`);
  });
  
  return CriteriaProcessor.combineResults(results, criterion.logic || 'AND', 'магические ресурсы');
});
```

### Использование нового критерия:
```json
{
  "type": "magic_resources",
  "logic": "AND",
  "conditions": [
    {
      "magic_type": "mana",
      "operator": ">=",
      "value": 100
    },
    {
      "magic_type": "crystals",
      "operator": ">=",
      "value": 5
    }
  ]
}
```

## 🔄 Обратная совместимость

Система поддерживает старый формат `requirements.conditions` наряду с новым `requirements.criteria`:

```json
{
  "id": "legacy_building",
  "name": "Старое здание",
  "requirements": {
    "logic": "AND",
    "conditions": [
      {
        "target": "province.climate",
        "operator": "equals",
        "value": "умеренный"
      }
    ]
  }
}
```

## 📋 Преимущества новой системы

1. **Простота** - более короткий и понятный синтаксис
2. **Гибкость** - легко добавлять новые типы критериев
3. **Производительность** - оптимизированная обработка
4. **Читаемость** - четкое разделение типов проверок
5. **Расширяемость** - возможность добавления кастомных процессоров
6. **Совместимость** - работает со старым форматом

## 🚀 Быстрый старт

1. **Замените** старый формат `requirements.conditions` на `requirements.criteria`
2. **Используйте** готовые типы критериев вместо сложных `target` путей
3. **Добавляйте** свои процессоры через `CriteriaProcessor.addProcessor()`
4. **Тестируйте** новые критерии на простых примерах

## 🔥 Максимальные возможности системы

### Все поддерживаемые логические операторы:
- `AND` - все условия ✅
- `OR` - любое условие ✅  
- `NOT` - ни одно условие ❌
- `XOR` - ровно одно условие ⚡
- `NAND` - не все условия 🚫
- `NOR` - ни одно условие 🔒

### 🏛️ Пример: Межгалактический Портал (использует ВСЕ возможности)

```json
{
  "id": "intergalactic_portal", 
  "name": "Межгалактический Портал",
  "requirements": {
    "logic": "AND",
    "criteria": [
      
      // 🌍 УНИКАЛЬНОСТЬ: Только один в мире
      {
        "type": "world_buildings",
        "logic": "AND",
        "conditions": [
          {
            "building_id": "intergalactic_portal",
            "operator": "==",
            "value": 0
          }
        ]
      },

      // 🏛️ ГОСУДАРСТВО: Мощная научная база ИЛИ технологическое превосходство
      {
        "type": "state_buildings", 
        "logic": "OR",
        "conditions": [
          // Вариант 1: Много исследовательских центров
          {
            "building_id": "research_lab",
            "operator": ">=", 
            "value": 10
          },
          // Вариант 2: Космические технологии
          {
            "building_id": "space_center",
            "operator": ">=",
            "value": 3
          }
        ]
      },

      // 🌟 ПЛАНЕТА: НЕ должна быть радиоактивной или мертвой
      {
        "type": "planet_property",
        "path": "planet_type", 
        "logic": "NOT",
        "operator": "in",
        "value": ["радиоактивная", "мертвая", "газовый_гигант"]
      },

      // 🌡️ КЛИМАТ: XOR - либо идеальный, либо экстремальный (но не обычный!)
      {
        "type": "climate",
        "logic": "XOR", 
        "operator": "in",
        "value": ["идеальный", "экстремальный"]
      },

      // 🏗️ ПРОВИНЦИЯ: Сложная комбинация зданий
      {
        "type": "province_buildings",
        "logic": "AND",
        "conditions": [
          // Обязательно: энергетическая база
          {
            "building_id": "fusion_reactor",
            "operator": ">=",
            "value": 2
          },
          // XOR: либо квантовые лаборатории, либо псионические центры
          {
            "logic": "XOR",
            "sub_conditions": [
              {
                "building_id": "quantum_lab", 
                "operator": ">=",
                "value": 3
              },
              {
                "building_id": "psionic_center",
                "operator": ">=", 
                "value": 2
              }
            ]
          },
          // NOT: никаких примитивных зданий
          {
            "logic": "NOT",
            "sub_conditions": [
              {
                "building_id": "farm",
                "operator": ">",
                "value": 0
              },
              {
                "building_id": "windmill", 
                "operator": ">",
                "value": 0
              }
            ]
          }
        ]
      },

      // 💎 РЕСУРСЫ: Редкие материалы (NAND - не все одновременно)
      {
        "type": "province_resources",
        "logic": "NAND",
        "conditions": [
          {
            "resource_id": "antimatter",
            "operator": ">=",
            "value": 1000
          },
          {
            "resource_id": "dark_matter", 
            "operator": ">=",
            "value": 500
          },
          {
            "resource_id": "exotic_matter",
            "operator": ">=", 
            "value": 100
          }
        ]
      },

      // 👥 НАСЕЛЕНИЕ: Высокотехнологичное общество
      {
        "type": "population",
        "operator": "between",
        "value": [50000, 200000]
      },

      // 🔬 КАСТОМНАЯ ФОРМУЛА: Индекс технологического развития  
      {
        "type": "custom_formula",
        "formula": "(population * 0.001) + (infrastructure.level * 10) + (science.level * 50)",
        "operator": ">=",
        "value": 1000
      },

      // 🏞️ МЕСТНОСТЬ: NOR - не равнины и не пустыни
      {
        "type": "terrain",
        "logic": "NOR", 
        "operator": "in",
        "value": ["равнина", "пустыня", "болото"]
      },

      // 🌌 СПЕЦИАЛЬНОЕ СВОЙСТВО: Магнитные аномалии
      {
        "type": "province_property",
        "path": "anomalies.magnetic", 
        "operator": ">=",
        "value": 5
      },

      // 🔮 МАГИЧЕСКИЕ РЕСУРСЫ (кастомный критерий)
      {
        "type": "magic_resources",
        "logic": "OR",
        "conditions": [
          {
            "magic_type": "void_crystals",
            "operator": ">=", 
            "value": 10
          },
          {
            "magic_type": "reality_shards",
            "operator": ">=",
            "value": 5
          }
        ]
      }
    ]
  }
}
```

### 🎯 Объяснение сложной логики:

1. **AND (корневой)**: Все группы условий должны выполняться
2. **OR (государство)**: Либо много лабораторий, либо космические центры  
3. **NOT (планета)**: Планета НЕ должна быть в списке непригодных
4. **XOR (климат)**: Только один из двух экстремальных климатов
5. **NAND (ресурсы)**: НЕ все редкие ресурсы одновременно (баланс)
6. **NOR (местность)**: НИ равнины, НИ пустыни, НИ болота

### 🚀 Вложенная логика в зданиях провинции (ПОДДЕРЖИВАЕТСЯ!):

```json
{
  "type": "province_buildings",
  "logic": "AND", 
  "conditions": [
    // Простое условие
    {
      "building_id": "fusion_reactor",
      "operator": ">=",
      "value": 2
    },
    // Вложенная XOR логика
    {
      "logic": "XOR",
      "sub_conditions": [
        {
          "building_id": "quantum_lab",
          "operator": ">=", 
          "value": 3
        },
        {
          "building_id": "psionic_center", 
          "operator": ">=",
          "value": 2
        }
      ]
    },
    // Вложенная NOT логика
    {
      "logic": "NOT",
      "sub_conditions": [
        {
          "building_id": "farm",
          "operator": ">",
          "value": 0
        },
        {
          "building_id": "windmill",
          "operator": ">", 
          "value": 0
        }
      ]
    },
    // Многоуровневая вложенность
    {
      "logic": "OR",
      "sub_conditions": [
        {
          "building_id": "advanced_lab",
          "operator": ">=",
          "value": 1
        },
        {
          "logic": "AND",
          "sub_conditions": [
            {
              "building_id": "basic_lab",
              "operator": ">=",
              "value": 3
            },
            {
              "building_id": "library",
              "operator": ">=",
              "value": 2
            }
          ]
        }
      ]
    }
  ]
}
```

### 🔮 Сложные критерии с универсальной вложенностью:

```json
{
  "type": "complex_criteria",
  "logic": "AND",
  "conditions": [
    // Простой критерий климата
    {
      "type": "climate",
      "operator": "equals",
      "value": "умеренный"
    },
    // Вложенная логика из разных типов критериев
    {
      "logic": "OR",
      "sub_conditions": [
        {
          "type": "population",
          "operator": ">=",
          "value": 10000
        },
        {
          "logic": "AND",
          "sub_conditions": [
            {
              "type": "terrain",
              "operator": "equals",
              "value": "горы"
            },
            {
              "property": "infrastructure.level",
              "operator": ">=",
              "value": 5
            }
          ]
        }
      ]
    }
  ]
}
```

### 💎 Магические ресурсы с вложенностью:

```json
{
  "type": "magic_resources",
  "logic": "OR",
  "conditions": [
    // Простое условие
    {
      "magic_type": "mana",
      "operator": ">=",
      "value": 100
    },
    // Вложенная логика
    {
      "logic": "AND", 
      "sub_conditions": [
        {
          "magic_type": "crystals",
          "operator": ">=",
          "value": 5
        },
        {
          "magic_type": "essence",
          "operator": ">=",
          "value": 20
        }
      ]
    },
    // Многоуровневая вложенность
    {
      "logic": "XOR",
      "sub_conditions": [
        {
          "magic_type": "void_energy",
          "operator": ">=",
          "value": 50
        },
        {
          "logic": "NOT",
          "sub_conditions": [
            {
              "magic_type": "dark_magic",
              "operator": ">",
              "value": 0
            }
          ]
        }
      ]
    }
  ]
}
```

## 🎯 Примеры реальных зданий с sub_conditions:

### 🏰 Магическая Крепость
```json
{
  "id": "magic_fortress",
  "name": "Магическая Крепость", 
  "requirements": {
    "logic": "AND",
    "criteria": [
      {
        "type": "terrain",
        "operator": "in",
        "value": ["горы", "холмы"]
      },
      {
        "type": "province_buildings",
        "logic": "AND",
        "conditions": [
          {
            "building_id": "mage_tower",
            "operator": ">=",
            "value": 1
          },
          {
            "logic": "OR",
            "sub_conditions": [
              // Либо много простых зданий
              {
                "logic": "AND",
                "sub_conditions": [
                  {
                    "building_id": "barracks",
                    "operator": ">=",
                    "value": 3
                  },
                  {
                    "building_id": "armory",
                    "operator": ">=", 
                    "value": 2
                  }
                ]
              },
              // Либо одно элитное
              {
                "building_id": "royal_guard_post",
                "operator": ">=",
                "value": 1
              }
            ]
          }
        ]
      },
      {
        "type": "magic_resources",
        "logic": "AND",
        "conditions": [
          {
            "magic_type": "mana",
            "operator": ">=",
            "value": 200
          },
          {
            "logic": "XOR",
            "sub_conditions": [
              {
                "magic_type": "fire_crystals",
                "operator": ">=",
                "value": 10
              },
              {
                "magic_type": "ice_crystals", 
                "operator": ">=",
                "value": 10
              }
            ]
          }
        ]
      }
    ]
  }
}
```

### 🎓 Технологический Университет
```json
{
  "id": "tech_university",
  "name": "Технологический Университет",
  "requirements": {
    "logic": "AND", 
    "criteria": [
      {
        "type": "population",
        "operator": ">=",
        "value": 25000
      },
      {
        "type": "complex_criteria",
        "logic": "OR",
        "conditions": [
          // Вариант 1: Высокий уровень развития
          {
            "logic": "AND",
            "sub_conditions": [
              {
                "type": "custom_formula",
                "formula": "infrastructure.level + science.level",
                "operator": ">=",
                "value": 15
              },
              {
                "property": "education.literacy_rate",
                "operator": ">=",
                "value": 0.8
              }
            ]
          },
          // Вариант 2: Много исследовательских зданий
          {
            "type": "province_buildings",
            "logic": "AND",
            "conditions": [
              {
                "building_id": "library",
                "operator": ">=",
                "value": 2
              },
              {
                "logic": "OR",
                "sub_conditions": [
                  {
                    "building_id": "research_lab",
                    "operator": ">=",
                    "value": 3
                  },
                  {
                    "logic": "AND",
                    "sub_conditions": [
                      {
                        "building_id": "observatory",
                        "operator": ">=",
                        "value": 1
                      },
                      {
                        "building_id": "workshop",
                        "operator": ">=",
                        "value": 2
                      }
                    ]
                  }
                ]
              }
            ]
          }
        ]
      }
    ]
  }
}
```

## ✨ Ключевые особенности sub_conditions:

1. **Безграничная вложенность** - можно создавать критерии любой сложности
2. **Поддержка всех типов** - здания, ресурсы, магия, свойства
3. **Рекурсивная обработка** - sub_conditions внутри sub_conditions
4. **Все логические операторы** - AND, OR, NOT, XOR, NAND, NOR
5. **Смешанные типы** - разные критерии в одной вложенной группе

Теперь можно создавать критерии любой сложности! 🎉

## 💡 Практические примеры использования:

### 🏭 **Фабрика (простая)**
```json
{
  "logic": "AND",
  "criteria": [
    {"type": "population", "operator": ">=", "value": 1000},
    {"type": "terrain", "operator": "equals", "value": "равнина"}
  ]
}
```

### ⚔️ **Крепость (средняя сложность)**
```json
{
  "logic": "AND", 
  "criteria": [
    {
      "type": "terrain",
      "logic": "OR",
      "operator": "in", 
      "value": ["холмы", "горы", "утесы"]
    },
    {
      "type": "province_buildings",
      "logic": "XOR",
      "conditions": [
        {"building_id": "quarry", "operator": ">=", "value": 1},
        {"building_id": "mine", "operator": ">=", "value": 1}
      ]
    }
  ]
}
```

### 🌟 **Чудо света (максимальная сложность)**
- Использует все 6 логических операторов
- Вложенные условия
- Кастомные формулы
- Глобальные ограничения

Система поддерживает **любые комбинации** логики! 🎉

Теперь добавление новых критериев стало намного проще и не требует изменения основного кода!