/* =========================================================
   NEWS (PLAIN) — DROP-IN MODULE FOR YOUR ENGINE
   - No changes to read/write/runGame
   - Buffer: data.__NEWS_BUFFER__ (NOT data.Новости)
   - Output columns (existing in your headers):
       * data["Новости"]           (default)
       * data["Новости.Логистика"] (only category "Логистика")
   ========================================================= */

const NEWS_CELL_LIMIT = 45000;

// ВАШИ реальные категории, которые вы используете в коде:
// - "Постройки" (в criteria)
// - "Система"   (errors)
// - "Логистика" (отдельная колонка)
const NEWS_ICONS = {
  "Постройки": "🏭",
  "Система":   "⚙️",
  "Логистика": "🚚",
  "Общее":     "ℹ️"
};

// Куда писать по категориям (ТОЛЬКО существующие у вас колонки)
const NEWS_ROUTE = {
  DEFAULT: "Новости",
  BY_CATEGORY: {
    "Логистика": "Новости.Логистика"
    // всё остальное пойдёт в "Новости"
  }
};

const NEWS_BUFFER_KEY = "__NEWS_BUFFER__";

/* =======================
   Buffer + API (same names your project expects)
   ======================= */

function NEWS_ensure_(data) {
  if (!data || typeof data !== "object") throw new Error("data is required");
  if (!data[NEWS_BUFFER_KEY] || typeof data[NEWS_BUFFER_KEY] !== "object") data[NEWS_BUFFER_KEY] = {};
  if (!Array.isArray(data[NEWS_BUFFER_KEY].list)) data[NEWS_BUFFER_KEY].list = [];
}

function initNotifications(data) {
  NEWS_ensure_(data);
  data[NEWS_BUFFER_KEY].list = [];

  // очистим видимые колонки (в пределах их текущей длины)
  NEWS_clearColumn_(data, "Новости");
  NEWS_clearColumn_(data, "Новости.Логистика");
}

function pushNotice(data, n) {
  NEWS_ensure_(data);
  data[NEWS_BUFFER_KEY].list.push(NEWS_normalize_(n));
}

/* =======================
   Normalize / dedup / sort
   ======================= */

function NEWS_normalize_(n) {
  if (typeof n === "string") {
    return { category: "Общее", sub: "", priority: 100, parts: [{ text: n }] };
  }
  const parts = Array.isArray(n.parts)
    ? n.parts
    : (n.text != null ? [{ text: String(n.text) }] : [{ text: "" }]);

  return {
    category: n.category || "Общее",
    sub: n.sub || "",
    priority: (n.priority ?? 100),
    parts
  };
}

function NEWS_key_(n) {
  return JSON.stringify({
    category: n.category || "Общее",
    sub: n.sub || "",
    parts: (n.parts || []).map(p => (p && p.text) ? String(p.text) : "")
  });
}

function NEWS_collapse_(list) {
  const map = new Map();
  list.forEach(n => {
    const k = NEWS_key_(n);
    if (!map.has(k)) map.set(k, { ...n, count: 1 });
    else {
      const s = map.get(k);
      s.count++;
      s.priority = Math.min(s.priority, n.priority);
    }
  });
  return Array.from(map.values());
}

/* =======================
   Render (plain string)
   ======================= */

function NEWS_icon_(category) {
  return NEWS_ICONS[category] || NEWS_ICONS["Общее"] || "✎";
}

function NEWS_render_(n) {
  const category = n.category || "Общее";
  const sub = n.sub || "";
  const icon = NEWS_icon_(category);
  const mult = (n.count || 1) > 1 ? `x${n.count} ` : "";
  const body = (n.parts || []).map(p => (p && p.text) ? String(p.text) : "").join("");

  let s = `${icon} ${category}\n`;
  if (sub && sub.trim()) s += `⌬ ${sub}\n`;
  s += `${mult}${icon} ${body}`;

  if (s.length > NEWS_CELL_LIMIT) s = s.slice(0, NEWS_CELL_LIMIT - 1) + "…";
  return s;
}

/* =======================
   Output helpers
   ======================= */

function NEWS_routeField_(category) {
  return NEWS_ROUTE.BY_CATEGORY[category] || NEWS_ROUTE.DEFAULT;
}

function NEWS_clearColumn_(data, field) {
  if (!Array.isArray(data[field])) return; // если колонки нет в headers — не трогаем
  for (let i = 0; i < data[field].length; i++) data[field][i] = "";
}

function NEWS_writeColumnNoResize_(data, field, lines) {
  // пишем только в существующую колонку (есть в headers -> есть массив)
  if (!Array.isArray(data[field])) return;

  const cap = data[field].length; // емкость = число строк в диапазоне
  const written = Math.min(lines.length, cap);

  for (let i = 0; i < written; i++) data[field][i] = lines[i];

  // если не поместилось — в последнюю строку этой же колонки (без добавления строк)
  const cut = Math.max(0, lines.length - written);
  if (cut > 0 && cap > 0) {
    const msg = `⛔ Новости обрезаны\nНе поместилось: ${cut}\nЗаписано: ${written} из ${lines.length}`;
    data[field][cap - 1] = msg;
  }
}

/* =======================
   FLUSH (same name your runGame calls)
   ======================= */

function flushNotifications(data) {
  NEWS_ensure_(data);

  const raw = data[NEWS_BUFFER_KEY].list || [];
  if (!raw.length) {
    // ничего не пишем, колонки уже очищены в initNotifications
    return;
  }

  // collapse + sort
  const collapsed = NEWS_collapse_(raw);
  collapsed.sort((a, b) =>
    (a.category || "Общее").localeCompare(b.category || "Общее") ||
    (a.sub || "").localeCompare(b.sub || "") ||
    a.priority - b.priority ||
    (b.count || 1) - (a.count || 1)
  );

  // group by output column
  const byField = new Map();
  collapsed.forEach(n => {
    const field = NEWS_routeField_(n.category || "Общее");
    if (!byField.has(field)) byField.set(field, []);
    byField.get(field).push(NEWS_render_(n));
  });

  // write (no resize)
  byField.forEach((lines, field) => {
    NEWS_writeColumnNoResize_(data, field, lines);
  });

  // clear buffer
  data[NEWS_BUFFER_KEY].list = [];
}

/* =========================================================
   QUICK TEST (optional): call once inside any logicFunction
   ========================================================= */
// function NEWS_test_(data) {
//   pushNotice(data, { category: "Постройки", sub: "Test", priority: 1, parts: [{ text: "Новости работают" }] });
//   pushNotice(data, { category: "Логистика", sub: "Test", priority: 1, parts: [{ text: "Логистика отдельно" }] });
//   return data;
// }