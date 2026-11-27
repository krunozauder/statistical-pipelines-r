// Interactive report: everything is computed in R; this script only selects and formats.
const $ = id => document.getElementById(id);
let W = D.report, W2 = D.waves[0], FMT = "en";

function num(x, d = 0) {
  if (x === null || x === undefined || isNaN(x)) return "";
  const s = Number(x).toLocaleString("en-US", { minimumFractionDigits: d, maximumFractionDigits: d });
  return FMT === "en" ? s : s.replace(/,/g, "#").replace(/\./g, ",").replace(/#/g, ".");
}
const pct = (x, d = 1) => num(100 * x, d) + "%";

function table(id, head, rows, total) {
  const all = total ? rows.concat([total]) : rows;
  let h = "<table><tr>" + head.map(c => "<th>" + c + "</th>").join("") + "</tr>";
  all.forEach((r, i) => h += "<tr" + (total && i === all.length - 1 ? " class='total'" : "") + ">" + r.map(c => "<td>" + c + "</td>").join("") + "</tr>");
  $(id).innerHTML = "<button class='copy'>Copy for Excel</button>" + h + "</table>";
  $(id).querySelector("button").onclick = () => {
    const tsv = [head].concat(all).map(r => r.join("\t")).join("\n");
    navigator.clipboard.writeText(tsv).then(() => alertCopied(id), () => {
      const t = document.createElement("textarea"); t.value = tsv; document.body.appendChild(t); t.select();
      document.execCommand("copy"); t.remove(); alertCopied(id);
    });
  };
}
function alertCopied(id) { const b = $(id).querySelector("button"); b.textContent = "Copied"; setTimeout(() => b.textContent = "Copy for Excel", 1500); }

function lorenz() {
  const w = 520, h = 340, m = 40, sx = p => m + p * (w - 2 * m), sy = l => h - m - l * (h - 2 * m);
  const path = pts => pts.map((d, i) => (i ? "L" : "M") + sx(d.p).toFixed(1) + "," + sy(d.L).toFixed(1)).join("");
  let s = `<svg width="${w}" height="${h}" font-size="11"><rect x="${m}" y="${m}" width="${w - 2 * m}" height="${h - 2 * m}" fill="#fff" stroke="#ccc"/>`;
  s += `<line x1="${sx(0)}" y1="${sy(0)}" x2="${sx(1)}" y2="${sy(1)}" stroke="#bbb" stroke-dasharray="4"/>`;
  [0, .25, .5, .75, 1].forEach(t => s += `<text x="${sx(t)}" y="${h - m + 15}" text-anchor="middle">${pct(t, 0)}</text><text x="${m - 5}" y="${sy(t) + 4}" text-anchor="end">${pct(t, 0)}</text>`);
  [[W2, "#9DB4C0"], [W, "#1F3864"]].forEach(([wv, c], i) => {
    s += `<path d="${path(D.res[wv].lorenz)}" fill="none" stroke="${c}" stroke-width="2"/>`;
    s += `<text x="${sx(0.05)}" y="${sy(0.9) + 14 * i}" fill="${c}">${wv}${wv === W ? "" : " (comparison)"}</text>`;
  });
  s += `<text x="${w / 2}" y="${h - 5}" text-anchor="middle">Cumulative share of households</text></svg>`;
  $("lorenz").innerHTML = s;
}

function render() {
  const r = D.res[W], s = r.summary[0];
  $("title").textContent = D.title.replace("{wave}", W);
  $("tiles").innerHTML = [["EUR " + num(s.median), "Median equivalised income"], [num(100 * s.gini, 1), "Gini coefficient"],
    [pct(s.top10), "Income share of the top 10%"], [pct(s.poor_share), "At-risk-of-poverty rate"]]
    .map(t => `<div class="tile"><div class="v">${t[0]}</div><div class="l">${t[1]}</div></div>`).join("");
  table("t_summary", ["Indicator"].concat(D.waves), D.indicators.map(k => [D.names[k]].concat(D.waves.map(wv => {
    const v = D.res[wv].summary[0][k];
    return ["households", "median", "mean", "poverty_line"].includes(k) ? num(v) : k === "gini" ? num(100 * v, 1) : pct(v);
  }))));
  table("t_classes", ["Equivalised income, EUR", "Households", "Share of households", "Share of income"],
    r.classes.map(c => [c.class, num(c.households), pct(c.share_households), pct(c.share_income)]),
    ["Total", num(s.households), pct(1), pct(1)]);
  table("t_deciles", ["Decile", "Upper bound, EUR", "Mean, EUR", "Share of income"],
    r.deciles.map(c => ["D" + c.decile, c.decile === 10 ? "" : num(c.upper_bound), num(c.mean), pct(c.share_income)]),
    ["Total", "", num(s.mean), pct(1)]);
  table("t_regions", ["Region", "Households", "Median, EUR", "Mean, EUR", "At-risk-of-poverty rate"],
    r.regions.map(c => [c.region, num(c.households), num(c.median), num(c.mean), pct(c.poor_share)]));
  const t = D.trans;
  $("h_trans").textContent = "Movement between deciles, " + t.name;
  $("trans_text").textContent = `Households observed in both waves: ${pct(t.summary.stayed, 0)} stayed in the same decile, ${pct(t.summary.moved_up, 0)} moved up and ${pct(t.summary.moved_down, 0)} moved down. Rows: decile in the earlier wave; columns: decile in the later wave.`;
  table("t_trans", ["From \\ to"].concat([...Array(10).keys()].map(i => "D" + (i + 1))),
    t.matrix.map((row, i) => ["D" + (i + 1)].concat(row.map(v => pct(v, 0)))));
  lorenz();
}

function init() {
  D.waves.forEach(wv => { $("wave").add(new Option(wv, wv)); $("wave2").add(new Option(wv, wv)); });
  $("wave").value = W; $("wave2").value = W2;
  $("wave").onchange = e => { W = e.target.value; render(); };
  $("wave2").onchange = e => { W2 = e.target.value; render(); };
  $("fmt").onchange = e => { FMT = e.target.value; render(); };
  $("notes").textContent = D.notes;
  render();
}
init();
