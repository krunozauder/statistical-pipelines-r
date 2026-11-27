# Distribution statistics from household microdata

Weighted distribution measures from a household panel, with all outputs of a statistical release
produced from one calculation: Excel tables, charts, a draft text, a checks workbook and an interactive
HTML report.

The data are synthetic: a fictional panel of 2,500 households in two waves (2024 and 2025), with
household weights, region, composition and disposable income; about 88% of households are observed in
both waves. The methods are the ones I use for household surveys (HBS, EU-SILC, HFCS) and for
distributions of granular financial data; the code is written from scratch for this repository.

## What is calculated

- equivalised income (OECD-modified scale) and weighted quantiles (Hyndman–Fan type 7 generalised to
  weights; equal to `quantile(type = 7)` when weights are equal)
- median, mean, Gini coefficient, Lorenz curve, income shares of the bottom 50% and the top 10%, 5% and 1%
- at-risk-of-poverty threshold (60% of the median) and rate
- households and income by income class, income deciles, regions
- movement between income deciles for households observed in both waves (transition matrix)

## Steps

| Script | What it does |
|---|---|
| `R/01_read.R` | Reads the waves; stops if a file or column is missing, a household appears twice or a weight is not positive |
| `R/02_measures.R` | Computes everything once and saves `measures.rds`; all later steps read only this file |
| `R/03_tables_charts.R` | Excel workbook with all tables and waves; Lorenz curve, decile shares and transition charts (PNG) |
| `R/04_text.R` | Draft text from `config/texts.yml`, with the numbers filled in from the same results |
| `R/05_checks.R` | Comparison with previously published figures (`data/input/published.csv`, after rounding as published) and internal consistency of the tables; each line marked OK or DIFFERENCE |
| `R/06_html.R` | One self-contained HTML file that works offline: choice of wave and comparison wave, number format, a "Copy for Excel" button on every table |

`R/functions.R` holds the measures; `R/00_make_data.R` generates the synthetic data. The published
figures file contains one deliberate difference (a household count published before a revision of the
weights), to show how the checks report it.

## Running

R 4.x with dplyr, tidyr, yaml, openxlsx, ggplot2, jsonlite and rmarkdown (pandoc comes with RStudio).
From this folder:

```
Rscript run.R
Rscript -e 'testthat::test_file("test/test_distribution.R")'
```

Output goes to `output/`: `Distribution_<wave>.xlsx`, `charts/`, `Text_<wave>.docx`, `Checks_<wave>.xlsx`
and `Report_<wave>.html`.
