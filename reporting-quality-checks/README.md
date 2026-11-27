# Quality checks and feedback for statistical reporting

A pipeline that checks periodic data returns from reporting units, decides which units have to be
contacted, and writes the feedback for them: an Excel file with the failing records and a letter.

The data are synthetic: monthly business survey returns from 40 fictional enterprises with one or more
establishments each, plus unit totals reported separately. The design follows the pipelines I use in
production for granular reporting; this version is written from scratch and contains no real data,
rules or texts.

## Steps

| Script | What it does | Stops if |
|---|---|---|
| `R/01_read.R` | Reads the register, the monthly returns and the unit totals for all periods | a file or column is missing, a column is not numeric, a unit is not in the register |
| `R/02_prepare.R` | Adds what the rules need: duplicates, previous-period turnover, activity code in the register, unit-level sums | records were lost or turnover changed during preparation |
| `R/03_checks.R` | Applies every rule in the version valid for the period; decides which units are flagged | – |
| `R/04_feedback.R` | For the report period: Excel file and letter for every flagged unit, list of letters | – |
| `R/05_summary.R` | All units together: failing records and flagged units by rule and period (Excel, chart) | – |

`R/00_make_data.R` generates the synthetic data and a list of planted errors; `run.R` runs steps 01–05.

## Rules are configuration, not code

Rules live in `config/rules.yml`. Each rule has a base (which records it applies to), a check, and the
conditions under which a unit is flagged:

```yaml
R04:
  name: Implausible hours per employee
  level: record
  applies_to: "employees > 0 & !is.na(hours_worked)"
  check: "hours_worked / employees < 40 | hours_worked / employees > 250"
  threshold: 0.05      # share of failing records in the unit's base ...
  min_records: 2       # ... at least this many records ...
  vs_total: 2          # ... and twice the share for all units together
  letter: "monthly hours worked per employee are below 40 or above 250 in {n} of {base} records ({share})"
```

A single implausible record does not trigger a letter; a pattern does. A rule can change over time
(`versions` with `valid_from` / `valid_to`): in the example, the minimum-wage check uses EUR 900 until
December 2025 and EUR 1,000 from January 2026, and every period is checked against the rule valid then.
Letter wording is in `config/letters.yml` and in the `letter` field of each rule, so the same issue
always produces the same sentence. Periods, tolerances and letter settings are in `config/settings.yml`.

## Output (`output/`)

- `feedback/<period>_<unit>.xlsx` – summary of the issues, failing records, rule definitions
- `letters/<period>_<unit>.docx` – letter to the unit (Markdown if pandoc is not available)
- `<period>_letters_list.xlsx` – list of letters with e-mail addresses and annexes
- `summary_<period>.xlsx` and `.png` – all units together, without unit names
- `checks.rds` – full results for further analysis

## Running

R 4.x with dplyr, tidyr, yaml, openxlsx, ggplot2 and rmarkdown (pandoc comes with RStudio).
From this folder:

```
Rscript run.R
Rscript -e 'testthat::test_file("test/test_pipeline.R")'
```

The tests generate fresh data in a temporary folder and check that exactly the planted errors are
flagged, that isolated errors below the thresholds are not, that the rule version valid for each period
is used, that every flagged unit gets a letter and an Excel file, and that the run stops on broken input.
