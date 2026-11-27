# Input-output tables: from published Excel to SDMX, symmetric tables and impact analysis

Three things in one pipeline, all on published Croatian tables:

1. **Excel to SDMX.** Supply and use tables are read from their published worksheet layouts into one
   long table, checked against the accounting identities and code lists, and written as SDMX-CSV and
   SDMX-JSON.
2. **Supply and use to symmetric tables.** Domestic symmetric input-output tables derived with the
   models of the Eurostat Manual of Supply, Use and Input-Output Tables (2008).
3. **Impact tool.** Changes in final demand by product, entered in Excel, give the effects on output,
   value added, compensation, employment, taxes and imports (type I and type II).

The approach follows the code I wrote when compiling these tables at the Croatian Bureau of
Statistics and for input-output studies of investment in renewable energy; it is rewritten for this
repository.

## Sources

| Source | Tables | File |
|---|---|---|
| Institute of Economics, Zagreb – supply and use tables 2013 | T1500, T1600, T1610, T1611, T1612 | [tablice_ponude_uporabe_2013.xlsx](https://www.eizg.hr/UserDocsImages/publikacije/knjige/tablice_ponude_uporabe_2013.xlsx) |
| Croatian Bureau of Statistics – supply and use tables and input-output tables 2010 | T1500 – T1800 | [12-01-04_01_2015.xlsx](https://web.dzs.hr/Hrv_Eng/publication/2015/12-01-04_01_2015.xlsx) |

Copies are in `data/input` so that the code runs as is; `R/00_download.R` downloads them again. The
tables remain the publications of their authors. Values are in thousand HRK.

## Steps

| Script | What it does |
|---|---|
| `R/01_read_tables.R` | Reads every table through `config/layouts.yml` (sheet, title, where the codes are). Layout problems are logged, not fixed silently: columns with values but no code, duplicate row or column codes, recoded codes (`KPD_` → `CPA_`), placeholder rows |
| `R/02_checks_sut.R` | Accounting identities: supply = use by product, output by industry in both tables, domestic + imports = total use, value added = output − intermediate consumption = its components |
| `R/03_sdmx.R` | Validation against `config/dsd.yml` and the code lists (unknown codes, duplicate keys, non-numeric values); SDMX-CSV and SDMX-JSON (all dimensions at observation level) per source; `Checks_tables.xlsx` |
| `R/04_siot.R` | Symmetric tables: model B (industry technology, product × product), model A (product technology, product × product) and model D (fixed product sales structure, industry × industry); balance checks, negative flows, comparison with the published CBS table T1800 |
| `R/05_impacts.R` | Leontief inverse of the model B table; scenarios from `data/input/scenarios.xlsx`; import shares from the use tables; type I and type II (households endogenous) effects and multipliers; `Impacts_<source>.xlsx` |

The dimension names follow the Eurostat input-output datasets (`prod_na` rows, `induse` columns,
`stk_flow` total / domestic / imports). Code lists in `config/codelists` are built from the English
labels of the CBS publication (`R/00_codelists.R`), plus documented additions.

## What the checks find

- All identities hold in both sources, except output of industry T (households as employers) in the CBS
  2010 tables, which differs between the supply and the use table by 2.2 thousand HRK.
- Model B applied to the CBS 2010 supply and domestic use tables reproduces the published domestic
  symmetric table T1800 exactly, which is consistent with the industry technology assumption. Model A gives
  negative flows, as expected for a non-diagonal make matrix.
- The Institute of Economics workbook has columns without codes, a duplicated column (`TU`), a mistyped
  code (`KPA_TOTAL`), placeholder rows (`0`) and a row code (`P2I`) that is not in the CBS code list; all
  are listed in the checks workbook.

## Impact tool

`data/input/scenarios.xlsx`, sheet `final_demand`: one row per product (CPA code), one column per
scenario, changes in final demand in thousand HRK. `R/00_make_scenarios.R` writes the template with
three illustrative scenarios (solar power plants, wind farms, energy renovation of buildings, 100
million HRK each). Each value is split into domestic output and imports with the import shares of the
column set in `settings.yml` (gross fixed capital formation by default).

## Running

R 4.x with dplyr, tidyr, yaml, openxlsx and jsonlite. From this folder:

```
Rscript run.R
Rscript -e 'testthat::test_file("test/test_input_output.R")'
```

Output in `output/`: `sdmx/` (CSV and JSON), `Checks_tables.xlsx`, `Symmetric_tables.xlsx`,
`Impacts_eiz_2013.xlsx`.
