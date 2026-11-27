# Periodic report on borrower-based limits from loan-level data

A pipeline that turns monthly loan-level data reported by lenders into a quarterly PDF report on the
borrower-based limits: an overview page, pages for all lenders together and pages for each lender,
plus an Excel file per lender.

The data are synthetic: about 27,000 new loans of 12 fictional lenders over two years, with
borrower income, debt service, loan amount, maturity, interest rate and, for housing loans, the value of
the property. The limits in `config/limits.yml` are illustrative and change in January 2026. The design
follows the quarterly report that I prepare on the implementation of borrower-based measures; this
version is written from scratch and contains no real data, rules or texts.

## What the report shows

- **Overview:** for each lender, the utilisation of the allowance for each loan category (the share of
  new lending above the LTV or DSTI limit divided by the allowance) and loans above the maximum
  maturity, with a sentence on how many lenders did not comply.
- **All lenders:** new lending above and below the limits, the share above the limits against the
  allowance over time, distributions of new lending by DSTI and LTV class, annual growth with the
  contributions of the largest lenders, and a table of indicators by quarter.
- **Each lender:** the share above the limits against the allowance, the distributions compared with
  all lenders, and the table of indicators.

## Steps

| Script | What it does | Stops if |
|---|---|---|
| `R/01_prepare.R` | Computes DSTI and LTV for each loan and attaches the limits valid in its quarter. Loans whose ratios cannot be computed count as above the limit. | a column is missing, a loan id is duplicated, a category is not in `limits.yml` |
| `R/02_indicators.R` | Indicators by quarter, lender and category, and for all lenders: volume, share above the limits, utilisation of the allowance, loans above the maximum maturity, loans with ratios not computable, average ratios (without implausible values) and interest rate, distributions by class, annual growth and contributions | the lenders do not add up to all lenders, or volume was lost |
| `R/03_charts_tables.R` | Chart and table functions for the report; one Excel file per lender with its indicators and the loans to check | – |
| `R/04_report.R` | Renders `report/report.Rmd` (R Markdown; PDF with XeLaTeX, otherwise HTML) with a child page for every lender | – |

## Configuration

| File | Content |
|---|---|
| `config/limits.yml` | loan categories; LTV and DSTI limits, maximum maturity and allowance by category, in versions with validity dates; bounds for averages; classes for the distributions |
| `config/texts.yml` | title, figure and table captions, notes and source; `{quarter_text}` and similar placeholders are filled in; an empty text is not shown |
| `config/settings.yml` | report quarter (empty = last complete quarter), number of quarters shown, report format and font, tolerance |

Figures and tables are numbered automatically, and each figure has its note and source below it.

## Running

R 4.x with dplyr, tidyr, yaml, ggplot2, openxlsx, rmarkdown, knitr and kableExtra. The PDF report needs
XeLaTeX; without it the report is written as a self-contained HTML file. To get the PDF, install TinyTeX
once in R with `install.packages("tinytex"); tinytex::install_tinytex()`. From this folder:

```
Rscript run.R
Rscript -e 'testthat::test_file("test/test_report.R")'
```

The first run generates the synthetic data (`R/00_make_data.R`). Output in `output/`:
`Report_<quarter>.pdf` (or `.html`) and `lenders/<quarter>_<lender>.xlsx`.

The tests check that the planted behaviour is found (a lender above the housing allowance after the
limits are tightened, consumer loans above the maximum maturity, a lender that often reports no income),
that each quarter uses the limits valid then, that the lenders add up to the total, that the report and
the Excel files are written, and that the run stops on duplicate loan ids.
