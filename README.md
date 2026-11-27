# Statistical pipelines in R

Simplified general examples of how I build data processing pipelines for official statistics and financial data: reading
and checking data from many reporting units, feedback to the units, and reproducible outputs.
Each folder is a self-contained project with its own README, configuration, synthetic or public data
and tests.

| Folder | Content |
|---|---|
| [`lending-limits-report`](lending-limits-report) | Quarterly PDF report on borrower-based limits from loan-level data: overview, all lenders, pages and an Excel file for each lender, limits with validity dates |
| [`reporting-quality-checks`](reporting-quality-checks) | Rule-based quality checks of periodic returns, flagging of reporting units, Excel feedback files and letters |
| [`input-output`](input-output) | Published supply and use tables from Excel to SDMX-CSV and SDMX-JSON, symmetric input-output tables (Eurostat models A, B, D) and a tool for the impacts of changes in final demand |
| [`distribution-stats`](distribution-stats) | Weighted distribution measures from household microdata; tables, charts, text, checks and an interactive HTML report from one calculation |

Krunoslav Zauder
