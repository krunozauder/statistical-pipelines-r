# 00. SCENARIO TEMPLATE ----
# Writes data/input/scenarios.xlsx: changes in final demand by product (thousand HRK, purchasers'
# split into domestic and imported in step 05), one column per scenario. The three scenarios are
# illustrative allocations of 100 million HRK each; replace them with your own vectors.
# Usage (from the module folder): Rscript R/00_make_scenarios.R

suppressMessages({library(dplyr); library(openxlsx)})
cl <- read.csv("config/codelists/prod_na.csv", stringsAsFactors = FALSE) %>% filter(grepl("^CPA_", code), code != "CPA_TOTAL")
alloc <- list(
  `Solar power plants`             = c(CPA_C27 = .30, CPA_C26 = .15, CPA_C25 = .10, CPA_C28 = .04, CPA_F = .30, CPA_M71 = .08, CPA_H49 = .03),
  `Wind farms`                     = c(CPA_C28 = .35, CPA_C25 = .20, CPA_C27 = .10, CPA_F = .25, CPA_M71 = .06, CPA_H49 = .04),
  `Energy renovation of buildings` = c(CPA_F = .70, CPA_C23 = .10, CPA_C16 = .05, CPA_C22 = .05, CPA_C27 = .05, CPA_M71 = .05))
d <- tibble(code = cl$code, product = cl$label)
for (s in names(alloc)) d[[s]] <- round(100000 * coalesce(alloc[[s]][d$code], 0))

wb <- createWorkbook()
addWorksheet(wb, "final_demand")
writeData(wb, "final_demand", d, headerStyle = createStyle(textDecoration = "bold", fgFill = "#DCE6F1", wrapText = TRUE))
setColWidths(wb, "final_demand", 1:ncol(d), c(12, 50, rep(16, length(alloc))))
freezePane(wb, "final_demand", firstRow = TRUE, firstCol = TRUE)
addWorksheet(wb, "notes")
writeData(wb, "notes", c("Changes in final demand by product, thousand HRK.",
                         "One column per scenario; the column name is the scenario name. Rows must use the CPA codes of the tables.",
                         "Step 05 splits each value into domestic output and imports with the import shares of the column set in settings.yml.",
                         "The three scenarios are illustrative allocations, not results of any study."))
saveWorkbook(wb, "data/input/scenarios.xlsx", overwrite = TRUE)
cat("Template -> data/input/scenarios.xlsx\n")
