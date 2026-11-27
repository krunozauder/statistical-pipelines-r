# TESTS ----
# Fresh synthetic data in a temporary folder; the indicators must show the planted behaviour, add up,
# follow the limits valid in each quarter, and the report and Excel files must be produced.
# From the module folder:  Rscript -e 'testthat::test_file("test/test_report.R")'

library(testthat)
MOD <- normalizePath(if (file.exists("config/limits.yml")) "." else "..")
TMP <- file.path(tempdir(), "llr_test"); unlink(TMP, recursive = TRUE); dir.create(TMP)
owd <- setwd(MOD); system2("Rscript", c("R/00_make_data.R", file.path(TMP, "loans.csv")), stdout = FALSE); setwd(owd)
run_pipeline <- function(data, out, report = TRUE) {
  suppressWarnings(rm(list = c("HELPERS_LOADED", "RES", "DATA", "OUT"), envir = globalenv()))
  assign("MODULE", MOD, globalenv()); assign("DATA", data, globalenv()); assign("OUT", out, globalenv())
  for (s in c("helpers", "01_prepare", "02_indicators", if (report) "04_report"))
    capture.output(suppressWarnings(sys.source(file.path(MOD, "R", paste0(s, ".R")), envir = globalenv())))
}
run_pipeline(file.path(TMP, "loans.csv"), file.path(TMP, "out"))
q <- RES$report_quarter
b <- RES$ind %>% filter(quarter == q, lender != SYSTEM)

test_that("the planted behaviour is found", {
  expect_true(b$breach_allowance[b$lender == "L03" & b$category == "housing"])
  expect_true(b$breach_maturity[b$lender == "L07" & b$category == "consumer"])
  expect_true(all(b$not_computable[b$lender == "L10"] > 0))
})

test_that("each quarter uses the limits valid then", {
  a <- RES$ind %>% filter(lender == SYSTEM, category == "housing")
  expect_equal(a$allowance[a$quarter == "2025Q4"], 0.15)
  expect_equal(a$allowance[a$quarter == "2026Q1"], 0.10)
})

test_that("lenders add up to all lenders", {
  s <- RES$ind %>% group_by(quarter, category) %>% summarise(d = sum(volume[lender != SYSTEM]) - sum(volume[lender == SYSTEM]), .groups = "drop")
  expect_true(all(abs(s$d) < 1e-6))
})

test_that("the report and one Excel file per lender are written", {
  expect_true(any(file.exists(file.path(TMP, "out", paste0("Report_", q, c(".pdf", ".html"))))))
  expect_equal(length(list.files(file.path(TMP, "out", "lenders"))), length(RES$lenders))
})

test_that("the run stops on duplicate loan ids", {
  d <- read.csv(file.path(TMP, "loans.csv")); write.csv(rbind(d, d[1, ]), file.path(TMP, "bad.csv"), row.names = FALSE)
  expect_error(run_pipeline(file.path(TMP, "bad.csv"), file.path(TMP, "out_bad"), report = FALSE), "Duplicate loan ids")
})
