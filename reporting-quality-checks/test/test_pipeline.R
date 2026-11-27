# TESTS ----
# Fresh synthetic data in a temporary folder; the checks must find exactly the planted errors,
# apply the rule version valid for each period, write a letter and an Excel file for every flagged
# unit, and stop on broken input.
# From the module folder:  Rscript -e 'testthat::test_file("test/test_pipeline.R")'

library(testthat)
MOD <- normalizePath(if (file.exists("config/rules.yml")) "." else "..")

run_pipeline <- function(input, output) {
  suppressWarnings(rm(list = c("HELPERS_LOADED", "CHECKS", "IN", "OUT"), envir = globalenv()))
  assign("MODULE", MOD, globalenv()); assign("IN", input, globalenv()); assign("OUT", output, globalenv())
  for (s in c("helpers", "01_read", "02_prepare", "03_checks", "04_feedback", "05_summary"))
    capture.output(sys.source(file.path(MOD, "R", paste0(s, ".R")), envir = globalenv()))
  get("CHECKS", globalenv())
}

TMP <- file.path(tempdir(), "rqc_test"); unlink(TMP, recursive = TRUE)
owd <- setwd(MOD)
system2("Rscript", c("R/00_make_data.R", file.path(TMP, "input")), stdout = FALSE)
setwd(owd)
CH <- run_pipeline(file.path(TMP, "input"), file.path(TMP, "output"))
planted <- read.csv(file.path(TMP, "input", "planted_errors.csv"), stringsAsFactors = FALSE)

test_that("exactly the planted errors are flagged", {
  found <- CH$units[CH$units$flagged, c("period", "unit_id", "rule")]
  key <- function(d) sort(paste(d$period, d$unit_id, d$rule))
  expect_equal(key(found), key(planted))
})

test_that("isolated failing records below the thresholds do not flag a unit", {
  iso <- CH$units[CH$units$unit_id %in% c("U002", "U003") & CH$units$rule %in% c("R04", "R07"), ]
  expect_true(sum(iso$n) > 0)
  expect_false(any(iso$flagged))
})

test_that("the rule version valid for the period is applied", {
  r05 <- CH$units[CH$units$rule == "R05" & CH$units$unit_id == "U018", ]
  expect_false(any(r05$flagged[r05$period == "2025-12"]))    # 950 per employee: allowed in 2025
  expect_true(r05$flagged[r05$period == "2026-03"])          # below 1,000 from 2026
})

test_that("every flagged unit gets a letter and an Excel file", {
  u <- unique(planted$unit_id)
  expect_true(all(file.exists(file.path(TMP, "output", "feedback", paste0("2026-03_", u, ".xlsx")))))
  expect_equal(length(list.files(file.path(TMP, "output", "letters"))), length(u))
})

test_that("the run stops on broken input", {
  bad <- file.path(TMP, "bad"); dir.create(bad)
  file.copy(list.files(file.path(TMP, "input"), full.names = TRUE), bad)
  reg <- read.csv(file.path(bad, "register.csv")); write.csv(reg[-1, ], file.path(bad, "register.csv"), row.names = FALSE)
  expect_error(run_pipeline(bad, file.path(TMP, "out_bad")), "not in the register")
  file.remove(file.path(bad, "returns_2026-01.csv"))
  expect_error(run_pipeline(bad, file.path(TMP, "out_bad")), "Missing input file")
})
