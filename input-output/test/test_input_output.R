# TESTS ----
# Run on the published tables in data/input; output goes to a temporary folder.
# From the module folder:  Rscript -e 'testthat::test_file("test/test_input_output.R")'

library(testthat)
MOD <- normalizePath(if (file.exists("config/layouts.yml")) "." else "..")
run_pipeline <- function(output, scenarios = NULL) {
  suppressWarnings(rm(list = c("HELPERS_LOADED", "SIOT", "IN", "OUT"), envir = globalenv()))
  assign("MODULE", MOD, globalenv()); assign("OUT", output, globalenv())
  assign("IN", if (is.null(scenarios)) file.path(MOD, "data", "input") else scenarios, globalenv())
  for (s in c("helpers", "01_read_tables", "02_checks_sut", "03_sdmx", "04_siot", "05_impacts"))
    capture.output(sys.source(file.path(MOD, "R", paste0(s, ".R")), envir = globalenv()))
}
TMP <- file.path(tempdir(), "io_test"); unlink(TMP, recursive = TRUE)
run_pipeline(file.path(TMP, "out"))

test_that("all tables are read and layout problems are logged", {
  expect_equal(nrow(distinct(LONG, source, table)), 12)
  expect_true(any(READ_LOG$source == "eiz_2013" & READ_LOG$table == "T1612" & grepl("duplicate column", READ_LOG$issue)))
  expect_true(any(READ_LOG$source == "dzs_2010" & READ_LOG$table == "T1610" & grepl("duplicate row", READ_LOG$issue)))
})

test_that("accounting identities hold except one small difference in the CBS tables", {
  bad <- CHECKS_SUT %>% filter(status != "OK")
  expect_equal(paste(bad$source, substr(bad$check, 1, 1)), "dzs_2010 2")
  expect_lt(bad$max_abs_difference, 5)
})

test_that("SDMX-CSV and SDMX-JSON hold the same observations", {
  csv <- read.csv(file.path(TMP, "out", "sdmx", "eiz_2013.csv"))
  js  <- jsonlite::fromJSON(file.path(TMP, "out", "sdmx", "eiz_2013.json"), simplifyVector = FALSE)
  expect_equal(length(js$data$dataSets[[1]]$observations), nrow(csv))
  expect_equal(VALID$detail[VALID$status != "OK"], "P2I")
})

test_that("model B reproduces the published CBS domestic symmetric table", {
  expect_lt(COMPARE$share_of_absolute_difference[COMPARE$model == "B"], 1e-9)
})

test_that("symmetric tables keep total value added; model A gives negative flows", {
  chk <- SIOT_CHECK %>% filter(grepl("^total value added", check))
  expect_true(all(chk$status == "OK"))
  expect_true(any(SIOT_CHECK$model == "A" & grepl("negative", SIOT_CHECK$check) & SIOT_CHECK$value > 0))
})

test_that("impacts are consistent", {
  expect_true(all(SUMMARY$output_type2 > SUMMARY$output_type1))
  expect_true(all(SUMMARY$output_multiplier_type1 > 1))
  expect_true(file.exists(file.path(TMP, "out", "Impacts_eiz_2013.xlsx")))
})

test_that("unknown product codes in the scenarios stop the run", {
  alt <- file.path(TMP, "in_alt"); dir.create(alt)
  file.copy(list.files(file.path(MOD, "data", "input"), full.names = TRUE), alt)
  sc <- openxlsx::read.xlsx(file.path(alt, "scenarios.xlsx"), sheet = "final_demand", sep.names = " ")
  sc$code[1] <- "CPA_XYZ"; sc[1, 3] <- 1000
  openxlsx::write.xlsx(list(final_demand = sc), file.path(alt, "scenarios.xlsx"), overwrite = TRUE)
  expect_error(run_pipeline(file.path(TMP, "out_alt"), alt), "not in the tables")
})
