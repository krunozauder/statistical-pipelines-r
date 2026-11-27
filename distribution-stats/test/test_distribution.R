# TESTS ----
# From the module folder:  Rscript -e 'testthat::test_file("test/test_distribution.R")'

library(testthat)
MOD <- normalizePath(if (file.exists("config/texts.yml")) "." else "..")

run_pipeline <- function(input, output) {
  suppressWarnings(rm(list = c("HELPERS_LOADED", "MEAS", "IN", "OUT"), envir = globalenv()))
  assign("MODULE", MOD, globalenv()); assign("IN", input, globalenv()); assign("OUT", output, globalenv())
  for (s in c("helpers", "01_read", "02_measures", "03_tables_charts", "04_text", "05_checks", "06_html"))
    capture.output(sys.source(file.path(MOD, "R", paste0(s, ".R")), envir = globalenv()))
}

TMP <- file.path(tempdir(), "dist_test"); unlink(TMP, recursive = TRUE)
owd <- setwd(MOD); system2("Rscript", c("R/00_make_data.R", file.path(TMP, "input")), stdout = FALSE); setwd(owd)
run_pipeline(file.path(TMP, "input"), file.path(TMP, "output"))

test_that("weighted measures agree with the unweighted ones for equal weights", {
  set.seed(1); x <- rlnorm(301); w <- rep(2, 301)
  expect_equal(wquantile(x, w, c(0.1, 0.5, 0.9)), unname(quantile(x, c(0.1, 0.5, 0.9), type = 7)))
  expect_equal(gini(rep(100, 50), rep(1, 50)), 0)
  expect_equal(lorenz_at(lorenz(rep(1, 10), rep(1, 10)), 0.3), 0.3)
})

test_that("tables are internally consistent", {
  expect_true(all(CHECK_RESULTS$consistency$status == "OK"))
})

test_that("the comparison with published figures finds only the revised household count", {
  d <- CHECK_RESULTS$published
  expect_equal(d$indicator[d$status != "OK"], "households")
})

test_that("text, tables and HTML use the same numbers", {
  med <- MEAS$res[[REPORT]]$summary$median
  txt <- paste(readLines(file.path(TMP, "output", paste0("Text_", REPORT, ".md"))), collapse = " ")
  expect_true(grepl(fmt_eur(med), txt, fixed = TRUE))
  html <- paste(readLines(file.path(TMP, "output", paste0("Report_", REPORT, ".html")), warn = FALSE), collapse = "\n")
  D <- jsonlite::fromJSON(regmatches(html, regexpr("(?s)\\{\"title\".*?\\}(?=;\\n)", html, perl = TRUE)))
  expect_equal(D$res[[REPORT]]$summary$median, med, tolerance = 1e-6)
})

test_that("the run stops on duplicate households", {
  bad <- file.path(TMP, "bad"); dir.create(bad)
  file.copy(list.files(file.path(TMP, "input"), full.names = TRUE), bad)
  f <- file.path(bad, paste0("households_", WAVES[1], ".csv")); d <- read.csv(f)
  write.csv(rbind(d, d[1, ]), f, row.names = FALSE)
  expect_error(run_pipeline(bad, file.path(TMP, "out_bad")), "reported twice")
})
