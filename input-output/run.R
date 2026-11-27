# RUN ALL STEPS ----
# From the module folder:  Rscript run.R
# or in RStudio, with the module folder as working directory:  source("run.R")
# The published workbooks are in data/input (R/00_download.R gets them again); the scenario
# template is made by R/00_make_scenarios.R.

MODULE <- getwd()
if (!file.exists(file.path(MODULE, "config", "layouts.yml"))) stop("Run from the input-output folder.")
for (s in c("helpers", "01_read_tables", "02_checks_sut", "03_sdmx", "04_siot", "05_impacts"))
  source(file.path(MODULE, "R", paste0(s, ".R")), encoding = "UTF-8")
