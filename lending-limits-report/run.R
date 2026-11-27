# RUN ALL STEPS ----
# From the module folder:  Rscript run.R
# or in RStudio, with the module folder as working directory:  source("run.R")
# Synthetic data are generated on the first run (R/00_make_data.R).

MODULE <- getwd()
if (!file.exists(file.path(MODULE, "config", "limits.yml"))) stop("Run from the lending-limits-report folder.")
if (!file.exists(file.path(MODULE, "data", "loans.csv"))) system2("Rscript", "R/00_make_data.R")
for (s in c("helpers", "01_prepare", "02_indicators", "04_report"))
  source(file.path(MODULE, "R", paste0(s, ".R")), encoding = "UTF-8")
