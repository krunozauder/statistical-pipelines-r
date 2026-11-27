# RUN ALL STEPS ----
# From the module folder:  Rscript run.R
# or in RStudio, with the module folder as working directory:  source("run.R")
# New synthetic data:  Rscript R/00_make_data.R

MODULE <- getwd()
if (!file.exists(file.path(MODULE, "config", "rules.yml"))) stop("Run from the reporting-quality-checks folder.")
for (s in c("helpers", "01_read", "02_prepare", "03_checks", "04_feedback", "05_summary"))
  source(file.path(MODULE, "R", paste0(s, ".R")), encoding = "UTF-8")
