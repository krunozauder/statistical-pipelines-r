# RUN ALL STEPS ----
# From the module folder:  Rscript run.R
# or in RStudio, with the module folder as working directory:  source("run.R")
# New synthetic data:  Rscript R/00_make_data.R

MODULE <- getwd()
if (!file.exists(file.path(MODULE, "config", "texts.yml"))) stop("Run from the distribution-stats folder.")
for (s in c("helpers", "01_read", "02_measures", "03_tables_charts", "04_text", "05_checks", "06_html"))
  source(file.path(MODULE, "R", paste0(s, ".R")), encoding = "UTF-8")
