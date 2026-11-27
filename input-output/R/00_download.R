# 00. DOWNLOAD ----
# Gets the published workbooks listed in layouts.yml into data/input (copies are already included).
# Usage (from the module folder): Rscript R/00_download.R

library(yaml)
lay <- yaml.load_file("config/layouts.yml")$sources
for (s in names(lay)) {
  f <- file.path("data/input", lay[[s]]$file)
  download.file(lay[[s]]$url, f, mode = "wb")
  cat(s, "->", f, "\n")
}
