# HELPERS ----
# Loaded by run.R and by the tests. MODULE, IN and OUT can be set before sourcing.

suppressMessages({library(dplyr); library(tidyr); library(yaml); library(openxlsx)})
HELPERS_LOADED <- TRUE

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "") || all(is.na(a))) b else a
read_yml <- function(f) yaml.load(paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n"))

MODULE <- if (exists("MODULE")) MODULE else getwd()
S   <- read_yml(file.path(MODULE, "config", "settings.yml"))
LAY <- read_yml(file.path(MODULE, "config", "layouts.yml"))$sources
IN  <- if (exists("IN"))  IN  else file.path(MODULE, S$paths$input)
OUT <- if (exists("OUT")) OUT else file.path(MODULE, S$paths$output)
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

start_step <- function(k, text) cat(sprintf("\n== %02d %s ==\n", k, text))
stop_if    <- function(cond, ...) if (isTRUE(cond)) stop(paste0(...), call. = FALSE)
HEAD <- createStyle(textDecoration = "bold", fgFill = "#DCE6F1", border = "Bottom", wrapText = TRUE)
write_sheets <- function(tables, file) {
  wb <- createWorkbook()
  for (nm in names(tables)) {
    addWorksheet(wb, nm); writeData(wb, nm, tables[[nm]], headerStyle = HEAD)
    freezePane(wb, nm, firstRow = TRUE); setColWidths(wb, nm, cols = seq_along(tables[[nm]]), widths = 14)
  }
  saveWorkbook(wb, file, overwrite = TRUE)
}
