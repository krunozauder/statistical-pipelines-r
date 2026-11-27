# HELPERS ----
# Loaded by run.R and by the tests. MODULE, IN and OUT can be set before sourcing.

suppressMessages({library(dplyr); library(tidyr); library(yaml)})
HELPERS_LOADED <- TRUE

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "") || all(is.na(a))) b else a
read_yml <- function(f) yaml.load(paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n"))

MODULE <- if (exists("MODULE")) MODULE else getwd()
S      <- read_yml(file.path(MODULE, "config", "settings.yml"))
TXT    <- read_yml(file.path(MODULE, "config", "texts.yml"))
IN     <- if (exists("IN"))  IN  else file.path(MODULE, S$paths$input)
OUT    <- if (exists("OUT")) OUT else file.path(MODULE, S$paths$output)
WAVES  <- unlist(S$waves)
REPORT <- S$report_wave %||% WAVES[length(WAVES)]
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
source(file.path(MODULE, "R", "functions.R"))

start_step <- function(k, text) cat(sprintf("\n== %02d %s ==\n", k, text))
stop_if    <- function(cond, ...) if (isTRUE(cond)) stop(paste0(...), call. = FALSE)
fmt_eur    <- function(x) formatC(round(x), format = "d", big.mark = ",")
fmt_pct    <- function(x, d = 1) paste0(formatC(100 * x, format = "f", digits = d), "%")
fill <- function(txt, vals) {
  for (k in names(vals)) txt <- gsub(paste0("{", k, "}"), as.character(vals[[k]]), txt, fixed = TRUE)
  txt
}
