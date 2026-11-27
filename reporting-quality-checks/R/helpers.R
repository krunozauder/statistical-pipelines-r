# HELPERS ----
# Loaded by run.R and by the tests. MODULE (module folder) can be set before sourcing;
# IN and OUT can be set to run on other data (the tests do this).

suppressMessages({library(dplyr); library(tidyr); library(yaml)})
HELPERS_LOADED <- TRUE

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "") || all(is.na(a))) b else a

read_yml <- function(f) yaml.load(paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n"))

MODULE <- if (exists("MODULE")) MODULE else getwd()
S      <- read_yml(file.path(MODULE, "config", "settings.yml"))
RULES  <- read_yml(file.path(MODULE, "config", "rules.yml"))$rules
LET    <- read_yml(file.path(MODULE, "config", "letters.yml"))
IN     <- if (exists("IN"))  IN  else file.path(MODULE, S$paths$input)
OUT    <- if (exists("OUT")) OUT else file.path(MODULE, S$paths$output)
PERIODS  <- unlist(S$periods)
REPORT   <- S$report_period %||% PERIODS[length(PERIODS)]
TOL_UNIT <- S$tolerance$unit_total
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

start_step <- function(k, text) cat(sprintf("\n== %02d %s ==\n", k, text))
stop_if    <- function(cond, ...) if (isTRUE(cond)) stop(paste0(...), call. = FALSE)
fmt_eur    <- function(x) formatC(round(x), format = "d", big.mark = ",")
fmt_pct    <- function(x) paste0(formatC(100 * x, format = "f", digits = 1), "%")
period_date <- function(p) as.Date(paste0(p, "-01"))
period_text <- function(p) paste(month.name[as.integer(substr(p, 6, 7))], substr(p, 1, 4))

# fill {placeholders} in a text
fill <- function(txt, vals) {
  for (k in names(vals)) txt <- gsub(paste0("{", k, "}"), as.character(vals[[k]]), txt, fixed = TRUE)
  txt
}

# evaluate a rule expression within a data frame; NA counts as FALSE
eval_in <- function(expr, d) {
  v <- eval(parse(text = expr), d, globalenv())
  if (length(v) == 1) v <- rep(v, nrow(d))
  v %in% TRUE
}

# version of a rule valid for period p (NULL if none)
rule_version <- function(r, p) {
  if (is.null(r$versions)) return(r)
  d <- period_date(p)
  for (v in r$versions) {
    from <- as.Date(v$valid_from %||% "1900-01-01"); to <- as.Date(v$valid_to %||% "2999-12-31")
    if (d >= from && d <= to) return(modifyList(r[setdiff(names(r), "versions")], v))
  }
  NULL
}
