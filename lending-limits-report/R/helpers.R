# HELPERS ----
# Loaded by run.R, the report and the tests. MODULE, DATA and OUT can be set before sourcing.

suppressMessages({library(dplyr); library(tidyr); library(yaml)})
HELPERS_LOADED <- TRUE

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "") || all(is.na(a))) b else a
read_yml <- function(f) yaml.load(paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n"))

MODULE <- if (exists("MODULE")) MODULE else getwd()
S    <- read_yml(file.path(MODULE, "config", "settings.yml"))
LIM  <- read_yml(file.path(MODULE, "config", "limits.yml"))
LIM$classes <- lapply(LIM$classes, unlist); LIM$average_bounds <- lapply(LIM$average_bounds, unlist)
TXT  <- read_yml(file.path(MODULE, "config", "texts.yml"))
DATA <- if (exists("DATA")) DATA else file.path(MODULE, S$paths$data, "loans.csv")
OUT  <- if (exists("OUT"))  OUT  else file.path(MODULE, S$paths$output)
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
CATS <- names(LIM$categories)
SYSTEM <- "All lenders"

start_step <- function(k, text) cat(sprintf("\n== %02d %s ==\n", k, text))
stop_if    <- function(cond, ...) if (isTRUE(cond)) stop(paste0(...), call. = FALSE)
fill <- function(txt, vals) { for (k in names(vals)) txt <- gsub(paste0("{", k, "}"), as.character(vals[[k]]), txt, fixed = TRUE); txt }
fmt_pct <- function(x, d = 1) ifelse(is.na(x), "", paste0(formatC(100 * x, format = "f", digits = d), "%"))
fmt_num <- function(x, d = 0) ifelse(is.na(x), "", formatC(x, format = "f", digits = d, big.mark = ","))

quarter_of   <- function(month) paste0(substr(month, 1, 4), "Q", (as.integer(substr(month, 6, 7)) - 1) %/% 3 + 1)
cap <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))
quarter_text <- function(q) paste0(c("first", "second", "third", "fourth")[as.integer(substr(q, 6, 6))], " quarter of ", substr(q, 1, 4))
quarter_start <- function(q) as.Date(paste0(substr(q, 1, 4), "-", sprintf("%02d", 3 * as.integer(substr(q, 6, 6)) - 2), "-01"))
shift_quarter <- function(q, k) { n <- as.integer(substr(q, 1, 4)) * 4 + as.integer(substr(q, 6, 6)) - 1 + k; paste0(n %/% 4, "Q", n %% 4 + 1) }

# limits valid on a date, one row per category
limits_on <- function(date) {
  for (v in LIM$versions) {
    if (date >= as.Date(v$valid_from) && date <= as.Date(v$valid_to %||% "2999-12-31"))
      return(bind_rows(lapply(CATS, function(k) tibble(category = k, cap_ltv = v[[k]]$ltv %||% NA_real_, cap_dsti = v[[k]]$dsti,
                                                         cap_maturity = 12 * v[[k]]$maturity_years, allowance = v[[k]]$allowance))))
  }
  stop("No limits valid on ", date, call. = FALSE)
}
