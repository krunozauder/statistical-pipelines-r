# 00. SYNTHETIC DATA ----
# Fictional reporting units (enterprises) with one or more establishments, monthly returns for the
# periods in settings.yml, unit totals reported separately, and a list of planted errors that the
# checks must find (planted_errors.csv). All values are random; nothing comes from real data.
#
# Usage (from the module folder): Rscript R/00_make_data.R [output folder]   default: data/input

suppressMessages({library(dplyr); library(tidyr); library(yaml)})

args <- commandArgs(TRUE)
S <- read_yaml("config/settings.yml")
OUT <- if (length(args)) args[1] else S$paths$input
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
set.seed(2711)

PERIODS <- S$periods
N_UNITS <- 40
NACE    <- c("C", "F", "G", "H", "I", "J", "M")
EXPORT_SHARE <- c(C = 0.45, F = 0.05, G = 0.15, H = 0.30, I = 0.20, J = 0.35, M = 0.25)

# register ----
register <- tibble(unit_id   = sprintf("U%03d", 1:N_UNITS),
                   unit_name = paste("Enterprise", sprintf("%03d", 1:N_UNITS)),
                   nace      = sample(NACE, N_UNITS, replace = TRUE),
                   email     = paste0("returns@enterprise", sprintf("%03d", 1:N_UNITS), ".example"))

# establishments ----
n_est <- sample(1:6, N_UNITS, replace = TRUE)
n_est[register$unit_id %in% c("U012", "U015", "U018", "U024")] <- 5      # units with planted errors that need several records
est <- register %>%
  select(unit_id, nace) %>%
  mutate(n = n_est) %>%
  uncount(n, .id = "k") %>%
  mutate(establishment_id = paste0(unit_id, "-", sprintf("%02d", k)),
         base_turnover    = rlnorm(n(), meanlog = 12, sdlog = 0.8),
         employees        = pmax(1, round(base_turnover / runif(n(), 8000, 16000))),
         wage_level       = runif(n(), 1300, 2800),
         export_share     = EXPORT_SHARE[nace] * runif(n(), 0.5, 1.3))

# monthly returns ----
ret <- est %>%
  crossing(period = PERIODS) %>%
  mutate(t            = match(period, PERIODS),
         turnover     = round(base_turnover * (1 + 0.03 * sin(2 * pi * t / 12)) * exp(rnorm(n(), 0, 0.04))),
         exports      = round(turnover * export_share),
         hours_worked = round(employees * runif(n(), 150, 180)),
         gross_wages  = round(employees * wage_level * exp(rnorm(n(), 0, 0.02)))) %>%
  select(period, unit_id, establishment_id, nace, turnover, exports, employees, hours_worked, gross_wages) %>%
  arrange(period, unit_id, establishment_id)

# planted errors ----
P  <- PERIODS[length(PERIODS)]                      # most errors go into the last period
ix <- function(u, p, k = 1) which(ret$unit_id == u & ret$period == p)[k]

ret$turnover[ix("U005", P)]          <- NA
ret$exports[ix("U008", P)]           <- -abs(ret$exports[ix("U008", P)]) - 100
ret$exports[ix("U012", P, 1:2)]      <- round(ret$turnover[ix("U012", P, 1:2)] * 1.5)
ret$hours_worked[ix("U015", P, 1:2)] <- ret$employees[ix("U015", P, 1:2)] * 400
ret$gross_wages[ix("U018", P, 1:2)]  <- ret$employees[ix("U018", P, 1:2)] * 950
ret$gross_wages[ix("U018", PERIODS[3], 1:2)] <- ret$employees[ix("U018", PERIODS[3], 1:2)] * 950   # allowed before 2026: not an error
ret$gross_wages[ix("U021", P)]       <- 0
ret$turnover[ix("U024", P, 1:2)]     <- ret$turnover[ix("U024", P, 1:2)] * 6
ret$nace[ix("U030", P)]              <- setdiff(NACE, ret$nace[ix("U030", P)])[1]
ret <- bind_rows(ret, ret[ix("U033", P), ]) %>% arrange(period, unit_id, establishment_id)

# isolated errors below the thresholds: they fail a rule, but must not flag the unit
ret$hours_worked[ix("U002", PERIODS[2])] <- ret$employees[ix("U002", PERIODS[2])] * 30
ret$turnover[ix("U003", PERIODS[4])]     <- ret$turnover[ix("U003", PERIODS[4])] * 5

# unit totals, reported separately ----
totals <- ret %>%
  distinct(period, unit_id, establishment_id, .keep_all = TRUE) %>%
  group_by(period, unit_id) %>%
  summarise(turnover_total  = sum(turnover, na.rm = TRUE),
            employees_total = sum(employees, na.rm = TRUE), .groups = "drop")
totals$turnover_total[totals$unit_id == "U027" & totals$period == P] <-
  totals$turnover_total[totals$unit_id == "U027" & totals$period == P] + 50000

planted <- tribble(~unit_id, ~rule,
                   "U005", "R01", "U008", "R02", "U012", "R03", "U015", "R04", "U018", "R05",
                   "U021", "R06", "U024", "R07", "U027", "R08", "U030", "R09", "U033", "R10") %>%
  mutate(period = P, .before = 1)

# save ----
write.csv(register, file.path(OUT, "register.csv"), row.names = FALSE)
for (p in PERIODS) {
  write.csv(filter(ret, period == p), file.path(OUT, paste0("returns_", p, ".csv")), row.names = FALSE, na = "")
  write.csv(filter(totals, period == p), file.path(OUT, paste0("unit_totals_", p, ".csv")), row.names = FALSE)
}
write.csv(planted, file.path(OUT, "planted_errors.csv"), row.names = FALSE)
cat("Synthetic data:", N_UNITS, "units,", nrow(est), "establishments,", length(PERIODS), "periods ->", OUT, "\n")
