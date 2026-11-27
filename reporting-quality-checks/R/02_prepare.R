# 02. PREPARE ----
# Columns the rules need: duplicate records, turnover in the previous period, activity code in the
# register, and unit-level sums compared with the unit totals.
# Stops if records were lost or turnover changed during preparation.

if (!exists("HELPERS_LOADED")) source(file.path(MODULE, "R", "helpers.R"))
start_step(2, "prepare")

REC <- REC %>%
  arrange(period, unit_id, establishment_id) %>%
  group_by(period, establishment_id) %>%
  mutate(duplicate = row_number() > 1) %>%
  ungroup()

prev <- REC %>%
  filter(!duplicate) %>%
  transmute(period = PERIODS[match(period, PERIODS) + 1], establishment_id, prev_turnover = turnover) %>%
  filter(!is.na(period))

REC <- REC %>%
  left_join(prev, by = c("period", "establishment_id")) %>%
  left_join(REG %>% select(unit_id, nace_register = nace), by = "unit_id")

UNI <- REC %>%
  filter(!duplicate) %>%
  group_by(period, unit_id) %>%
  summarise(turnover_sum  = sum(turnover, na.rm = TRUE),
            employees_sum = sum(employees, na.rm = TRUE), .groups = "drop") %>%
  left_join(TOT, by = c("period", "unit_id"))

# reconciliation with the input ----
chk <- REC %>%
  group_by(period) %>%
  summarise(n = n(), turnover = sum(turnover, na.rm = TRUE), .groups = "drop") %>%
  full_join(IN_CONTROL, by = "period", suffix = c("", "_in"))
stop_if(any(chk$n != chk$n_in), "Records lost or added in preparation: ",
        paste(chk$period[chk$n != chk$n_in], collapse = ", "))
stop_if(any(abs(chk$turnover - chk$turnover_in) > S$tolerance$reconciliation), "Turnover changed in preparation")

cat("Prepared", nrow(REC), "records and", nrow(UNI), "unit-period totals; reconciliation OK\n")
