# 01. READ ----
# Register, monthly returns and unit totals for all periods in settings.yml.
# Stops if a file is missing, if a column is missing or not numeric, or if a unit is not in the register.

if (!exists("HELPERS_LOADED")) source(file.path(MODULE, "R", "helpers.R"))
start_step(1, "read")

COLS_NUM <- c("turnover", "exports", "employees", "hours_worked", "gross_wages")
read_one <- function(f) {
  stop_if(!file.exists(f), "Missing input file: ", f)
  read.csv(f, stringsAsFactors = FALSE, na.strings = "")
}

REG <- read_one(file.path(IN, "register.csv"))
REC <- bind_rows(lapply(PERIODS, function(p) read_one(file.path(IN, paste0("returns_", p, ".csv")))))
TOT <- bind_rows(lapply(PERIODS, function(p) read_one(file.path(IN, paste0("unit_totals_", p, ".csv")))))

miss <- setdiff(c("period", "unit_id", "establishment_id", "nace", COLS_NUM), names(REC))
stop_if(length(miss) > 0, "Columns missing in returns: ", paste(miss, collapse = ", "))
not_num <- COLS_NUM[!sapply(REC[COLS_NUM], is.numeric)]
stop_if(length(not_num) > 0, "Columns not numeric in returns: ", paste(not_num, collapse = ", "))
unknown <- setdiff(REC$unit_id, REG$unit_id)
stop_if(length(unknown) > 0, "Units not in the register: ", paste(unknown, collapse = ", "))
stop_if(!REPORT %in% PERIODS, "report_period ", REPORT, " is not among the periods")

# kept for the reconciliation in step 02
IN_CONTROL <- REC %>% group_by(period) %>% summarise(n = n(), turnover = sum(turnover, na.rm = TRUE), .groups = "drop")

cat("Read", nrow(REC), "records,", n_distinct(REC$unit_id), "units,", length(PERIODS), "periods\n")
