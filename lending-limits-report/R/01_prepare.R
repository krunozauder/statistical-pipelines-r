# 01. PREPARE ----
# Loan-level ratios and the limits valid in each loan's quarter.
# DSTI = debt service / income (not computable if income is missing or below min_income);
# LTV = amount / property value (housing loans). A loan whose ratio cannot be computed counts as above
# the limit in the allowance, because compliance cannot be shown.
# Stops if a column is missing, a loan id is duplicated or a category is not in limits.yml.

if (!exists("HELPERS_LOADED")) source(file.path(MODULE, "R", "helpers.R"))
start_step(1, "prepare")

stop_if(!file.exists(DATA), "Missing data file: ", DATA, " (Rscript R/00_make_data.R)")
LOANS <- read.csv(DATA, stringsAsFactors = FALSE)
need <- c("loan_id", "lender", "month", "category", "amount", "maturity_months", "interest_rate",
          "income_monthly", "debt_service_monthly", "property_value")
stop_if(length(setdiff(need, names(LOANS))) > 0, "Columns missing: ", paste(setdiff(need, names(LOANS)), collapse = ", "))
stop_if(any(duplicated(LOANS$loan_id)), "Duplicate loan ids")
stop_if(length(setdiff(LOANS$category, CATS)) > 0, "Categories not in limits.yml: ", paste(setdiff(LOANS$category, CATS), collapse = ", "))

LOANS <- LOANS %>%
  mutate(quarter = quarter_of(month),
         dsti = ifelse(!is.na(income_monthly) & income_monthly >= LIM$min_income, debt_service_monthly / income_monthly, NA),
         ltv  = ifelse(category == "housing" & property_value > 0, amount / property_value, NA))
lim <- bind_rows(lapply(unique(LOANS$quarter), function(q) limits_on(quarter_start(q)) %>% mutate(quarter = q)))
LOANS <- LOANS %>%
  left_join(lim, by = c("quarter", "category")) %>%
  mutate(computable = !is.na(dsti) & (is.na(cap_ltv) | !is.na(ltv)),
         above = !computable | dsti > cap_dsti | (!is.na(cap_ltv) & ltv > cap_ltv),
         above_maturity = maturity_months > cap_maturity)

complete_q <- LOANS %>% distinct(quarter, month) %>% count(quarter) %>% filter(n == 3) %>% pull(quarter) %>% sort()
REPORT_Q <- S$report_quarter %||% tail(complete_q, 1)
QUARTERS <- tail(complete_q[complete_q <= REPORT_Q], S$quarters_shown)
cat("Loans:", nrow(LOANS), "| report quarter:", REPORT_Q, "| quarters:", paste(range(QUARTERS), collapse = "–"), "\n")
