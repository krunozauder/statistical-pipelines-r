# 02. INDICATORS ----
# For every quarter, lender and category, and for all lenders together: volume of new lending, share
# above the ratio limits, utilisation of the allowance, loans above the maximum maturity, loans whose
# ratios cannot be computed, average ratios and interest rate, distributions by DSTI and LTV class and
# annual growth with the contribution of each lender. Saved to OUT/indicators.rds.
# Stops if the lenders do not add up to the system or if volume was lost in processing.

if (!exists("HELPERS_LOADED")) source(file.path(MODULE, "R", "helpers.R"))
start_step(2, "indicators")

L <- LOANS %>% filter(quarter %in% QUARTERS | quarter %in% shift_quarter(QUARTERS, -4))
both <- bind_rows(L, L %>% mutate(lender = SYSTEM))
in_bounds <- function(x, b) !is.na(x) & x >= b[1] & x <= b[2]

IND <- both %>%
  group_by(quarter, lender, category) %>%
  summarise(volume = sum(amount), loans = n(), volume_above = sum(amount[above]),
            not_computable = sum(!computable), above_maturity = sum(above_maturity),
            volume_above_maturity = sum(amount[above_maturity]),
            avg_dsti = weighted.mean(dsti[in_bounds(dsti, LIM$average_bounds$dsti)], amount[in_bounds(dsti, LIM$average_bounds$dsti)]),
            avg_ltv  = if (all(is.na(ltv))) NA_real_ else weighted.mean(ltv[in_bounds(ltv, LIM$average_bounds$ltv)], amount[in_bounds(ltv, LIM$average_bounds$ltv)]),
            avg_rate = weighted.mean(interest_rate, amount),
            allowance = first(allowance), .groups = "drop") %>%
  mutate(share_above = volume_above / volume, utilisation = share_above / allowance,
         breach_allowance = share_above > allowance, breach_maturity = above_maturity > 0)

# growth and contributions (volume compared with the same quarter a year earlier)
prev <- IND %>% transmute(quarter = shift_quarter(quarter, 4), lender, category, volume_prev = volume)
sys_prev <- prev %>% filter(lender == SYSTEM) %>% select(quarter, category, system_prev = volume_prev)
IND <- IND %>% left_join(prev, by = c("quarter", "lender", "category")) %>%
  left_join(sys_prev, by = c("quarter", "category")) %>%
  mutate(growth = volume / volume_prev - 1, contribution = (volume - volume_prev) / system_prev) %>%
  filter(quarter %in% QUARTERS)

# distributions by class ----
cls <- function(x, br) { l <- paste0(fmt_pct(head(br, -1), 0), "–", fmt_pct(tail(br, -1), 0)); l <- c(l, paste0(">", fmt_pct(tail(br, 1), 0)))
  factor(ifelse(is.na(x), "not computable", l[findInterval(x, br, left.open = TRUE, all.inside = FALSE)]), levels = c(l, "not computable")) }
DIST <- bind_rows(
  both %>% filter(quarter %in% QUARTERS) %>% mutate(ratio = "DSTI", class = cls(dsti, LIM$classes$dsti)),
  both %>% filter(quarter %in% QUARTERS, category == "housing") %>% mutate(ratio = "LTV", class = cls(ltv, LIM$classes$ltv))) %>%
  group_by(quarter, lender, category, ratio, class) %>% summarise(volume = sum(amount), .groups = "drop") %>%
  group_by(quarter, lender, category, ratio) %>% mutate(share = volume / sum(volume)) %>% ungroup()

# checks ----
chk <- IND %>% group_by(quarter, category) %>%
  summarise(lenders = sum(volume[lender != SYSTEM]), system = sum(volume[lender == SYSTEM]), .groups = "drop")
stop_if(any(abs(chk$lenders - chk$system) > S$tolerance), "Lenders do not add up to the system")
stop_if(abs(sum(IND$volume[IND$lender == SYSTEM]) - sum(LOANS$amount[LOANS$quarter %in% QUARTERS])) > S$tolerance,
        "Volume lost in processing")

RES <- list(report_quarter = REPORT_Q, quarters = QUARTERS, ind = IND, dist = DIST,
            lenders = sort(setdiff(unique(IND$lender), SYSTEM)),
            limits = limits_on(quarter_start(REPORT_Q)),
            issues = LOANS %>% filter(quarter == REPORT_Q, !computable | above_maturity) %>%
              select(loan_id, lender, category, amount, maturity_months, income_monthly, dsti, ltv, computable, above_maturity))
saveRDS(RES, file.path(OUT, "indicators.rds"))
print(IND %>% filter(quarter == REPORT_Q, breach_allowance | breach_maturity) %>%
        select(lender, category, share_above, allowance, utilisation, above_maturity) %>% as.data.frame())
