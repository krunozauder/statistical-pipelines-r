# 00. SYNTHETIC DATA ----
# Monthly new loans of 12 fictional lenders, July 2024 – June 2026: category, amount, maturity,
# interest rate, borrower income and debt service, and property value for housing loans.
# Some lenders behave in ways the report must show: L03 lends above the housing limits after the
# tightening of 2026, L07 grants consumer loans above the maximum maturity, L10 often reports no income,
# L05 grows fast. All values are random.
# Usage (from the module folder): Rscript R/00_make_data.R [output file]   default: data/loans.csv

suppressMessages(library(dplyr))
args <- commandArgs(TRUE)
OUT <- if (length(args)) args[1] else "data/loans.csv"
dir.create(dirname(OUT), recursive = TRUE, showWarnings = FALSE)
set.seed(907)

LENDERS <- tibble(lender = sprintf("L%02d", 1:12),
                  size = c(9, 7, 6, 5, 4, 3, 3, 2, 2, 1.5, 1, 1),         # relative market size
                  growth = c(0, .02, .01, 0, .25, 0, .03, 0, -.05, 0, 0, .02))
MONTHS <- format(seq(as.Date("2024-07-01"), as.Date("2026-06-01"), by = "month"), "%Y-%m")

loans <- bind_rows(lapply(seq_along(MONTHS), function(t) {
  m <- MONTHS[t]
  bind_rows(lapply(seq_len(nrow(LENDERS)), function(i) {
    L <- LENDERS[i, ]
    n <- rpois(1, 25 * L$size * (1 + L$growth)^(t / 12) * (1 + 0.15 * sin(2 * pi * t / 12)))
    cat <- sample(c("housing", "consumer"), n, replace = TRUE, prob = c(.45, .55))
    income <- round(rlnorm(n, log(1800), 0.45))
    amount <- round(ifelse(cat == "housing", rlnorm(n, log(110000), 0.45), rlnorm(n, log(12000), 0.7)), -2)
    mat <- ifelse(cat == "housing", sample(c(180, 240, 300, 360, 420), n, TRUE, c(.1, .25, .3, .3, .05)),
                  sample(c(24, 36, 60, 72, 84), n, TRUE, c(.15, .25, .35, .15, .10)))
    rate <- ifelse(cat == "housing", 0.035, 0.065) + rnorm(n, 0, 0.006)
    ann <- amount * (rate / 12) / (1 - (1 + rate / 12)^(-mat))
    other <- round(income * pmax(0, rnorm(n, 0.08, 0.06)))
    value <- ifelse(cat == "housing", round(amount / pmin(pmax(rnorm(n, 0.68, 0.12), 0.2), 0.84), -2), NA)
    d <- tibble(lender = L$lender, month = m, category = cat, amount, maturity_months = mat,
                interest_rate = round(rate, 4), income_monthly = income,
                debt_service_monthly = round(ann + other), property_value = value)
    # keep most loans within the limits, as lenders do
    over <- d$debt_service_monthly / d$income_monthly > ifelse(d$category == "housing", 0.40, 0.35)
    keep_over <- runif(n) < ifelse(d$category == "housing", 0.04, 0.015)
    d$income_monthly[over & !keep_over] <- round(d$debt_service_monthly[over & !keep_over] / runif(sum(over & !keep_over), 0.15, 0.34))
    if (L$lender == "L03" && m >= "2026-01") {                         # above the housing limits after tightening
      k <- which(d$category == "housing")[runif(sum(d$category == "housing")) < 0.18]
      d$property_value[k] <- round(d$amount[k] / runif(length(k), 0.88, 0.95), -2)
    }
    if (L$lender == "L07" && m >= "2026-01") {                         # consumer loans above 7 years
      k <- which(d$category == "consumer")[1:2]; d$maturity_months[k[!is.na(k)]] <- 96
    }
    if (L$lender == "L10") d$income_monthly[runif(n) < 0.12] <- NA     # income often missing
    d
  }))
})) %>% mutate(loan_id = sprintf("%s-%06d", lender, row_number()), .before = 1)

write.csv(loans, OUT, row.names = FALSE, na = "")
cat("Synthetic loans:", nrow(loans), "loans,", n_distinct(loans$lender), "lenders,", length(MONTHS), "months ->", OUT, "\n")
