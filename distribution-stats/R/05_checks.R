# 05. CHECKS ----
# Checks_<wave>.xlsx: (1) comparison with previously published figures (data/input/published.csv),
# after rounding the result as it was published; (2) internal consistency of the tables.
# Differences are reported, not hidden: the status column says OK or DIFFERENCE.

if (!exists("HELPERS_LOADED")) source(file.path(MODULE, "R", "helpers.R"))
if (!exists("MEAS")) MEAS <- readRDS(file.path(OUT, "measures.rds"))
suppressMessages(library(openxlsx))
start_step(5, "checks")

# (1) published figures ----
pub_file <- file.path(IN, "published.csv")
PUB <- if (file.exists(pub_file)) read.csv(pub_file, stringsAsFactors = FALSE, colClasses = c(wave = "character")) else NULL
vs_pub <- NULL
if (!is.null(PUB)) {
  calc <- bind_rows(lapply(MEAS$res, `[[`, "summary"), .id = "wave") %>%
    pivot_longer(-wave, names_to = "indicator", values_to = "calculated")
  tol <- c(eur = S$tolerance$eur, pct = S$tolerance$pct, count = S$tolerance$count)
  vs_pub <- PUB %>%
    left_join(calc, by = c("wave", "indicator")) %>%
    mutate(calculated = ifelse(unit == "pct", 100 * calculated, calculated),
           calculated_rounded = round(calculated, rounding),
           difference = calculated_rounded - value,
           status = ifelse(!is.na(difference) & abs(difference) <= tol[unit], "OK", "DIFFERENCE"))
}

# (2) internal consistency ----
cons <- bind_rows(lapply(names(MEAS$res), function(wv) {
  r <- MEAS$res[[wv]]; s <- r$summary
  tibble(wave = wv,
         check = c("Classes: households add up to the total", "Classes: income shares add up to 1",
                   "Deciles: households add up to the total", "Deciles: each holds 10% of households (+/- 1 p.p.)",
                   "Deciles: income shares add up to 1", "Lorenz curve ends at (1, 1)",
                   "Top shares: top 1% < top 5% < top 10%", "Bottom 50% share + share above median = 1 (approx.)"),
         ok = c(abs(sum(r$classes$households) - s$households) < 1e-6 * s$households,
                abs(sum(r$classes$share_income) - 1) < 1e-9,
                abs(sum(r$deciles$households) - s$households) < 1e-6 * s$households,
                all(abs(r$deciles$households / s$households - 0.1) < 0.01),
                abs(sum(r$deciles$share_income) - 1) < 1e-9,
                abs(tail(r$lorenz$p, 1) - 1) < 1e-9 & abs(tail(r$lorenz$L, 1) - 1) < 1e-9,
                s$top1 < s$top5 & s$top5 < s$top10,
                s$bottom50 < 0.5))
}))
cons_tr <- bind_rows(lapply(names(MEAS$trans), function(nm) {
  m <- MEAS$trans[[nm]]
  tibble(wave = nm, check = c("Transitions: each row adds up to 1", "Transitions: stayed + up + down = 1"),
         ok = c(all(abs(tapply(m$matrix$share, m$matrix$d_from, sum) - 1) < 1e-9),
                abs(m$summary$stayed + m$summary$moved_up + m$summary$moved_down - 1) < 1e-9))
}))
CONS <- bind_rows(cons, cons_tr) %>% mutate(status = ifelse(ok, "OK", "DIFFERENCE")) %>% select(-ok)

HEAD <- createStyle(textDecoration = "bold", fgFill = "#DCE6F1", border = "Bottom")
RED  <- createStyle(fontColour = "#C00000", textDecoration = "bold")
wb <- createWorkbook()
for (sh in list(list("Published figures", vs_pub), list("Consistency", CONS))) {
  if (is.null(sh[[2]])) next
  addWorksheet(wb, sh[[1]]); writeData(wb, sh[[1]], sh[[2]], headerStyle = HEAD)
  setColWidths(wb, sh[[1]], cols = seq_along(sh[[2]]), widths = "auto")
  conditionalFormatting(wb, sh[[1]], cols = ncol(sh[[2]]), rows = 2:(nrow(sh[[2]]) + 1), type = "contains", rule = "DIFFERENCE", style = RED)
}
saveWorkbook(wb, file.path(OUT, paste0("Checks_", REPORT, ".xlsx")), overwrite = TRUE)

CHECK_RESULTS <- list(published = vs_pub, consistency = CONS)
cat("Published figures:", if (is.null(vs_pub)) "no file" else paste(sum(vs_pub$status == "OK"), "OK,", sum(vs_pub$status != "OK"), "DIFFERENCE"),
    "| consistency:", sum(CONS$status == "OK"), "OK,", sum(CONS$status != "OK"), "DIFFERENCE\n")
if (!is.null(vs_pub)) print(as.data.frame(vs_pub %>% filter(status != "OK") %>% select(wave, indicator, value, calculated_rounded, difference)))
