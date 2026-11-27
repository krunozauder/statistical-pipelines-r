# 03. CHARTS, TABLES AND EXCEL FILES ----
# Chart and table functions used by the report (report/report.Rmd), and one Excel file per lender
# with its indicators and the loans that need attention in the report quarter.

if (!exists("HELPERS_LOADED")) source(file.path(MODULE, "R", "helpers.R"))
if (!exists("RES")) RES <- readRDS(file.path(OUT, "indicators.rds"))
suppressMessages({library(ggplot2); library(openxlsx)})

BLUE <- "#1F3864"; LIGHT <- "#9DB4C0"; RED <- "#C00000"
COL_CAT <- c(housing = BLUE, consumer = "#C08A2B")
th <- theme_minimal(base_size = 8.5) + theme(legend.position = "bottom", panel.grid.minor = element_blank(),
                                             strip.text = element_text(face = "bold"), legend.title = element_blank())
cat_lab <- function(x) unname(unlist(LIM$categories)[x])

g_overview <- function(q = RES$report_quarter) {
  d <- RES$ind %>% filter(quarter == q, lender != SYSTEM)
  a <- d %>% transmute(lender, column = paste0(cat_lab(category), "\nallowance"), value = utilisation, breach = breach_allowance,
                       label = fmt_pct(utilisation, 0))
  m <- d %>% group_by(lender) %>% summarise(n = sum(above_maturity), .groups = "drop") %>%
    transmute(lender, column = "Maximum\nmaturity", value = ifelse(n > 0, 1.5, 0), breach = n > 0, label = ifelse(n > 0, paste(n, "loans"), "OK"))
  x <- bind_rows(a, m) %>% mutate(lender = factor(lender, levels = rev(RES$lenders)))
  ggplot(x, aes(column, lender)) +
    geom_tile(aes(fill = pmin(value, 1.5)), colour = "white", linewidth = 1, width = 0.96, height = 0.92) +
    geom_tile(data = filter(x, breach), fill = NA, colour = RED, linewidth = 0.9, width = 0.9, height = 0.8) +
    geom_text(aes(label = label), size = 2.6) +
    scale_fill_gradient2(low = "#EAF1F8", mid = "#9DB4C0", high = "#E8A0A0", midpoint = 0.75, limits = c(0, 1.5), guide = "none") +
    scale_x_discrete(position = "top") + labs(x = NULL, y = NULL) + th + theme(panel.grid = element_blank())
}

g_above_below <- function(l = SYSTEM) {
  d <- RES$ind %>% filter(lender == l) %>%
    transmute(quarter, category = cat_lab(category), `Above the limits` = volume_above / 1e6, `Within the limits` = (volume - volume_above) / 1e6) %>%
    pivot_longer(c(`Above the limits`, `Within the limits`))
  ggplot(d, aes(quarter, value, fill = name)) + geom_col(width = 0.7) + facet_wrap(~category, scales = "free_y") +
    scale_fill_manual(values = c(`Above the limits` = RED, `Within the limits` = LIGHT)) +
    labs(x = NULL, y = "EUR million") + th + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

g_utilisation <- function(l = SYSTEM) {
  d <- RES$ind %>% filter(lender == l) %>% mutate(category = cat_lab(category))
  ggplot(d, aes(quarter, group = category)) +
    geom_line(aes(y = allowance, colour = category), linetype = "dashed") +
    geom_line(aes(y = share_above, colour = category), linewidth = 0.8) + geom_point(aes(y = share_above, colour = category), size = 1.5) +
    scale_colour_manual(values = setNames(COL_CAT, cat_lab(names(COL_CAT)))) +
    scale_y_continuous(labels = scales::percent) + labs(x = NULL, y = "Share of new lending") + th +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

g_distribution <- function(l = SYSTEM, q = RES$report_quarter) {
  d <- RES$dist %>% filter(quarter == q, lender %in% unique(c(l, SYSTEM))) %>%
    mutate(panel = paste(cat_lab(category), ratio, sep = ": "), who = ifelse(lender == SYSTEM, SYSTEM, l))
  ggplot(d, aes(class, share, fill = who)) + geom_col(position = position_dodge(preserve = "single"), width = 0.75) +
    facet_wrap(~panel, scales = "free_x", nrow = 1) + scale_fill_manual(values = setNames(c(BLUE, LIGHT), c(l, SYSTEM))[unique(c(l, SYSTEM))]) +
    scale_y_continuous(labels = scales::percent) + labs(x = NULL, y = "Share of volume") + th +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

g_momentum <- function() {
  d <- RES$ind %>% filter(!is.na(contribution)) %>% mutate(category = cat_lab(category))
  big <- RES$ind %>% filter(lender != SYSTEM, quarter == RES$report_quarter) %>% group_by(lender) %>%
    summarise(v = sum(volume)) %>% arrange(-v) %>% slice(1:5) %>% pull(lender)
  bars <- d %>% filter(lender != SYSTEM) %>% mutate(lender = ifelse(lender %in% big, lender, "Other lenders")) %>%
    group_by(quarter, category, lender) %>% summarise(contribution = sum(contribution), .groups = "drop")
  ggplot() + geom_col(data = bars, aes(quarter, contribution, fill = lender), width = 0.7) +
    geom_line(data = filter(d, lender == SYSTEM), aes(quarter, growth, group = 1), linewidth = 0.8) +
    geom_point(data = filter(d, lender == SYSTEM), aes(quarter, growth), size = 1.5) +
    facet_wrap(~category) + scale_fill_brewer(palette = "Blues", direction = -1) +
    scale_y_continuous(labels = scales::percent) + labs(x = NULL, y = "Annual change") + th
}

ROWS <- c(volume = "New lending (EUR million)", loans = "Number of loans", share_above = "Share above the ratio limits",
          allowance = "Allowance", utilisation = "Utilisation of the allowance", above_maturity = "Loans above the maximum maturity",
          not_computable = "Loans with ratios not computable", avg_dsti = "Average DSTI", avg_ltv = "Average LTV",
          avg_rate = "Average interest rate", growth = "Annual growth of new lending")
indicator_table <- function(l = SYSTEM) {
  RES$ind %>% filter(lender == l) %>% arrange(category, quarter) %>%
    mutate(volume = fmt_num(volume / 1e6, 1), loans = fmt_num(loans), share_above = fmt_pct(share_above), allowance = fmt_pct(allowance, 0),
           utilisation = fmt_pct(utilisation, 0), above_maturity = fmt_num(above_maturity), not_computable = fmt_num(not_computable),
           avg_dsti = fmt_pct(avg_dsti), avg_ltv = fmt_pct(avg_ltv), avg_rate = fmt_pct(avg_rate, 2), growth = fmt_pct(growth)) %>%
    select(category, quarter, all_of(names(ROWS))) %>%
    pivot_longer(-c(category, quarter), names_to = "key") %>%
    pivot_wider(names_from = quarter) %>%
    mutate(Indicator = ROWS[key], category = cat_lab(category)) %>%
    filter(!(key == "avg_ltv" & category == cat_lab("consumer"))) %>%
    select(category, Indicator, everything(), -key)
}

write_lender_files <- function() {
  dir.create(file.path(OUT, "lenders"), showWarnings = FALSE)
  head <- createStyle(textDecoration = "bold", fgFill = "#DCE6F1", border = "Bottom")
  for (l in RES$lenders) {
    wb <- createWorkbook()
    addWorksheet(wb, "Indicators"); writeData(wb, "Indicators", indicator_table(l), headerStyle = head)
    addWorksheet(wb, "Loans to check")
    writeData(wb, "Loans to check", RES$issues %>% filter(lender == l), headerStyle = head)
    for (s in c("Indicators", "Loans to check")) { freezePane(wb, s, firstRow = TRUE); setColWidths(wb, s, 1:12, 16) }
    saveWorkbook(wb, file.path(OUT, "lenders", paste0(RES$report_quarter, "_", l, ".xlsx")), overwrite = TRUE)
  }
  length(RES$lenders)
}
