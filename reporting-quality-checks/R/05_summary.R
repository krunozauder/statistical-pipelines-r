# 05. SUMMARY ----
# All units together: failing records and flagged units by rule and period (Excel) and a chart of
# the share of failing records by rule. No unit names in this output, so it can be shared.

if (!exists("HELPERS_LOADED")) source(file.path(MODULE, "R", "helpers.R"))
if (!exists("CHECKS")) CHECKS <- readRDS(file.path(OUT, "checks.rds"))
suppressMessages({library(openxlsx); library(ggplot2)})
start_step(5, "summary")

by_rule <- CHECKS$units %>%
  group_by(rule, period) %>%
  summarise(records_checked = sum(base), records_failing = sum(n),
            share_failing = records_failing / records_checked,
            units_flagged = sum(flagged), .groups = "drop") %>%
  mutate(issue = sapply(rule, function(x) RULES[[x]]$name), .after = rule)

wide <- by_rule %>%
  select(rule, issue, period, units_flagged) %>%
  pivot_wider(names_from = period, values_from = units_flagged, values_fill = 0)

wb <- createWorkbook()
HEAD <- createStyle(textDecoration = "bold", fgFill = "#DCE6F1", border = "Bottom")
for (sh in list(list("Units flagged", wide), list("By rule and period", by_rule))) {
  addWorksheet(wb, sh[[1]]); writeData(wb, sh[[1]], sh[[2]], headerStyle = HEAD)
  freezePane(wb, sh[[1]], firstRow = TRUE); setColWidths(wb, sh[[1]], cols = 1:ncol(sh[[2]]), widths = "auto")
}
saveWorkbook(wb, file.path(OUT, paste0("summary_", REPORT, ".xlsx")), overwrite = TRUE)

g <- ggplot(by_rule, aes(period, share_failing, group = rule)) +
  geom_line(colour = "grey60") + geom_point(aes(colour = units_flagged > 0), size = 2) +
  facet_wrap(~ paste(rule, issue), ncol = 2, scales = "free_y", labeller = label_wrap_gen(40)) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = c(`FALSE` = "grey40", `TRUE` = "#C00000"), labels = c("no unit flagged", "unit(s) flagged"), name = NULL) +
  labs(x = NULL, y = "Share of failing records", title = "Failing records by rule, all units") +
  theme_bw(base_size = 9) + theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(OUT, paste0("summary_", REPORT, ".png")), g, width = 18, height = 22, units = "cm", dpi = 150)

cat("Summary ->", file.path(OUT, paste0("summary_", REPORT, ".xlsx")), "\n")
