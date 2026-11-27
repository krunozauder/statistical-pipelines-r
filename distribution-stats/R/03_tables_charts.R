# 03. TABLES AND CHARTS ----
# Excel workbook with all tables (all waves) and the charts as PNG files. Titles and notes are not
# drawn in the charts; they go into the text and the workbook.

if (!exists("HELPERS_LOADED")) source(file.path(MODULE, "R", "helpers.R"))
if (!exists("MEAS")) MEAS <- readRDS(file.path(OUT, "measures.rds"))
suppressMessages({library(openxlsx); library(ggplot2)})
start_step(3, "tables and charts")

NAMES <- c(households = "Households", median = "Median equivalised income (EUR)", mean = "Mean equivalised income (EUR)",
           gini = "Gini coefficient", bottom50 = "Income share of the bottom 50%", top10 = "Income share of the top 10%",
           top5 = "Income share of the top 5%", top1 = "Income share of the top 1%",
           poverty_line = "At-risk-of-poverty threshold (EUR)", poor_share = "At-risk-of-poverty rate")
long <- function(part) bind_rows(lapply(MEAS$res, `[[`, part), .id = "wave")

summ <- long("summary") %>%
  pivot_longer(-wave, names_to = "key") %>%
  pivot_wider(names_from = wave) %>%
  mutate(Indicator = NAMES[key], .before = 1)
tr_name <- names(MEAS$trans)[length(MEAS$trans)]
tr <- MEAS$trans[[tr_name]]
tr_wide <- tr$matrix %>% select(-n) %>% mutate(share = round(share, 4)) %>%
  pivot_wider(names_from = d_to, values_from = share, values_fill = 0, names_prefix = "to D") %>%
  rename(`From decile` = d_from)

TABLES <- list(Summary = summ, Classes = long("classes"), Deciles = long("deciles"),
               Regions = MEAS$res[[REPORT]]$regions %>% mutate(wave = REPORT, .before = 1),
               Transitions = tr_wide, `Transitions summary` = tr$summary %>% mutate(waves = tr_name, .before = 1),
               Lorenz = long("lorenz"))
HEAD <- createStyle(textDecoration = "bold", fgFill = "#DCE6F1", border = "Bottom", wrapText = TRUE)
wb <- createWorkbook()
for (nm in names(TABLES)) {
  addWorksheet(wb, nm)
  writeData(wb, nm, TABLES[[nm]], headerStyle = HEAD)
  freezePane(wb, nm, firstRow = TRUE)
  setColWidths(wb, nm, cols = seq_along(TABLES[[nm]]), widths = c(36, rep(14, ncol(TABLES[[nm]]) - 1)))
}
addWorksheet(wb, "Notes")
writeData(wb, "Notes", c(TXT$notes$definition, TXT$notes$source))
saveWorkbook(wb, file.path(OUT, paste0("Distribution_", REPORT, ".xlsx")), overwrite = TRUE)

# charts ----
DIR_C <- file.path(OUT, "charts"); dir.create(DIR_C, showWarnings = FALSE)
COL <- c("#9DB4C0", "#1F3864")
th  <- theme_minimal(base_size = 9) + theme(legend.position = "bottom", panel.grid.minor = element_blank())
save_chart <- function(g, name) ggsave(file.path(DIR_C, paste0(name, ".png")), g, width = 16, height = 9, units = "cm", dpi = 300)

save_chart(ggplot(long("lorenz"), aes(p, L, colour = wave)) +
             geom_abline(linetype = "dashed", colour = "grey70") + geom_line(linewidth = 0.8) +
             scale_colour_manual(values = COL, name = NULL) +
             scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) +
             labs(x = "Cumulative share of households", y = "Cumulative share of income") + th, "lorenz")

save_chart(ggplot(long("deciles"), aes(factor(decile), share_income, fill = wave)) +
             geom_col(position = "dodge", width = 0.7) +
             scale_fill_manual(values = COL, name = NULL) + scale_y_continuous(labels = scales::percent) +
             labs(x = "Income decile", y = "Share of total income") + th, "deciles")

save_chart(ggplot(tr$matrix, aes(factor(d_to), factor(d_from), fill = share)) +
             geom_tile(colour = "white") + geom_text(aes(label = round(100 * share)), size = 2.4) +
             scale_fill_gradient(low = "#F2F2F2", high = "#1F3864", labels = scales::percent, name = NULL) +
             scale_y_discrete(limits = rev) +
             labs(x = paste("Decile in", sub(".*-", "", tr_name)), y = paste("Decile in", sub("-.*", "", tr_name))) + th, "transitions")

cat("Workbook and", length(list.files(DIR_C)), "charts ->", OUT, "\n")
