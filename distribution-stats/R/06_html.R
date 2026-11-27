# 06. INTERACTIVE HTML REPORT ----
# One self-contained HTML file (works offline): the results from measures.rds are embedded as JSON,
# together with the style and script from html/. Every table has a "Copy for Excel" button.

if (!exists("HELPERS_LOADED")) source(file.path(MODULE, "R", "helpers.R"))
if (!exists("MEAS")) MEAS <- readRDS(file.path(OUT, "measures.rds"))
start_step(6, "HTML report")

tr_name <- names(MEAS$trans)[length(MEAS$trans)]
tr <- MEAS$trans[[tr_name]]
mat <- tr$matrix %>% select(-n) %>% complete(d_from = 1:10, d_to = 1:10, fill = list(share = 0)) %>%
  arrange(d_from, d_to)
DATA <- list(title = TXT$title, waves = MEAS$waves, report = REPORT,
             indicators = c("households", "median", "mean", "gini", "bottom50", "top10", "top5", "top1", "poverty_line", "poor_share"),
             names = list(households = "Households", median = "Median equivalised income (EUR)", mean = "Mean equivalised income (EUR)",
                          gini = "Gini coefficient", bottom50 = "Income share of the bottom 50%", top10 = "Income share of the top 10%",
                          top5 = "Income share of the top 5%", top1 = "Income share of the top 1%",
                          poverty_line = "At-risk-of-poverty threshold (EUR)", poor_share = "At-risk-of-poverty rate"),
             res = lapply(MEAS$res, function(r) list(summary = r$summary, classes = r$classes %>% mutate(class = as.character(class)),
                                                     deciles = r$deciles, regions = r$regions, lorenz = r$lorenz)),
             trans = list(name = tr_name, summary = as.list(tr$summary), matrix = split(mat$share, mat$d_from) %>% unname()),
             notes = paste(TXT$notes$definition, TXT$notes$source))

rd <- function(f) paste(readLines(file.path(MODULE, "html", f), encoding = "UTF-8", warn = FALSE), collapse = "\n")
page <- rd("template.html")
page <- sub("{{TITLE}}", fill(TXT$title, list(wave = REPORT)), page, fixed = TRUE)
page <- sub("{{CSS}}", rd("report.css"), page, fixed = TRUE)
page <- sub("{{JS}}", rd("report.js"), page, fixed = TRUE)
page <- sub("{{DATA}}", jsonlite::toJSON(DATA, auto_unbox = TRUE, digits = 8, dataframe = "rows"), page, fixed = TRUE)
f <- file.path(OUT, paste0("Report_", REPORT, ".html"))
writeLines(page, f, useBytes = TRUE)
cat("HTML report ->", f, "\n")
