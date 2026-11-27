# 04. TEXT ----
# Draft of the release text from texts.yml, with every number taken from measures.rds.
# Word output needs pandoc (comes with RStudio); otherwise only the .md file is written.

if (!exists("HELPERS_LOADED")) source(file.path(MODULE, "R", "helpers.R"))
if (!exists("MEAS")) MEAS <- readRDS(file.path(OUT, "measures.rds"))
start_step(4, "text")

k    <- match(REPORT, MEAS$waves)
cur  <- MEAS$res[[REPORT]]$summary
prev <- if (k > 1) MEAS$res[[MEAS$waves[k - 1]]]$summary else NULL
trs  <- if (k > 1) MEAS$trans[[paste(MEAS$waves[k - 1], REPORT, sep = "-")]]$summary else NULL
chg  <- if (!is.null(prev)) cur$median / prev$median - 1 else NA

VALS <- list(wave = REPORT, prev_wave = MEAS$waves[max(k - 1, 1)],
             median = fmt_eur(cur$median), mean = fmt_eur(cur$mean),
             median_change = if (is.na(chg)) "" else paste0(fmt_pct(abs(chg)), if (chg >= 0) " higher" else " lower"),
             top10 = fmt_pct(cur$top10), bottom50 = fmt_pct(cur$bottom50),
             gini = formatC(100 * cur$gini, format = "f", digits = 1),
             gini_prev = if (is.null(prev)) "" else formatC(100 * prev$gini, format = "f", digits = 1),
             poor_share = fmt_pct(cur$poor_share), poverty_line = fmt_eur(cur$poverty_line),
             stayed = if (is.null(trs)) "" else fmt_pct(trs$stayed, 0),
             moved_up = if (is.null(trs)) "" else fmt_pct(trs$moved_up, 0),
             moved_down = if (is.null(trs)) "" else fmt_pct(trs$moved_down, 0))

pars <- unlist(TXT$paragraphs)
if (k == 1) pars <- pars[!grepl("prev_wave|stayed", pars)]   # first wave: no comparisons
pars <- sapply(pars, fill, vals = VALS, USE.NAMES = FALSE)
pars <- sub("^(.)", "\\U\\1", pars, perl = TRUE)
md <- c(paste("#", fill(TXT$title, VALS)), "", rbind(pars, ""), paste0("*", TXT$notes$definition, " ", TXT$notes$source, "*"))
f_md <- file.path(OUT, paste0("Text_", REPORT, ".md"))
writeLines(md, f_md, useBytes = TRUE)
if (rmarkdown::pandoc_available()) rmarkdown::pandoc_convert(f_md, to = "docx", output = sub("\\.md$", ".docx", basename(f_md)))
TEXT_VALUES <- VALS
cat(pars, sep = "\n\n")
