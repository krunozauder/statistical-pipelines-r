# 04. FEEDBACK TO REPORTING UNITS ----
# For the report period, every flagged unit gets an Excel file (summary, failing records, rule
# definitions) and a letter. Letter sentences come from rules.yml and letters.yml, so the same issue
# always produces the same text. Word letters need pandoc (comes with RStudio); without it a .md file
# is written. A list of letters is saved for the register of outgoing mail.

if (!exists("HELPERS_LOADED")) source(file.path(MODULE, "R", "helpers.R"))
if (!exists("CHECKS")) CHECKS <- readRDS(file.path(OUT, "checks.rds"))
suppressMessages(library(openxlsx))
start_step(4, paste("feedback for", REPORT))

DIR_X <- file.path(OUT, "feedback"); DIR_L <- file.path(OUT, "letters")
dir.create(DIR_X, showWarnings = FALSE); dir.create(DIR_L, showWarnings = FALSE)
MAX_IDS  <- S$letters$records_listed_max
DATE     <- S$letters$date %||% format(Sys.Date(), "%d.%m.%Y")
HEAD     <- createStyle(textDecoration = "bold", fgFill = "#DCE6F1", border = "Bottom", wrapText = TRUE)

flag <- CHECKS$units %>% filter(period == REPORT, flagged)
rec  <- CHECKS$rec %>% filter(period == REPORT)

add_sheet <- function(wb, name, d, widths = 14) {
  addWorksheet(wb, name)
  writeData(wb, name, d, headerStyle = HEAD)
  freezePane(wb, name, firstRow = TRUE)
  setColWidths(wb, name, cols = seq_along(d), widths = widths)
}

# one sentence per flagged rule
sentence <- function(id, s, u) {
  r   <- rule_version(RULES[[id]], REPORT)
  ids <- CHECKS$records %>% filter(period == REPORT, unit_id == u, rule == id) %>% pull(establishment_id)
  ids <- if (length(ids) <= MAX_IDS) paste(ids, collapse = ", ") else "listed in the annex"
  ut  <- CHECKS$uni %>% filter(period == REPORT, unit_id == u)
  fill(r$letter, list(n = s$n, base = s$base, share = fmt_pct(s$share), ids = ids,
                      turnover_total = fmt_eur(ut$turnover_total), turnover_sum = fmt_eur(ut$turnover_sum),
                      employees_total = ut$employees_total, employees_sum = ut$employees_sum))
}

write_letter <- function(u, lines, file, annex = TRUE) {
  reg <- CHECKS$reg %>% filter(unit_id == u)
  v   <- list(unit_name = reg$unit_name, period_text = period_text(REPORT), reply_days = S$letters$reply_days)
  body <- if (length(lines) == 1) {
    paste0(fill(LET$intro_one, v), " ", lines, ".")
  } else {
    paste0(fill(LET$intro_more, v), "\n\n", paste0("- ", lines, ";", collapse = "\n"))
  }
  md <- c(LET$sender, "", DATE, "", reg$unit_name, "", paste0("**", fill(LET$subject, v), "**"), "",
          LET$opening, "", body, "", if (annex) c(LET$annex, ""), fill(LET$request, v), "", LET$closing, "", LET$signature)
  f_md <- paste0(file, ".md")
  writeLines(md, f_md, useBytes = TRUE)
  if (rmarkdown::pandoc_available()) {
    rmarkdown::pandoc_convert(f_md, to = "docx", output = paste0(file, ".docx"))
    file.remove(f_md)
    paste0(basename(file), ".docx")
  } else basename(f_md)
}

letters_list <- list()
for (u in unique(flag$unit_id)) {
  fu  <- flag %>% filter(unit_id == u) %>% arrange(rule)
  ids <- fu$rule
  # Excel ----
  summ <- fu %>%
    transmute(Rule = rule, Issue = sapply(rule, function(x) RULES[[x]]$name),
              `Records checked` = base, `Records failing` = n,
              `Share failing` = round(share, 4), `Share, all units` = round(share_total, 4))
  bad <- CHECKS$records %>% filter(period == REPORT, unit_id == u, rule %in% ids) %>%
    group_by(establishment_id) %>% summarise(Rules = paste(rule, collapse = ", "), .groups = "drop")
  recs <- rec %>% filter(unit_id == u, establishment_id %in% bad$establishment_id) %>%
    select(period:gross_wages, prev_turnover) %>% left_join(bad, by = "establishment_id")
  defs <- tibble(Rule = ids,
                 Issue     = sapply(ids, function(x) RULES[[x]]$name),
                 `Applies to` = sapply(ids, function(x) rule_version(RULES[[x]], REPORT)$applies_to),
                 Check     = sapply(ids, function(x) rule_version(RULES[[x]], REPORT)$check))
  wb <- createWorkbook()
  add_sheet(wb, "Summary", summ, c(8, 40, 14, 14, 14, 14))
  if (nrow(recs)) add_sheet(wb, "Records", recs)
  add_sheet(wb, "Rules", defs, c(8, 40, 40, 60))
  f_x <- paste0(REPORT, "_", u, ".xlsx")
  saveWorkbook(wb, file.path(DIR_X, f_x), overwrite = TRUE)
  # letter ----
  lines <- sapply(ids, function(id) sentence(id, fu[fu$rule == id, ], u))
  f_l <- write_letter(u, lines, file.path(DIR_L, paste0(REPORT, "_", u)), annex = nrow(recs) > 0)
  reg <- CHECKS$reg %>% filter(unit_id == u)
  letters_list[[u]] <- tibble(unit_id = u, unit_name = reg$unit_name, email = reg$email,
                              rules = paste(ids, collapse = ", "), letter = f_l, annex = f_x)
}

LETTERS_LIST <- bind_rows(letters_list)
if (nrow(LETTERS_LIST)) write.xlsx(LETTERS_LIST, file.path(OUT, paste0(REPORT, "_letters_list.xlsx")), overwrite = TRUE)
cat("Letters and feedback files:", nrow(LETTERS_LIST), "units ->", DIR_L, "and", DIR_X, "\n")
