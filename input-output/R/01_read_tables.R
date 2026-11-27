# 01. READ PUBLISHED TABLES ----
# Every table of every source in layouts.yml, read from its worksheet layout into one long table
# (one value per row): table, stk_flow, prod_na (row code), induse (column code), geo, time, value.
# Problems in the layout are not fixed silently; they go into READ_LOG: columns without a code,
# duplicate row or column codes (the first occurrence is kept), codes changed by `recode`.

if (!exists("HELPERS_LOADED")) source(file.path(MODULE, "R", "helpers.R"))
start_step(1, "read published tables")

READ_LOG <- list()
log_it <- function(source, table, issue, codes) {
  if (length(codes)) READ_LOG[[length(READ_LOG) + 1]] <<- tibble(source, table, issue, codes = paste(unique(codes), collapse = ", "))
}

sheet_cache <- list()
get_sheet <- function(file, sheet) {
  key <- paste(file, sheet)
  if (is.null(sheet_cache[[key]])) {
    stop_if(!file.exists(file), "Missing input file: ", file, " (run R/00_download.R)")
    sheet_cache[[key]] <<- read.xlsx(file, sheet = sheet, colNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)
  }
  sheet_cache[[key]]
}

read_table <- function(src, id) {
  L <- LAY[[src]]; t <- L$tables[[id]]
  x <- get_sheet(file.path(IN, L$file), t$sheet)
  cc <- trimws(as.character(x[[L$code_col]]))
  start <- 1
  if (!is.null(t$title)) {
    start <- which(grepl(t$title, trimws(as.character(x[[1]]))))[1]
    stop_if(is.na(start), src, " ", id, ": title not found in sheet ", t$sheet)
  }
  hr <- which(cc == "Code" & seq_along(cc) > start)[1]
  stop_if(is.na(hr), src, " ", id, ": no row with column codes")
  # column codes
  cols <- L$first_col:ncol(x)
  ccodes <- trimws(as.character(unlist(x[hr, cols])))
  nocode <- is.na(ccodes) | ccodes == ""
  has_val <- sapply(cols, function(j) any(!is.na(suppressWarnings(as.numeric(x[(hr + 1):min(nrow(x), hr + 120), j])))))
  log_it(src, id, "columns with values but without a code (dropped; column numbers)", cols[nocode & has_val])
  keep <- !nocode
  log_it(src, id, "duplicate column codes (first kept)", ccodes[keep][duplicated(ccodes[keep])])
  keep <- keep & !duplicated(ccodes)
  cols <- cols[keep]; ccodes <- ccodes[keep]
  # rows until the next title or `stop_blank` empty codes
  rows <- integer(0); blank <- 0
  for (r in (hr + 1):nrow(x)) {
    v <- cc[r]
    if (grepl("^\\d{4} - ", trimws(as.character(x[r, 1]))) %in% TRUE) break
    if (is.na(v) || v == "" ) { blank <- blank + 1; if (blank >= L$stop_blank && length(rows)) break; next }
    blank <- 0; rows <- c(rows, r)
  }
  rcodes <- cc[rows]
  for (p in names(L$recode)) {
    ch <- grepl(p, rcodes); log_it(src, id, paste0("row codes recoded (", p, " -> ", L$recode[[p]], ")"), rcodes[ch & !grepl("^KPD_", rcodes)])
    rcodes <- sub(p, L$recode[[p]], rcodes)
  }
  log_it(src, id, "placeholder rows (dropped)", rcodes[rcodes %in% L$drop_codes])
  ok <- !rcodes %in% L$drop_codes & !rcodes %in% c("No", "")
  log_it(src, id, "duplicate row codes (first kept)", rcodes[ok][duplicated(rcodes[ok])])
  ok <- ok & !duplicated(ifelse(ok, rcodes, NA))
  rows <- rows[ok]; rcodes <- rcodes[ok]
  vals <- suppressWarnings(matrix(as.numeric(as.matrix(x[rows, cols])), nrow = length(rows)))
  tibble(source = src, table = id, stk_flow = t$stk_flow,
         prod_na = rep(rcodes, times = length(cols)), induse = rep(ccodes, each = length(rows)),
         geo = L$geo, time_period = L$time_period, unit = L$unit, value = as.vector(vals)) %>%
    filter(!is.na(value))
}

LONG <- bind_rows(lapply(unlist(S$sources), function(src) bind_rows(lapply(names(LAY[[src]]$tables), read_table, src = src))))
READ_LOG <- bind_rows(READ_LOG)
LABELS <- NULL
cat("Read", nrow(LONG), "values from", n_distinct(paste(LONG$source, LONG$table)), "tables\n")
print(LONG %>% count(source, table), n = 20)
if (nrow(READ_LOG)) print(as.data.frame(READ_LOG))
