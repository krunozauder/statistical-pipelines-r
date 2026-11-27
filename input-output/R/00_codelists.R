# 00. CODE LISTS ----
# Builds the code lists in config/codelists from the English labels of the CBS publication
# (rows: products and transactions; columns: industries and uses). Run once; the lists are then the
# reference against which every source is validated in step 03.
# Usage (from the module folder): Rscript R/00_codelists.R

suppressMessages({library(dplyr); library(openxlsx)})
x  <- read.xlsx("data/input/12-01-04_01_2015.xlsx", sheet = "Table data english", colNames = FALSE,
                skipEmptyRows = FALSE, skipEmptyCols = FALSE)
c1 <- trimws(as.character(x[[1]])); c2 <- trimws(as.character(x[[2]]))
hdr <- which(c1 == "Code")
rows <- bind_rows(lapply(hdr, function(h) {
  r <- (h + 1):nrow(x)
  end <- which(is.na(c1[r]) | c1[r] == "" | grepl("^\\d{4} - ", c1[r]))[1]
  r <- r[seq_len(if (is.na(end)) length(r) else end - 1)]
  tibble(code = c1[r], label = c2[r])
}))
cols <- bind_rows(lapply(hdr, function(h) {
  tibble(code = trimws(as.character(unlist(x[h, -(1:2)]))), label = trimws(as.character(unlist(x[h - 1, -(1:2)]))))
}))
clean <- function(d) d %>% filter(!is.na(code), code != "") %>% mutate(label = gsub("\\s+", " ", label)) %>% distinct(code, .keep_all = TRUE)
write.csv(clean(rows), "config/codelists/prod_na.csv", row.names = FALSE)
write.csv(clean(cols), "config/codelists/induse.csv", row.names = FALSE)
cat("prod_na:", nrow(clean(rows)), "codes; induse:", nrow(clean(cols)), "codes\n")
