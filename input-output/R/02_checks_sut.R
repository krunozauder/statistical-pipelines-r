# 02. CHECKS OF THE SUPPLY AND USE TABLES ----
# Accounting identities that published tables must satisfy, for every source:
#   1 supply = use by product (T1500 SUPBP vs T1610 TU)
#   2 output by industry in supply = in use (T1500 CPA_TOTAL vs T1610 P1)
#   3 domestic + imported use = total use, every product x use cell (T1611 + T1612 vs T1610)
#   4 value added = output - intermediate consumption (P1 - CPA_TOTAL - D21_M_D31 vs B1G)
#   5 value added = compensation + other net taxes on production + operating surplus (D1 + D29_M_D39 + B2G_B3G vs B1G)
# Cells are compared where the column exists in all tables. Differences above the tolerance are
# listed, not corrected.

if (!exists("HELPERS_LOADED")) source(file.path(MODULE, "R", "helpers.R"))
start_step(2, "checks of supply and use tables")

TOL <- S$tolerance$balance
val <- function(d, tab, rows = NULL, cols = NULL) {
  d %>% filter(table == tab, is.null(rows) | prod_na %in% rows, is.null(cols) | induse %in% cols)
}
codes_of <- function(src) {
  d <- LONG %>% filter(source == src, table == "T1500")
  cols <- unique(d$induse)
  list(products = setdiff(unique(d$prod_na[grepl("^CPA_", d$prod_na)]), "CPA_TOTAL"),
       industries = cols[seq_len(match("TOTAL", cols) - 1)])
}
compare <- function(src, check, a, b, by) {
  full_join(a, b, by = by, suffix = c("_a", "_b")) %>%
    mutate(value_a = coalesce(value_a, 0), value_b = coalesce(value_b, 0), diff = value_a - value_b,
           source = src, check = check)
}

CHK <- list()
for (src in unlist(S$sources)) {
  d <- LONG %>% filter(source == src); k <- codes_of(src)
  cells <- d %>% filter(prod_na %in% k$products)
  CHK[[length(CHK) + 1]] <- compare(src, "1 supply = use by product",
    val(d, "T1500", k$products, "SUPBP") %>% select(prod_na, value), val(d, "T1610", k$products, "TU") %>% select(prod_na, value), "prod_na")
  CHK[[length(CHK) + 1]] <- compare(src, "2 output by industry, supply = use",
    val(d, "T1500", "CPA_TOTAL", k$industries) %>% select(induse, value), val(d, "T1610", "P1", k$industries) %>% select(induse, value), "induse")
  CHK[[length(CHK) + 1]] <- compare(src, "3 domestic + imports = total use",
    filter(cells, table %in% c("T1611", "T1612"), induse %in% cells$induse[cells$table == "T1610"]) %>%
      group_by(prod_na, induse) %>% summarise(value = sum(value), .groups = "drop"),
    filter(cells, table == "T1610") %>% select(prod_na, induse, value), c("prod_na", "induse"))
  w <- val(d, "T1610", c("P1", "CPA_TOTAL", "D21_M_D31", "B1G", "D1", "D29_M_D39", "B2G_B3G"), k$industries) %>%
    select(prod_na, induse, value) %>% pivot_wider(names_from = prod_na, values_from = value, values_fill = 0)
  CHK[[length(CHK) + 1]] <- compare(src, "4 value added = output - intermediate consumption",
    w %>% transmute(induse, value = P1 - CPA_TOTAL - D21_M_D31), w %>% transmute(induse, value = B1G), "induse")
  CHK[[length(CHK) + 1]] <- compare(src, "5 value added = D1 + D29_M_D39 + B2G_B3G",
    w %>% transmute(induse, value = D1 + D29_M_D39 + B2G_B3G), w %>% transmute(induse, value = B1G), "induse")
}
CHK <- bind_rows(CHK)
CHECKS_SUT <- CHK %>%
  group_by(source, check) %>%
  summarise(cells = n(), differences = sum(abs(diff) > TOL), max_abs_difference = max(abs(diff)), .groups = "drop") %>%
  mutate(status = ifelse(differences == 0, "OK", "DIFFERENCE"))
print(as.data.frame(CHECKS_SUT))
