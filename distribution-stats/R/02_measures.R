# 02. MEASURES ----
# Everything the tables, charts, text and HTML report use is computed here once and saved to
# OUT/measures.rds, so all outputs agree by construction.

if (!exists("HELPERS_LOADED")) source(file.path(MODULE, "R", "helpers.R"))
start_step(2, "measures")

one_wave <- function(d) {
  x <- d$eq_income; w <- d$weight
  lz  <- lorenz(x, w)
  med <- wquantile(x, w, 0.5)
  pl  <- S$poverty_line * med
  list(summary = tibble(households = sum(w), median = med, mean = weighted.mean(x, w), gini = gini(x, w),
                        bottom50 = lorenz_at(lz, 0.5),
                        !!!setNames(as.list(1 - lorenz_at(lz, 1 - unlist(S$top_shares))), paste0("top", 100 * unlist(S$top_shares))),
                        poverty_line = pl, poor_share = sum(w[x < pl]) / sum(w)),
       classes = class_table(x, w, unlist(S$classes)),
       deciles = tibble(decile = decile(x, w), x, w) %>%
         group_by(decile) %>%
         summarise(households = sum(w), upper_bound = max(x), mean = weighted.mean(x, w), income = sum(x * w), .groups = "drop") %>%
         mutate(share_income = income / sum(income)),
       regions = d %>% group_by(region) %>%
         summarise(households = sum(weight), median = wquantile(eq_income, weight, 0.5), mean = weighted.mean(eq_income, weight),
                   poor_share = sum(weight[eq_income < pl]) / sum(weight), .groups = "drop"),
       lorenz = tibble(p = seq(0, 1, 0.01), L = lorenz_at(lz, seq(0, 1, 0.01))))
}

RES <- lapply(setNames(WAVES, WAVES), function(wv) one_wave(filter(HH, wave == wv)))

# transitions between consecutive waves: deciles in each wave's full sample, households in both waves,
# weights of the earlier wave
TRANS <- list()
for (k in seq_along(WAVES)[-1]) {
  a <- HH %>% filter(wave == WAVES[k - 1]) %>% mutate(d_from = decile(eq_income, weight))
  b <- HH %>% filter(wave == WAVES[k]) %>% mutate(d_to = decile(eq_income, weight))
  p <- inner_join(a %>% select(hh_id, weight, d_from), b %>% select(hh_id, d_to), by = "hh_id")
  m <- p %>% count(d_from, d_to, wt = weight) %>% group_by(d_from) %>% mutate(share = n / sum(n)) %>% ungroup()
  TRANS[[paste(WAVES[k - 1], WAVES[k], sep = "-")]] <- list(
    matrix = m,
    summary = tibble(panel_households = sum(p$weight),
                     stayed = sum(p$weight[p$d_to == p$d_from]) / sum(p$weight),
                     moved_up = sum(p$weight[p$d_to > p$d_from]) / sum(p$weight),
                     moved_down = sum(p$weight[p$d_to < p$d_from]) / sum(p$weight)))
}

MEAS <- list(waves = WAVES, report = REPORT, res = RES, trans = TRANS)
saveRDS(MEAS, file.path(OUT, "measures.rds"))
print(bind_rows(lapply(RES, `[[`, "summary"), .id = "wave"), width = 120)
