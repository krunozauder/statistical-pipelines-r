# 03. CHECKS ----
# Every rule in rules.yml, in the version valid for the period. Record rules: a unit is flagged when the
# share of failing records in its base is above the threshold, at least min_records fail and, where
# vs_total is set, its share is above vs_total x the share for all units. Unit rules: flagged when true.
# Output: OUT/checks.rds (unit results, failing records, prepared data) -> steps 04 and 05.

if (!exists("HELPERS_LOADED")) source(file.path(MODULE, "R", "helpers.R"))
start_step(3, "checks")

res_unit <- list(); res_rec <- list()
for (p in PERIODS) {
  d <- filter(REC, period == p); u <- filter(UNI, period == p)
  for (id in names(RULES)) {
    r <- rule_version(RULES[[id]], p)
    if (is.null(r)) next
    if (r$level == "record") {
      base <- eval_in(r$applies_to, d)
      fail <- base & eval_in(r$check, d)
      res_rec[[length(res_rec) + 1]] <- d[fail, c("period", "unit_id", "establishment_id")] %>% mutate(rule = id)
      s <- tibble(unit_id = d$unit_id, base, fail) %>%
        group_by(unit_id) %>%
        summarise(base = sum(base), n = sum(fail), .groups = "drop") %>%
        filter(base > 0)
      tot <- sum(s$n) / sum(s$base)
      s <- s %>% mutate(share = n / base, share_total = tot,
                        flagged = n >= (r$min_records %||% 1) & share > (r$threshold %||% 0) &
                                  (is.null(r$vs_total) | share > (r$vs_total %||% 0) * tot))
    } else {
      base <- eval_in(r$applies_to, u)
      fail <- base & eval_in(r$check, u)
      s <- tibble(unit_id = u$unit_id, base = as.integer(base), n = as.integer(fail)) %>%
        filter(base > 0) %>%
        mutate(share = n / base, share_total = mean(fail[base]), flagged = n > 0)
    }
    res_unit[[length(res_unit) + 1]] <- s %>% mutate(period = p, rule = id, .before = 1)
  }
}

CHECKS <- list(units   = bind_rows(res_unit),
               records = bind_rows(res_rec),
               rec     = REC,
               uni     = UNI,
               reg     = REG)
saveRDS(CHECKS, file.path(OUT, "checks.rds"))

flag <- CHECKS$units %>% filter(flagged)
cat("Flagged unit-rule-periods:", nrow(flag), "\n")
print(flag %>% count(period, rule) %>% pivot_wider(names_from = period, values_from = n, values_fill = 0), n = 50)
