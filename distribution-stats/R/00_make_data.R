# 00. SYNTHETIC DATA ----
# A fictional household panel: two waves, part of the households observed in both, household weights,
# region, composition and disposable income. Also a file of "previously published" figures for the
# first wave, used by the checks in step 05 (one figure deliberately differs, as after a revision).
#
# Usage (from the module folder): Rscript R/00_make_data.R [output folder]   default: data/input

suppressMessages({library(dplyr); library(yaml)})
args <- commandArgs(TRUE)
S <- yaml.load_file("config/settings.yml")
OUT <- if (length(args)) args[1] else S$paths$input
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
set.seed(1404)

N        <- 2500
REGIONS  <- c("North", "South", "East", "West", "Capital", "Islands")
POP      <- 1450000                                     # households in the fictional country

hh <- tibble(hh_id    = sprintf("H%05d", 1:N),
             region   = sample(REGIONS, N, replace = TRUE, prob = c(.2, .15, .15, .15, .25, .1)),
             adults   = sample(1:4, N, replace = TRUE, prob = c(.30, .45, .17, .08)),
             children = rpois(N, 0.6),
             level    = rlnorm(N, meanlog = 9.9, sdlog = 0.55) * ifelse(region == "Capital", 1.25, 1))

wave1 <- hh %>%
  mutate(wave = S$waves[[1]],
         income = round(level * (1 + 0.5 * (adults - 1) + 0.3 * children) * exp(rnorm(N, 0, 0.10))),
         weight = POP / N * runif(N, 0.6, 1.4))

# second wave: 88% stay, income grows on average, some move a lot; new households replace those who left
stay <- runif(N) < 0.88
new  <- tibble(hh_id    = sprintf("H%05d", N + seq_len(sum(!stay))),
               region   = sample(REGIONS, sum(!stay), replace = TRUE),
               adults   = sample(1:4, sum(!stay), replace = TRUE, prob = c(.35, .45, .14, .06)),
               children = rpois(sum(!stay), 0.6),
               level    = rlnorm(sum(!stay), meanlog = 9.9, sdlog = 0.55))
wave2 <- bind_rows(
  wave1[stay, ] %>% mutate(income = round(income * exp(rnorm(n(), 0.05, 0.18)))),
  new %>% mutate(income = round(level * (1 + 0.5 * (adults - 1) + 0.3 * children) * exp(rnorm(n(), 0.05, 0.10))))
) %>% mutate(wave = S$waves[[2]], weight = POP * 1.01 / n() * runif(n(), 0.6, 1.4))

cols <- c("wave", "hh_id", "weight", "region", "adults", "children", "income")
write.csv(wave1[cols], file.path(OUT, paste0("households_", S$waves[[1]], ".csv")), row.names = FALSE)
write.csv(wave2[cols], file.path(OUT, paste0("households_", S$waves[[2]], ".csv")), row.names = FALSE)

# "published" figures for the first wave: rounded as in a release; the number of households
# was published before a revision of the weights and differs on purpose
source("R/functions.R")
eq <- wave1$income / (1 + 0.5 * (wave1$adults - 1) + 0.3 * wave1$children)
published <- tibble(wave      = S$waves[[1]],
                    indicator = c("households", "median", "mean", "gini", "top10", "poor_share"),
                    value     = c(round(sum(wave1$weight), -3) + 3000,
                                  round(wquantile(eq, wave1$weight, 0.5), -1),
                                  round(weighted.mean(eq, wave1$weight), -1),
                                  round(100 * gini(eq, wave1$weight), 1),
                                  round(100 * (1 - lorenz_at(lorenz(eq, wave1$weight), 0.9)), 1),
                                  round(100 * sum(wave1$weight[eq < 0.6 * wquantile(eq, wave1$weight, 0.5)]) / sum(wave1$weight), 1)),
                    unit      = c("count", "eur", "eur", "pct", "pct", "pct"),
                    rounding  = c(-3, -1, -1, 1, 1, 1),
                    source    = "Release of 15 June (fictional)")
write.csv(published, file.path(OUT, "published.csv"), row.names = FALSE)
cat("Synthetic data:", nrow(wave1), "and", nrow(wave2), "households ->", OUT, "\n")
