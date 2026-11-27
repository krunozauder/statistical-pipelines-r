# 01. READ ----
# Household files for all waves; equivalised income.
# Stops if a file or column is missing, a household appears twice in a wave, or a weight is not positive.

if (!exists("HELPERS_LOADED")) source(file.path(MODULE, "R", "helpers.R"))
start_step(1, "read")

COLS <- c("wave", "hh_id", "weight", "region", "adults", "children", "income")
read_wave <- function(wv) {
  f <- file.path(IN, paste0("households_", wv, ".csv"))
  stop_if(!file.exists(f), "Missing input file: ", f)
  d <- read.csv(f, stringsAsFactors = FALSE, colClasses = c(wave = "character"))
  miss <- setdiff(COLS, names(d))
  stop_if(length(miss) > 0, "Columns missing in ", basename(f), ": ", paste(miss, collapse = ", "))
  stop_if(any(duplicated(d$hh_id)), "Households reported twice in ", basename(f))
  stop_if(any(!(d$weight > 0)), "Weights must be positive: ", basename(f))
  d
}
E <- S$equivalence
HH <- bind_rows(lapply(WAVES, read_wave)) %>%
  mutate(scale = E$first_adult + E$other_adult * pmax(adults - 1, 0) + E$child * children,
         eq_income = income / scale)

cat("Read", nrow(HH), "households in", length(WAVES), "waves\n")
