# DISTRIBUTION MEASURES ----
# Weighted versions of the usual measures. x = income, w = weights.

# weighted quantile, Hyndman-Fan type 7 generalised to weights (Kish effective sample size);
# with equal weights it gives the same result as quantile(x, type = 7)
wquantile <- function(x, w, probs) {
  o <- order(x); x <- x[o]; w <- w[o] / sum(w)
  n  <- 1 / sum(w^2)
  cp <- cumsum(c(0, w))
  sapply(probs, function(p) {
    h <- p * (n - 1) + 1
    u <- pmax((h - 1) / n, pmin(h / n, cp))
    sum(diff(u * n - h + 1) * x)
  })
}

# Lorenz curve: cumulative population share p and income share L, from (0, 0) to (1, 1)
lorenz <- function(x, w) {
  o <- order(x); x <- x[o]; w <- w[o]
  tibble(p = c(0, cumsum(w) / sum(w)), L = c(0, cumsum(x * w) / sum(x * w)))
}

# income share of the population below population share p (linear between observations)
lorenz_at <- function(lz, p) approx(lz$p, lz$L, xout = p, ties = "ordered")$y

gini <- function(x, w) {
  lz <- lorenz(x, w)
  1 - sum(diff(lz$p) * (head(lz$L, -1) + tail(lz$L, -1)))
}

# households and income by class (breaks = lower bounds, last class open)
class_table <- function(x, w, breaks) {
  lab <- c(paste0(formatC(head(breaks, -1), format = "d", big.mark = ","), "-", formatC(tail(breaks, -1), format = "d", big.mark = ",")),
           paste0(formatC(tail(breaks, 1), format = "d", big.mark = ","), " and more"))
  tibble(class = factor(lab[findInterval(x, breaks)], levels = lab), x, w) %>%
    group_by(class, .drop = FALSE) %>%
    summarise(households = sum(w), income = sum(x * w), .groups = "drop") %>%
    mutate(share_households = households / sum(households), share_income = income / sum(income))
}

# decile groups by weighted cut points
decile <- function(x, w) findInterval(x, wquantile(x, w, seq(0.1, 0.9, 0.1)), left.open = TRUE) + 1
