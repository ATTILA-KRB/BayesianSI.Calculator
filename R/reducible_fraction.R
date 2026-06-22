#' Reducible uncertainty fraction
#'
#' Internal helper computing the fraction of the tolerance-interval gap that is
#' not covered by the prediction interval, i.e. `(TI - PI) / (TI - Median)`.
#'
#' Guards against a zero-width interval (a tolerance-interval bound equal to the
#' median), which would otherwise divide by zero and yield a silent `Inf`/`NaN`;
#' such degenerate rows return `NA` instead.
#'
#' Callers are responsible for any further scaling (e.g. to a percentage) and
#' rounding.
#'
#' @param ti_bound Numeric vector of tolerance-interval bounds.
#' @param pi_bound Numeric vector of prediction-interval bounds.
#' @param median Numeric vector of point estimates (medians).
#'
#' @return Numeric vector of reducible fractions, with `NA` where
#'   `ti_bound == median`.
#' @noRd
reducible_fraction <- function(ti_bound, pi_bound, median) {
  denom <- ti_bound - median
  ifelse(denom == 0, NA_real_, (ti_bound - pi_bound) / denom)
}
