make_interval_row <- function(Median, TI_Lower, TI_Upper, PI_Lower, PI_Upper) {
  data.frame(
    Median = Median,
    CI_Lower = Median - 0.5, CI_Upper = Median + 0.5,
    TI_Lower = TI_Lower, TI_Upper = TI_Upper,
    PI_Lower = PI_Lower, PI_Upper = PI_Upper
  )
}

test_that("lognorm_adjuster computes reducible uncertainty as a percentage", {
  data <- make_interval_row(Median = 1, TI_Lower = 0.3, TI_Upper = 1.7,
                            PI_Lower = 0.4, PI_Upper = 1.6)
  res <- lognorm_adjuster(data, FALSE)

  expect_equal(res$ReducibleUpper, round((1.7 - 1.6) / (1.7 - 1) * 100, 3))
  expect_equal(res$ReducibleLower, round((0.3 - 0.4) / (0.3 - 1) * 100, 3))
})

test_that("lognorm_adjuster returns NA instead of Inf/NaN on a zero-width interval", {
  # Regression: TI bound equal to the Median used to divide by zero
  upper_degenerate <- make_interval_row(Median = 1.7, TI_Lower = 0.3, TI_Upper = 1.7,
                                        PI_Lower = 0.4, PI_Upper = 1.6)
  res_upper <- lognorm_adjuster(upper_degenerate, FALSE)
  expect_true(is.na(res_upper$ReducibleUpper))
  expect_false(is.infinite(res_upper$ReducibleUpper))

  lower_degenerate <- make_interval_row(Median = 0.3, TI_Lower = 0.3, TI_Upper = 1.7,
                                        PI_Lower = 0.4, PI_Upper = 1.6)
  res_lower <- lognorm_adjuster(lower_degenerate, FALSE)
  expect_true(is.na(res_lower$ReducibleLower))
})

test_that("lognorm_adjuster exponentiates values when log_normal is TRUE", {
  data <- make_interval_row(Median = 0, TI_Lower = -1, TI_Upper = 1,
                            PI_Lower = -0.5, PI_Upper = 0.5)
  res <- lognorm_adjuster(data, TRUE)
  expect_equal(res$Median, exp(0))
  expect_equal(res$TI_Upper, exp(1))
})

test_that("lognorm_adjuster requires input data", {
  expect_error(lognorm_adjuster())
})
