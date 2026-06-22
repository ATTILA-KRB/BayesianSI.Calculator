# Standard-normal predicted distribution: median 0, variance 1, so the CDF
# domain is roughly [-3.72, 3.72].
pd <- data.frame(
  By       = c("1", "1"),
  median   = c(0, 0),
  variance = c(1, 1)
)

test_that("a single in-range value returns its probability", {
  res <- get_probability_table(pred_dist = pd, by_value = "1", x_value = "0", ci_percent = 90)
  expect_s3_class(res, "data.frame")
  expect_equal(res[[1]], 0)
  expect_equal(res[[2]], 0.5, tolerance = 0.01)  # P(X <= 0) for N(0,1)
})

test_that("a single out-of-range value is reported as out of range", {
  res <- get_probability_table(pred_dist = pd, by_value = "1", x_value = "100", ci_percent = 90)
  expect_equal(res[[2]], "Out of range")
})

test_that("an in-range interval returns the interval probability", {
  res <- get_probability_table(pred_dist = pd, by_value = "1", x_value = "[-1, 1]", ci_percent = 90)
  expect_true(is.numeric(res[[2]]))
  expect_equal(res[[2]], pnorm(1) - pnorm(-1), tolerance = 0.02)  # ~0.6827
})

test_that("an out-of-range interval is reported as out of range (range-validation regression)", {
  # Before the fix the range check was tautological and never detected
  # out-of-range intervals; this pins the corrected behaviour.
  res <- get_probability_table(pred_dist = pd, by_value = "1", x_value = "[-100, 100]", ci_percent = 90)
  expect_equal(res[[2]], "Out of range")
})
