test_that("calculate_PI brackets the mean for a single group", {
  data <- data.frame(
    By = rep("A", 4),
    mean = rep(10, 4),
    variance = rep(4, 4)
  )
  res <- calculate_PI(data, percent_for_pi = 95)

  expect_true(all(c("By", "PI_Lower", "PI_Upper") %in% names(res)))
  expect_equal(nrow(res), 1)
  expect_lt(res$PI_Lower, 10)
  expect_gt(res$PI_Upper, 10)
  # Symmetric normal: bounds should sit roughly 1.96 sd either side of the mean.
  # Tolerance is loose because calculate_PI converges on the probability scale.
  expect_equal(res$PI_Lower, qnorm(0.025, 10, 2), tolerance = 0.05)
  expect_equal(res$PI_Upper, qnorm(0.975, 10, 2), tolerance = 0.05)
})

test_that("calculate_PI returns one row per group", {
  data <- data.frame(
    By = c("A", "A", "B", "B"),
    mean = c(10, 11, 20, 21),
    variance = c(2, 2.1, 3, 3.1)
  )
  res <- calculate_PI(data, percent_for_pi = 90)
  expect_equal(nrow(res), 2)
  expect_setequal(res$By, c("A", "B"))
})

test_that("calculate_PI validates the requested percentage", {
  data <- data.frame(By = "A", mean = 10, variance = 4)
  expect_error(calculate_PI(data, percent_for_pi = 100))
  expect_error(calculate_PI(data, percent_for_pi = 0))
})
