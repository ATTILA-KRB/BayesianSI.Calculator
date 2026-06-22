test_that("calculate_gP_mean averages the lower/upper quantile midpoints per group", {
  data <- data.frame(
    By             = c("A", "A", "B", "B"),
    lower_quantile = c(-2, -1, -3, -2),
    upper_quantile = c(2, 3, 1, 2)
  )
  res <- calculate_gP_mean(data, P = 90)

  expect_equal(res[["A"]], 0.5)   # midpoints 0 and 1
  expect_equal(res[["B"]], -0.5)  # midpoints -1 and 0
})

test_that("calculate_gP_mean errors when required columns are missing", {
  expect_error(calculate_gP_mean(data.frame(By = "A"), P = 90))
})
