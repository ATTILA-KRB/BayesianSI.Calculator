test_that("calculate_tolerance_interval returns ordered bounds", {
  data <- data.frame(
    Group = rep("A", 100),
    lower_quantile = 1:100,
    upper_quantile = 101:200
  )
  res <- calculate_tolerance_interval(data, "lower_quantile", "upper_quantile", 95, "Group")

  expect_true(all(c("By", "TI_Lower", "TI_Upper") %in% names(res)))
  expect_equal(nrow(res), 1)
  expect_lt(res$TI_Lower, res$TI_Upper)
})

test_that("calculate_tolerance_interval handles multiple groups", {
  data <- data.frame(
    Group = rep(c("A", "B"), each = 50),
    lower_quantile = c(1:50, 1:50),
    upper_quantile = c(51:100, 51:100)
  )
  res <- calculate_tolerance_interval(data, "lower_quantile", "upper_quantile", 90, "Group")
  expect_equal(nrow(res), 2)
})

test_that("calculate_tolerance_interval errors on missing columns", {
  data <- data.frame(Group = "A", lower_quantile = 1, upper_quantile = 2)
  expect_error(
    calculate_tolerance_interval(data, "nope", "upper_quantile", 95, "Group")
  )
})
