pd <- data.frame(
  By       = c("A", "A", "B", "B"),
  mean     = c(0, 0, 1, 1),
  median   = c(0, 0, 1, 1),
  variance = c(1, 1, 1, 1)
)

test_that("create_CDF returns a single data frame for a given by_value", {
  res <- create_CDF(pred_dist = pd, by_value = "A", n_points = 20)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 20)
  expect_true(all(c("x", "y_mean", "ci_lower", "ci_upper") %in% names(res)))
  expect_true(all(res$y_mean >= 0 & res$y_mean <= 1))
})

test_that("create_CDF returns a named list over all by_values when by_value is NULL", {
  res <- create_CDF(pred_dist = pd, by_value = NULL, n_points = 20)
  expect_type(res, "list")
  expect_setequal(names(res), c("A", "B"))
  expect_s3_class(res[["A"]], "data.frame")
})

test_that("create_all_cdf_dataframes builds one CDF frame per by_value", {
  res <- create_all_cdf_dataframes(pd, n_points = 20)
  expect_type(res, "list")
  expect_setequal(names(res), c("A", "B"))
  expect_equal(nrow(res[["A"]]), 20)
})

test_that("create_all_cdf_dataframes errors without a By column", {
  expect_error(create_all_cdf_dataframes(data.frame(median = 0, variance = 1)))
})
