pd <- data.frame(
  By       = c("1", "1"),
  median   = c(0, 0),
  variance = c(1, 1)
)

test_that("create_cdf_dataframe returns the expected columns and length", {
  res <- create_cdf_dataframe(pd, by_value = "1", n_points = 20)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 20)
  expect_true(all(c("x", "x_log", "y_mean", "ci_lower", "ci_upper", "by_value") %in% names(res)))
})

test_that("the mean CDF is monotonic and bounded in [0, 1]", {
  res <- create_cdf_dataframe(pd, by_value = "1", n_points = 50)
  expect_true(all(res$y_mean >= 0 & res$y_mean <= 1))
  expect_false(is.unsorted(res$y_mean))  # non-decreasing CDF
})

test_that("invalid arguments raise errors", {
  expect_error(create_cdf_dataframe(pd, by_value = "1", n_points = 2))      # below the 3-300 bound
  expect_error(create_cdf_dataframe(pd, by_value = "1", ci_level = 100))    # ci_level must be < 100
  expect_error(create_cdf_dataframe(data.frame(By = "1"), by_value = "1"))  # missing median/variance
  expect_error(create_cdf_dataframe(pd, by_value = "does-not-exist"))       # no matching rows
})
