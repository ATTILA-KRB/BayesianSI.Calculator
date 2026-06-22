# End-to-end tests of the calculation pipeline: from raw MCMC-style draws
# (fixed effect + variance component, grouped by a "By" column) through to the
# merged interval table.

make_mcmc_data <- function(n = 200, groups = "1", seed = 42) {
  set.seed(seed)
  do.call(rbind, lapply(groups, function(g) {
    data.frame(
      t1 = rnorm(n, mean = 10, sd = 0.5),   # fixed effect
      v1 = rgamma(n, shape = 2, rate = 2),  # variance component (positive)
      By = g,
      stringsAsFactors = FALSE
    )
  }))
}

interval_cols <- c("By", "Median", "CI_Lower", "CI_Upper",
                   "PI_Lower", "PI_Upper", "TI_Lower", "TI_Upper",
                   "ReducibleUpper", "ReducibleLower")

test_that("perform_calculations returns a well-formed single-group result", {
  res <- perform_calculations(make_mcmc_data(), "t1", "v1", by = "By", tolerance_level = 90)

  expect_type(res, "list")
  expect_true(all(c("data", "summary") %in% names(res)))

  d <- res$data
  expect_equal(nrow(d), 1)
  expect_true(all(interval_cols %in% names(d)))

  # All interval bounds finite and properly ordered
  for (col in setdiff(interval_cols, "By")) expect_true(is.finite(d[[col]]), info = col)
  expect_lt(d$CI_Lower, d$CI_Upper)
  expect_lt(d$PI_Lower, d$PI_Upper)
  expect_lt(d$TI_Lower, d$TI_Upper)
  expect_lte(d$CI_Lower, d$Median)
  expect_lte(d$Median, d$CI_Upper)
})

test_that("perform_calculations returns one row per group", {
  res <- perform_calculations(make_mcmc_data(groups = c("A", "B")),
                              "t1", "v1", by = "By", tolerance_level = 90)
  expect_equal(nrow(res$data), 2)
  expect_setequal(res$data$By, c("A", "B"))
})

test_that("perform_calculations rejects non-data-frame input", {
  expect_error(perform_calculations(list(t1 = 1), "t1", "v1", by = "By", tolerance_level = 90))
})

test_that("bayesian_statistical_intervals produces the standard interval table", {
  res <- bayesian_statistical_intervals(make_mcmc_data(), "t1", "v1",
                                        by = "By", tolerance_interval_level = 90)

  expect_type(res, "list")
  expect_true(all(c("data", "summary") %in% names(res)))
  d <- res$data
  expect_true(all(interval_cols %in% names(d)))
  expect_lt(d$CI_Lower, d$CI_Upper)
  expect_lt(d$TI_Lower, d$TI_Upper)
})

test_that("the Krishnamoorthy option adds two-sided tolerance columns", {
  res <- suppressWarnings(
    bayesian_statistical_intervals(make_mcmc_data(), "t1", "v1",
                                   by = "By", tolerance_interval_level = 90,
                                   use_krishnamoorthy = TRUE)
  )
  expect_true(all(c("K_TI_Lower", "K_TI_Upper") %in% names(res$data)))
  expect_false(is.null(res$krishnamoorthy_results))
})

test_that("the log-normal option exponentiates the interval bounds", {
  res <- bayesian_statistical_intervals(make_mcmc_data(), "t1", "v1",
                                        by = "By", tolerance_interval_level = 90,
                                        log_normal = TRUE)
  # exp() of any real is strictly positive
  expect_true(res$data$Median > 0)
  expect_true(res$data$PI_Lower > 0)
})
