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

test_that("perform_calculations forwards covariates and returns predicted distributions", {
  d <- make_mcmc_data()
  set.seed(7)
  d$cov1 <- runif(nrow(d), 0.5, 2)

  res <- perform_calculations(d, "t1", "v1", by = "By", tolerance_level = 90,
                              covariate_cols = "cov1", covariate_values = 1.5)

  # The raw predicted distributions are now returned (additive field)
  expect_true("predicted_distributions" %in% names(res))
  expect_false(is.null(res$predicted_distributions))
  expect_true(is.data.frame(res$predicted_distributions))

  # Result table is well-formed with reducible-uncertainty columns
  expect_true(all(interval_cols %in% names(res$data)))
  expect_true(all(c("ReducibleUpper", "ReducibleLower") %in% names(res$data)))

  # Equivalent to running the predicted-distributions + interval pipeline manually
  manual_pred <- calculate_predicted_distributions(
    data = d, fixed_effects = "t1", random_params = "v1", by = "By",
    tolerance_level = 90, multiplication_factor = 1,
    covariate_cols = "cov1", covariate_values = 1.5
  )
  expect_equal(res$predicted_distributions, manual_pred)

  ci_result <- calculate_ci(manual_pred, "median", 95, "By")
  ti_result <- calculate_tolerance_interval(manual_pred, "lower_quantile",
                                            "upper_quantile", 95, "By")
  pi_result <- calculate_PI(data = manual_pred, percent_for_pi = 95, Eta = 0.001)
  manual <- merge(merge(ci_result, ti_result, by = "By"), pi_result, by = "By")
  manual$ReducibleUpper <- round(reducible_fraction(manual$TI_Upper, manual$PI_Upper, manual$Median) * 100, 3)
  manual$ReducibleLower <- round(reducible_fraction(manual$TI_Lower, manual$PI_Lower, manual$Median) * 100, 3)

  expect_equal(as.data.frame(res$data), as.data.frame(manual))
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
