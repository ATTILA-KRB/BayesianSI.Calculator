# Smoke tests for the plotting functions: they should build a plotly object
# from valid inputs without erroring. We do not assert on the visual content.

make_mcmc <- function(n = 150, seed = 11) {
  set.seed(seed)
  data.frame(
    t1 = rnorm(n, 10, 0.5),
    v1 = rgamma(n, shape = 2, rate = 2),
    By = "1",
    stringsAsFactors = FALSE
  )
}

mcmc      <- make_mcmc()
result    <- perform_calculations(mcmc, "t1", "v1", by = "By", tolerance_level = 95)
pred_dist <- calculate_predicted_distributions(mcmc, "t1", "v1", by = "By", tolerance_level = 95)
cdf_data  <- create_cdf_dataframe(pred_dist, by_value = "1")

test_that("plot_intervals returns a plotly object", {
  expect_s3_class(plot_intervals(result), "plotly")
  # show_reducible requires matching PI/TI percentages (both 95 here)
  expect_s3_class(plot_intervals(result, show_reducible = TRUE), "plotly")
})

test_that("plot_one_sided_intervals returns upper and lower plotly objects", {
  p <- plot_one_sided_intervals(result$data)
  expect_type(p, "list")
  expect_named(p, c("upper", "lower"))
  expect_s3_class(p$upper, "plotly")
  expect_s3_class(p$lower, "plotly")
})

test_that("plot_posterior_predictive returns a plotly object", {
  expect_s3_class(plot_posterior_predictive(cdf_data), "plotly")
  expect_s3_class(plot_posterior_predictive(cdf_data, x_value = "10"), "plotly")
})

test_that("quantile_scatterplot returns a plotly object, including with ellipses", {
  # ellipse_confidence > 0 exercises the create_confidence_ellipse_points()
  # helper that used to be called but never defined.
  p <- suppressWarnings(
    quantile_scatterplot(pred_dist = pred_dist, tolerance_level = 95, ellipse_confidence = 95)
  )
  expect_s3_class(p, "plotly")
})

test_that("plot_krishnamoorthy returns a plotly object", {
  gp  <- calculate_gP_mean(pred_dist, 95)
  opt <- suppressWarnings(find_optimal_point(pred_dist, gp, gamma_actual = 80, tolerance = 0.01))
  p <- plot_krishnamoorthy(pred_dist, by_value = "1", optimal_point = opt[["1"]],
                           tolerance_level = 95, confidence_level = 80)
  expect_s3_class(p, "plotly")
})
