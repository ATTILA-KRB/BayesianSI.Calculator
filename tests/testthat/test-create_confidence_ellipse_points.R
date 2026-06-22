# create_confidence_ellipse_points() is an internal helper used by
# quantile_scatterplot(). It was previously called but never defined, which
# crashed the plot whenever ellipse_confidence > 0. These tests pin its shape
# and geometry.

ellipse <- BayesianStatisticalIntervalsCalculator:::create_confidence_ellipse_points

test_that("the helper returns an npoints x 2 matrix with x/y columns", {
  pts <- ellipse(cov_matrix = diag(2), centre = c(0, 0), level = 0.95, npoints = 100)
  expect_true(is.matrix(pts))
  expect_equal(dim(pts), c(100L, 2L))
  expect_equal(colnames(pts), c("x", "y"))
})

test_that("for an identity covariance the points lie on a circle of chi-sq radius", {
  centre <- c(0, 0)
  level <- 0.95
  pts <- ellipse(cov_matrix = diag(2), centre = centre, level = level, npoints = 200)

  radius <- sqrt(stats::qchisq(level, df = 2))
  distances <- sqrt((pts[, "x"] - centre[1])^2 + (pts[, "y"] - centre[2])^2)
  expect_equal(distances, rep(radius, nrow(pts)), tolerance = 1e-8)
})

test_that("the ellipse is centred on the supplied centre", {
  centre <- c(5, -3)
  pts <- ellipse(cov_matrix = diag(2), centre = centre, level = 0.9, npoints = 100)
  distances <- sqrt((pts[, "x"] - centre[1])^2 + (pts[, "y"] - centre[2])^2)
  radius <- sqrt(stats::qchisq(0.9, df = 2))
  expect_equal(distances, rep(radius, nrow(pts)), tolerance = 1e-8)
})
