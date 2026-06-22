# create_confidence_ellipse_points() is an exported helper used by
# quantile_scatterplot() and reused by the Shiny app. These tests pin its
# shape, geometry, input validation, and lenient handling of degenerate
# covariance matrices.

ellipse <- BayesianStatisticalIntervalsCalculator::create_confidence_ellipse_points

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

test_that("input validation rejects malformed arguments", {
  expect_error(
    ellipse(cov_matrix = diag(3), centre = c(0, 0)),
    "'cov_matrix' must be a 2x2 matrix"
  )
  expect_error(
    ellipse(cov_matrix = c(1, 0, 0, 1), centre = c(0, 0)),
    "'cov_matrix' must be a 2x2 matrix"
  )
  expect_error(
    ellipse(cov_matrix = diag(2), centre = c(0, 0, 0)),
    "'centre' must be a numeric vector of length 2"
  )
  expect_error(
    ellipse(cov_matrix = diag(2), centre = "a"),
    "'centre' must be a numeric vector of length 2"
  )
  expect_error(
    ellipse(cov_matrix = diag(2), centre = c(0, 0), level = 0),
    "'level' must be a number between 0 and 1"
  )
  expect_error(
    ellipse(cov_matrix = diag(2), centre = c(0, 0), level = 1),
    "'level' must be a number between 0 and 1"
  )
  expect_error(
    ellipse(cov_matrix = diag(2), centre = c(0, 0), level = c(0.5, 0.6)),
    "'level' must be a number between 0 and 1"
  )
  expect_error(
    ellipse(cov_matrix = diag(2), centre = c(0, 0), npoints = 3),
    "'npoints' must be a number >= 4"
  )
  expect_error(
    ellipse(cov_matrix = diag(2), centre = c(0, 0), npoints = "x"),
    "'npoints' must be a number >= 4"
  )
})

test_that("a non-positive-definite covariance returns a result without error", {
  # This matrix has a negative eigenvalue; the helper clamps it to zero rather
  # than erroring, so the plot degrades gracefully on degenerate covariance.
  non_pd <- matrix(c(1, 2, 2, 1), 2)
  expect_false(all(eigen(non_pd, symmetric = TRUE)$values > 0))

  pts <- ellipse(cov_matrix = non_pd, centre = c(0, 0), level = 0.95, npoints = 50)
  expect_true(is.matrix(pts))
  expect_equal(dim(pts), c(50L, 2L))
  expect_equal(colnames(pts), c("x", "y"))
  expect_true(all(is.finite(pts)))
})
