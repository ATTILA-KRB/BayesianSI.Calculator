test_that("calculate_predicted_distributions returns numeric columns and one row per observation", {
  data <- data.frame(
    f1  = c(0.50, 0.60, 0.40, 0.55),
    r1  = c(0.20, 0.25, 0.30, 0.22),
    grp = c("A", "A", "B", "B")
  )
  res <- calculate_predicted_distributions(data, "f1", "r1", by = "grp", tolerance_level = 90)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 4)
  for (col in c("mean", "median", "lower_quantile", "upper_quantile", "variance")) {
    expect_true(is.numeric(res[[col]]), info = col)
  }
})

test_that("infinite predictions stay numeric (regression: no character coercion)", {
  # An infinite fixed effect propagates to an infinite mean/median. Previously a
  # per-element sapply turned the whole column into character ("+Inf"/"-Inf");
  # the columns must remain numeric so downstream calculations keep working.
  data <- data.frame(
    f1  = c(Inf, Inf),
    r1  = c(0.20, 0.20),
    grp = c("A", "A")
  )
  res <- calculate_predicted_distributions(data, "f1", "r1", by = "grp", tolerance_level = 90)

  expect_true(is.numeric(res$median))
  expect_true(is.infinite(res$median[1]))
})
