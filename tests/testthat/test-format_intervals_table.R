# Build a real result to format.
make_result <- function() {
  set.seed(7)
  data <- data.frame(
    t1 = rnorm(150, 10, 0.5),
    v1 = rgamma(150, shape = 2, rate = 2),
    By = "1"
  )
  perform_calculations(data, "t1", "v1", by = "By", tolerance_level = 90)
}

test_that("format_intervals_table returns a DT datatable widget", {
  # Regression: previously used an unqualified tagList and errored at runtime.
  dt <- format_intervals_table(make_result())
  expect_s3_class(dt, "datatables")
})

test_that("format_intervals_table works with reducible columns shown", {
  dt <- format_intervals_table(make_result(), show_reducible = TRUE)
  expect_s3_class(dt, "datatables")
})

# Helper: render the datatable container header HTML to a single string so we
# can assert on the header text produced by the formatter.
header_html <- function(dt) {
  paste(as.character(dt$x$container), collapse = " ")
}

test_that("explicit percentages produce expected (unprefixed) headers", {
  dt <- format_intervals_table(
    make_result(),
    ci_percent = 95, pi_percent = 90, ti_percent = 99, conf_percent = 95
  )
  html <- header_html(dt)
  expect_match(html, "95% CI", fixed = TRUE)
  expect_match(html, "90% PI", fixed = TRUE)
  expect_match(html, "99% TI (95% Credibility)", fixed = TRUE)
})

test_that("is_one_sided = TRUE yields One-Sided headers and keeps PI", {
  dt <- format_intervals_table(
    make_result(),
    ci_percent = 95, pi_percent = 90, ti_percent = 99, conf_percent = 95,
    is_one_sided = TRUE
  )
  html <- header_html(dt)
  expect_match(html, "One-Sided 95% CI", fixed = TRUE)
  expect_match(html, "One-Sided 90% PI", fixed = TRUE)
  expect_true("PI_Lower" %in% names(dt$x$data))
})

test_that("is_one_sided = FALSE yields Two-Sided headers and drops PI columns", {
  dt <- format_intervals_table(
    make_result(),
    ci_percent = 95, pi_percent = 90, ti_percent = 99, conf_percent = 95,
    is_one_sided = FALSE
  )
  html <- header_html(dt)
  expect_match(html, "Two-Sided 95% CI", fixed = TRUE)
  expect_false(grepl("PI", html))
  expect_false("PI_Lower" %in% names(dt$x$data))
})

test_that("by_label appears in the header", {
  res <- make_result()
  # Force a multi-group result so the By header is not blanked.
  res$data$By <- rep(c("g1", "g2"), length.out = nrow(res$data))
  dt <- format_intervals_table(res, ci_percent = 95, pi_percent = 90,
                               ti_percent = 99, conf_percent = 95,
                               by_label = "A, B")
  expect_match(header_html(dt), "A, B", fixed = TRUE)
})

test_that("build_interval_headers builds prefixed and unprefixed labels", {
  build_interval_headers <- getFromNamespace(
    "build_interval_headers", "BayesianStatisticalIntervalsCalculator"
  )
  none <- build_interval_headers(95, 90, 99, 95, side = NULL)
  expect_equal(none$ci, "95% CI")
  expect_equal(none$pi, "90% PI")
  expect_equal(none$ti, "99% TI (95% Credibility)")

  one <- build_interval_headers(95, 90, 99, 95, side = TRUE)
  expect_equal(one$ci, "One-Sided 95% CI")

  two <- build_interval_headers(95, 90, 99, 95, side = FALSE)
  expect_equal(two$ci, "Two-Sided 95% CI")
})
