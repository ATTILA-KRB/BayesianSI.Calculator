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
