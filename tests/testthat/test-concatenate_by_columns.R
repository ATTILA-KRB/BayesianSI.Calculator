test_that("concatenate_by_columns joins the requested columns with underscores", {
  data <- data.frame(a = c(1, 2), b = c("x", "y"), stringsAsFactors = FALSE)
  res <- concatenate_by_columns(data, by_columns = c("a", "b"))

  expect_true("concatenated_by_column" %in% names(res))
  expect_equal(res$concatenated_by_column, c("1_x", "2_y"))
})

test_that("the new column name is configurable", {
  data <- data.frame(a = 1, b = 2)
  res <- concatenate_by_columns(data, by_columns = c("a", "b"), new_column_name = "grp")
  expect_equal(res$grp, "1_2")
})

test_that("a NULL by_columns returns the data unchanged", {
  data <- data.frame(a = 1:3, b = 4:6)
  expect_identical(concatenate_by_columns(data, by_columns = NULL), data)
})

test_that("missing columns and non-data-frame input raise errors", {
  data <- data.frame(a = 1)
  expect_error(concatenate_by_columns(data, by_columns = "nope"))
  expect_error(concatenate_by_columns(list(a = 1), by_columns = "a"))
})
