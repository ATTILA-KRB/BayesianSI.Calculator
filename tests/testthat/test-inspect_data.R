test_that("inspect_data returns an empty list for clean data", {
  res <- inspect_data(data.frame(x = c(1.5, 2.5, 3.5), y = c(4.5, 5.5, 6.5)))
  expect_type(res, "list")
  expect_length(res, 0)
})

test_that("inspect_data reports NA/NaN values under na_warnings", {
  res <- inspect_data(data.frame(x = c(1, NA, 3), y = c(1.5, 2.5, 3.5)))
  expect_true("na_warnings" %in% names(res))
  expect_equal(unname(res$na_warnings[["x"]]),
               "Column 'x' contains 1 NA/NaN values.")
})

test_that("inspect_data reports integer-only columns under integer_warnings", {
  res <- inspect_data(data.frame(x = c(1L, 2L, 3L), y = c(1.5, 2.5, 3.5)))
  expect_true("integer_warnings" %in% names(res))
  expect_true(any(res$integer_warnings == "Column 'x' contains only integer values."))
})

test_that("inspect_data reports zero-only columns under zero_warnings", {
  res <- inspect_data(data.frame(z = c(0, 0, 0), y = c(1.5, 2.5, 3.5)))
  expect_true("zero_warnings" %in% names(res))
  expect_true(any(res$zero_warnings == "Column 'z' contains only zero values."))
})

test_that("inspect_data reports list columns under list_warnings", {
  df <- data.frame(y = c(1.5, 2.5))
  df$lc <- list(c(1, 2), c(3, 4))
  res <- inspect_data(df)
  expect_true("list_warnings" %in% names(res))
  expect_true(any(grepl("contains list data", res$list_warnings)))
})

test_that("inspect_data returns a named list of character messages", {
  res <- inspect_data(data.frame(x = c(1, NA, 3), y = 1:3))
  expect_type(res, "list")
  expect_true(all(vapply(res, is.character, logical(1))))
  expect_true(!is.null(names(res)))
})
