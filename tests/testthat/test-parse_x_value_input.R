test_that("single numeric values are parsed", {
  res <- parse_x_value_input("5.2")
  expect_equal(res$type, "single")
  expect_equal(res$value, 5.2)
})

test_that("negative and whitespace-padded single values are parsed", {
  expect_equal(parse_x_value_input("  -3 ")$value, -3)
})

test_that("ranges are parsed into xmin/xmax", {
  res <- parse_x_value_input("[2.1, 7.8]")
  expect_equal(res$type, "range")
  expect_equal(res$xmin, 2.1)
  expect_equal(res$xmax, 7.8)
})

test_that("reversed ranges are reordered so xmin <= xmax", {
  res <- parse_x_value_input("[7.8, 2.1]")
  expect_equal(res$xmin, 2.1)
  expect_equal(res$xmax, 7.8)
})

test_that("invalid, empty, NULL and NA inputs return NULL", {
  expect_null(suppressMessages(parse_x_value_input("")))
  expect_null(suppressMessages(parse_x_value_input("abc")))
  expect_null(suppressMessages(parse_x_value_input(NULL)))
  expect_null(suppressMessages(parse_x_value_input(NA_character_)))
})
