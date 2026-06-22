test_that("reducible_fraction computes (ti - pi) / (ti - median)", {
  expect_equal(reducible_fraction(10, 8, 4), (10 - 8) / (10 - 4))
})

test_that("reducible_fraction returns NA when ti_bound == median", {
  expect_true(is.na(reducible_fraction(5, 5, 5)))
  expect_true(is.na(reducible_fraction(5, 3, 5)))
})

test_that("reducible_fraction is vectorised over its arguments", {
  result <- reducible_fraction(c(10, 5), c(8, 5), c(4, 5))
  expect_length(result, 2)
  expect_equal(result[1], (10 - 8) / (10 - 4))
  expect_true(is.na(result[2]))
})
