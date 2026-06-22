test_that("calculate_ci returns the expected structure", {
  data <- data.frame(
    Group = rep("A", 100),
    Value = 1:100
  )
  res <- calculate_ci(data, "Value", 90, "Group")

  expect_s3_class(res, "data.frame")
  expect_true(all(c("By", "N", "CI_Lower", "CI_Upper", "Median") %in% names(res)))
  expect_equal(nrow(res), 1)
  expect_equal(res$N, 100)
})

test_that("calculate_ci produces an ordered interval around the median", {
  data <- data.frame(Group = rep("A", 100), Value = 1:100)
  res <- calculate_ci(data, "Value", 90, "Group")

  expect_equal(res$Median, median(1:100))
  expect_lt(res$CI_Lower, res$Median)
  expect_gt(res$CI_Upper, res$Median)
})

test_that("calculate_ci handles multiple groups", {
  data <- data.frame(
    Group = rep(c("A", "B"), each = 50),
    Value = c(1:50, 101:150)
  )
  res <- calculate_ci(data, "Value", 95, "Group")
  expect_equal(nrow(res), 2)
})

test_that("calculate_ci validates its inputs", {
  data <- data.frame(Group = rep("A", 10), Value = 1:10)
  expect_error(calculate_ci(data, "Missing", 90, "Group"))
  expect_error(calculate_ci(data, "Value", 150, "Group"))
  expect_error(calculate_ci(data, "Value", 0, "Group"))
})
