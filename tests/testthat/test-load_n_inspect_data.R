test_that("load_n_inspect_data returns the data passed directly", {
  df <- data.frame(a = c(1.5, 2.5), b = c("x", "y"), stringsAsFactors = FALSE)
  res <- suppressWarnings(suppressMessages(load_n_inspect_data(data = df, silent = TRUE)))
  expect_equal(as.data.frame(res), df)
})

test_that("load_n_inspect_data reads a CSV file from disk", {
  df <- data.frame(a = c(1.5, 2.5, 3.5), b = c(10, 20, 30))
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  utils::write.csv(df, tmp, row.names = FALSE)

  res <- suppressWarnings(suppressMessages(load_n_inspect_data(file_path = tmp, silent = TRUE)))
  res <- as.data.frame(res)
  expect_equal(dim(res), c(3L, 2L))
  expect_equal(res$a, df$a)
})

test_that("load_n_inspect_data errors when neither file_path nor data is provided", {
  expect_error(load_n_inspect_data())
})
