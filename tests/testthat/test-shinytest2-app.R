# End-to-end tests of the bundled Shiny app via shinytest2 (headless Chrome).
#
# These are heavily guarded so they only run where the pieces are available
# (e.g. CI with shinytest2 + Chrome installed) and skip cleanly everywhere
# else - including environments without internet/Chrome - rather than failing.

# Shared guard: skip unless shinytest2, chromote and a Chrome binary are all
# available, and return the installed app directory.
skip_unless_shinytest2_ready <- function() {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  skip_if(is.null(chrome) || identical(chrome, ""), "No Chrome available for shinytest2")
  app_dir <- system.file("shiny", package = "BayesianStatisticalIntervalsCalculator")
  skip_if(app_dir == "", "Shiny app directory not found in the installed package")
  app_dir
}

test_that("the bundled Shiny app boots and serves its UI", {
  app_dir <- skip_unless_shinytest2_ready()

  app <- shinytest2::AppDriver$new(
    app_dir,
    name = "bsi-app-boot",
    load_timeout = 60 * 1000,
    timeout = 30 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  # Smoke assertion: the app rendered a non-empty page body.
  body_html <- app$get_html("body")
  expect_true(is.character(body_html) && nzchar(body_html))
})

test_that("the app's calculate pipeline produces interval results end to end", {
  app_dir <- skip_unless_shinytest2_ready()

  # Toy MCMC-style data (mirrors the README example): two groups via EGTPTNUM,
  # a fixed effect (b2) and a variance component (s2).
  toy <- data.frame(
    EGTPTNUM = c(60, 60, 60, 60, 60, 90, 90, 90, 90, 90),
    s2 = c(74.18, 58.92, 75.04, 64.75, 67.81, 59.75, 51.91, 57.58, 64.76, 52.09),
    b2 = c(-5.23, -3.97, -2.92, -7.64, -2.57, 0.61, -2.02, -0.41, 0.08, 2.43),
    b4 = c(1.32, 3.68, 3.39, 3.13, 3.50, 5.57, 4.90, 1.71, 3.58, 3.36)
  )
  csv_path <- tempfile(fileext = ".csv")
  utils::write.csv(toy, csv_path, row.names = FALSE)
  on.exit(unlink(csv_path), add = TRUE)

  app <- shinytest2::AppDriver$new(
    app_dir,
    name = "bsi-app-pipeline",
    load_timeout = 60 * 1000,
    timeout = 45 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  # 1. Upload the CSV and load it (populates the variable selectors).
  app$upload_file(file = csv_path)
  app$click("load_data")
  app$wait_for_idle(timeout = 30 * 1000)

  # 2. Choose the model: fixed effect, variance component, grouping column.
  app$set_inputs(
    fixed_var = "b2",
    random_var = "s2",
    by = "EGTPTNUM"
  )
  app$wait_for_idle(timeout = 15 * 1000)

  # 3. Run the calculation (this exercises the de-duplicated reactive pipeline:
  #    performCalculations -> perform_calculations -> CI/TI/PI + reducible).
  app$click("calculate_ci")
  app$wait_for_idle(timeout = 45 * 1000)

  # 4. Assert via the test-mode exports that the pipeline produced results.
  expect_true(isTRUE(app$get_value(export = "ci_calculated")))

  n_rows <- app$get_value(export = "intervals_nrow")
  expect_gt(n_rows, 0)
  # Two groups in the toy data -> two rows of intervals.
  expect_equal(n_rows, 2)

  cols <- app$get_value(export = "intervals_cols")
  # The CI/TI/PI columns prove the calculate_ci/tolerance/PI delegation ran;
  # the Reducible columns prove the reducible_fraction delegation ran.
  expect_true(all(c("CI_Lower", "CI_Upper",
                    "TI_Lower", "TI_Upper",
                    "PI_Lower", "PI_Upper",
                    "ReducibleLower", "ReducibleUpper") %in% cols))

  # 5. The one-sided intervals table (render_intervals_table ->
  #    format_intervals_table) actually rendered something non-empty.
  ci_html <- app$get_html("#ci_output")
  expect_true(is.character(ci_html) && nzchar(ci_html))
})
