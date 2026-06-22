# End-to-end test of the bundled Shiny app via shinytest2 (headless Chrome).
#
# This is heavily guarded so it only runs where the pieces are available
# (e.g. CI with shinytest2 + Chrome installed) and skips cleanly everywhere
# else - including environments without internet/Chrome - rather than failing.

test_that("the bundled Shiny app boots and serves its UI", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  # Skip when no Chrome/Chromium is discoverable for the headless driver.
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  skip_if(is.null(chrome) || identical(chrome, ""), "No Chrome available for shinytest2")

  app_dir <- system.file("shiny", package = "BayesianStatisticalIntervalsCalculator")
  skip_if(app_dir == "", "Shiny app directory not found in the installed package")

  app <- shinytest2::AppDriver$new(
    app_dir,
    name = "bsi-app",
    load_timeout = 60 * 1000,
    timeout = 30 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  # Smoke assertion: the app rendered a non-empty page body.
  body_html <- app$get_html("body")
  expect_true(is.character(body_html) && nzchar(body_html))
})
