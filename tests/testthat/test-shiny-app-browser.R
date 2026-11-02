# Browser smoke test: opens the app in headless Chrome, visits every tab and
# checks that no output shows an error. The app runs in a separate R process
# that loads the *installed* r4lineups, so the test is opt-in: install the
# package, then set R4LINEUPS_TEST_APP=true.

test_that("every tab of the app renders without errors", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("R4LINEUPS_TEST_APP"), "true"),
              "Set R4LINEUPS_TEST_APP=true to run the browser smoke test.")
  skip_if_not_installed("shinytest2")
  skip_if(is.null(chromote::find_chrome()), "Chrome is not available.")

  app <- shinytest2::AppDriver$new(
    system.file("shiny/r4lineups_app", package = "r4lineups"),
    name = "smoke", load_timeout = 60000, timeout = 120000
  )
  on.exit(app$stop(), add = TRUE)

  errors <- function() {
    app$wait_for_idle(duration = 1000, timeout = 180000)
    app$get_js(paste0(
      "Array.from(document.querySelectorAll(",
      "'.shiny-output-error:not(.shiny-output-error-validation)'))",
      ".map(e => e.id + ': ' + e.textContent)"
    ))
  }

  tabs <- c("Home", "Data", "Bias & size", "Diagnosticity", "Multi-d'", "ROC & CAC",
            "Calibration & decision", "EIG & PPV", "Bayesian")
  for (tab in tabs) {
    app$set_inputs(nav = tab, wait_ = FALSE)
    expect_length(errors(), 0)
  }

  app$set_inputs(nav = "Data", wait_ = FALSE)
  app$set_inputs(`data-source` = "two_procedures", wait_ = FALSE)
  for (tab in c("ANRI", "DPP", "Expected utility", "Bayesian curves", "RAC")) {
    app$set_inputs(nav = "Calibration & decision", wait_ = FALSE)
    app$set_inputs(`calibration-tabs` = tab, wait_ = FALSE)
    expect_length(errors(), 0)
  }
  app$set_inputs(nav = "ROC & CAC", wait_ = FALSE)
  expect_length(errors(), 0)
})
