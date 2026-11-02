model_test_data <- function() {
  data.frame(
    target_present = rep(c(TRUE, FALSE), each = 9),
    identification = c(
      "suspect", "suspect", "suspect", "filler", "filler", "filler",
      "reject", "reject", "reject",
      "suspect", "filler", "filler", "filler", "filler", "filler",
      "reject", "reject", "reject"
    ),
    confidence = rep(c(1, 2, 3), 6),
    stringsAsFactors = FALSE
  )
}

test_that("pyWitness dependency check is non-initializing by default", {
  result <- check_pywitness_deps(
    envname = "r4lineups-test-environment-that-does-not-exist",
    initialize = FALSE,
    verbose = FALSE
  )
  expect_s3_class(result, "tbl_df")
  expect_false(result$environment_exists)
  expect_equal(result$audited_revision,
               "e726dcfc09423d0e0ff7f46c8e3a711040293eba")
  expect_true(is.na(result$audited_revision_matches))
})

test_that("lineup model data preparation preserves a multinomial partition", {
  prepared <- r4lineups:::.prepare_lineup_model_data(
    model_test_data(), lineup_size = 6, confidence_bins = NULL
  )
  cells <- r4lineups:::.lineup_model_cells(prepared)

  expect_equal(prepared$ta_suspect_collapsed, 1)
  expect_equal(sum(cells$observed[cells$target_present]), prepared$n_tp)
  expect_equal(sum(cells$observed[!cells$target_present]), prepared$n_ta)
  expect_equal(prepared$confidence_levels, 1:3)
})

test_that("confidence bins are ordered, covering, and explicit", {
  data <- model_test_data()
  data$confidence <- rep(c(10, 50, 90), 6)
  prepared <- r4lineups:::.prepare_lineup_model_data(
    data, lineup_size = 6, confidence_bins = c(0, 40, 70, 100)
  )
  expect_equal(prepared$data$confidence, rep(1:3, 6))
  expect_equal(prepared$confidence_bins, c(0, 40, 70, 100))

  expect_error(
    r4lineups:::.prepare_lineup_model_data(
      data, lineup_size = 6, confidence_bins = c(20, 70, 100)
    ),
    "cover every"
  )
  expect_error(
    r4lineups:::.prepare_lineup_model_data(
      data, lineup_size = 6, confidence_bins = c(0, 70, 40, 100)
    ),
    "strictly increasing"
  )
  expect_error(
    r4lineups:::.prepare_lineup_model_data(
      data, lineup_size = 6, confidence_bins = c(0, 20, 40, 70, 100)
    ),
    "entirely empty"
  )
})

test_that("required lineup model columns and values are validated", {
  data <- model_test_data()
  expect_error(
    r4lineups:::.prepare_lineup_model_data(
      data[, -3], lineup_size = 6, confidence_bins = NULL
    ),
    "missing required"
  )
  invalid <- data
  invalid$identification[1] <- "other"
  expect_error(
    r4lineups:::.prepare_lineup_model_data(
      invalid, lineup_size = 6, confidence_bins = NULL
    ),
    "suspect, filler, or reject"
  )
  invalid <- data
  invalid$confidence[1] <- NA_real_
  expect_error(
    r4lineups:::.prepare_lineup_model_data(
      invalid, lineup_size = 6, confidence_bins = NULL
    ),
    "complete, finite"
  )

  invalid <- data[data$identification != "reject", ]
  expect_error(
    r4lineups:::.prepare_lineup_model_data(
      invalid, lineup_size = 6, confidence_bins = NULL
    ),
    "aggregate response category.*reject"
  )

  invalid <- data
  invalid$condition <- rep(c("A", "B"), each = nrow(invalid) / 2)
  expect_error(
    r4lineups:::.prepare_lineup_model_data(
      invalid, lineup_size = 6, confidence_bins = NULL
    ),
    "one condition per call"
  )
})

test_that("multinomial log likelihood is independently reproducible", {
  cells <- data.frame(
    target_present = c(FALSE, FALSE, TRUE, TRUE, TRUE),
    observed = c(3, 7, 5, 2, 3),
    expected = c(4, 6, 4, 3, 3)
  )
  ta <- lfactorial(10) - lfactorial(3) - lfactorial(7) +
    3 * log(0.4) + 7 * log(0.6)
  tp <- lfactorial(10) - lfactorial(5) - lfactorial(2) - lfactorial(3) +
    5 * log(0.4) + 2 * log(0.3) + 3 * log(0.3)
  expect_equal(
    r4lineups:::.loglik_at_expected(cells),
    ta + tp,
    tolerance = 1e-12
  )
})

test_that("model aliases and control values are validated", {
  expect_true(r4lineups:::.pywitness_revision_matches(
    "0.1.dev1+ge726dcfc0"
  ))
  expect_false(r4lineups:::.pywitness_revision_matches("0.1.0"))
  expect_equal(r4lineups:::.lineup_model_class("independent"),
               "ModelFitIndependentObservation")
  expect_equal(r4lineups:::.lineup_model_class("independent", "zero"),
               "ModelFitIndependentObservationSimple")
  expect_equal(r4lineups:::.lineup_model_class("ensemble"),
               "ModelFitEnsemble")
  expect_error(
    r4lineups:::.validate_lineup_model_control(list(maxiter = 0)),
    "positive integer"
  )
  expect_error(
    r4lineups:::.validate_lineup_model_control(list(unknown = 1)),
    "containing only"
  )
  expect_error(
    r4lineups:::.validate_lineup_model_starts(
      c(c1 = 1, c2 = 0.5), "independent"
    ),
    "strictly increasing"
  )
  expect_error(
    r4lineups:::.validate_lineup_model_starts(
      list(ensemble = c(targetMean = 1)), "independent"
    ),
    "not being fitted"
  )
})

test_that("fit_lineup_models gives a clear missing-environment error", {
  expect_error(
    fit_lineup_models(
      model_test_data(),
      models = "independent",
      envname = "r4lineups-test-environment-that-does-not-exist"
    ),
    "Run install_pywitness"
  )
})

test_that("R-native lineup comparison methods work without Python", {
  comparison <- data.frame(
    model = c("independent", "ensemble"),
    converged = c(TRUE, TRUE),
    status = c("success", "success"),
    iterations = c(10L, 12L),
    n_parameters = c(5L, 4L),
    pearson_chisq = c(3, 2),
    df = c(4L, 5L),
    p_value = c(0.56, 0.85),
    loglik_at_estimate = c(-20, -19),
    AIC = NA_real_,
    BIC = NA_real_,
    error = NA_character_
  )
  object <- list(
    comparison = comparison,
    parameters = data.frame(
      model = "independent", parameter = "targetMean", estimate = 1.5,
      fixed = FALSE, linked_to = NA_character_
    ),
    cells = NULL,
    diagnostics = comparison[, c("model", "converged", "status",
                                  "iterations", "error")],
    specification = list(
      estimator = "minimum Pearson chi-squared", lineup_size = 6,
      n_tp = 100, n_ta = 100, best_rest_ensemble_duplicate = FALSE
    ),
    engine = list(name = "pyWitness", version = "test"),
    information_criteria = list(
      reason = "not a maximum-likelihood estimate"
    )
  )
  class(object) <- "lineup_model_comparison"

  expect_output(print(object), "AIC/BIC unavailable")
  expect_output(summary(object), "Parameter estimates")
  expect_s3_class(plot(object, type = "comparison"), "ggplot")
})
