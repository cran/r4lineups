test_that("pyWitness bridge completes an opt-in independent-model fit", {
  skip_if(
    !identical(Sys.getenv("R4LINEUPS_RUN_PYWITNESS"), "true"),
    "Set R4LINEUPS_RUN_PYWITNESS=true to run the Python parity smoke test"
  )

  envname <- Sys.getenv(
    "R4LINEUPS_PYWITNESS_ENV",
    unset = "r4lineups-pywitness"
  )

  dat <- simulate_lineup_data(
    n_tp = 120,
    n_ta = 120,
    lineup_size = 6,
    d_prime = 1.5,
    c_criterion = c(0.2, 0.8, 1.4),
    conf_levels = 3,
    decision_rule = "max",
    seed = 812
  )

  fit <- fit_lineup_models(
    dat,
    models = "independent",
    lineup_size = 6,
    variance = "equal",
    shared_variance = "zero",
    control = list(maxiter = 300L),
    envname = envname
  )

  expect_s3_class(fit, "lineup_model_comparison")
  expect_false(any(vapply(fit, reticulate::is_py_object, logical(1))))
  expect_identical(fit$engine$audited_revision, .pywitness_audited_revision)
  expect_true(fit$comparison$converged[[1]])
  expect_true(is.finite(fit$comparison$pearson_chisq[[1]]))
  expect_true(is.na(fit$comparison$AIC[[1]]))
  expect_true(is.na(fit$comparison$BIC[[1]]))
  expect_equal(sum(fit$observed$count[fit$observed$target_present]), 120)
  expect_equal(sum(fit$observed$count[!fit$observed$target_present]), 120)
})

test_that("BEST-Rest and Ensemble are scale-equivalent in the real engine", {
  skip_if(
    !identical(Sys.getenv("R4LINEUPS_RUN_PYWITNESS"), "true"),
    "Set R4LINEUPS_RUN_PYWITNESS=true to run Python parity tests"
  )
  envname <- Sys.getenv("R4LINEUPS_PYWITNESS_ENV",
                        unset = "r4lineups-pywitness")
  dat <- simulate_lineup_data(
    n_tp = 100, n_ta = 100, lineup_size = 6, d_prime = 1.4,
    c_criterion = c(0.8, 1.4, 2), conf_levels = 3,
    decision_rule = "ensemble", seed = 914
  )
  # Retain confidence level 3 in TP cells while making the TA-filler level-3
  # cell exactly zero, exercising sparse-cell handling in the upstream pivot.
  dat$confidence[
    !dat$target_present & dat$identification %in% c("suspect", "filler") &
      dat$confidence == 3
  ] <- 2
  fit <- suppressWarnings(fit_lineup_models(
    dat,
    models = c("ensemble", "best_rest", "integration"),
    lineup_size = 6,
    variance = "equal",
    shared_variance = "zero",
    control = list(maxiter = 500L),
    envname = envname
  ))

  expect_true(all(fit$comparison$converged))
  expect_equal(
    fit$comparison$pearson_chisq[fit$comparison$model == "ensemble"],
    fit$comparison$pearson_chisq[fit$comparison$model == "best_rest"],
    tolerance = 1e-5
  )
  ensemble_criteria <- subset(
    fit$parameters, model == "ensemble" & grepl("^c[0-9]+$", parameter)
  )$estimate
  best_rest_criteria <- subset(
    fit$parameters, model == "best_rest" & grepl("^c[0-9]+$", parameter)
  )$estimate
  expect_equal(best_rest_criteria, 6 / 5 * ensemble_criteria,
               tolerance = 2e-4)
  expect_lt(
    fit$comparison$pearson_chisq[fit$comparison$model == "ensemble"],
    fit$comparison$pearson_chisq[fit$comparison$model == "integration"]
  )

  partitions <- aggregate(
    expected ~ model + target_present, fit$cells, sum
  )
  expect_equal(partitions$expected, rep(100, nrow(partitions)),
               tolerance = 1e-6)
  expect_true(any(fit$cells$observed == 0))
})

test_that("bridge matches a direct pyWitness call and variance paths run", {
  skip_if(
    !identical(Sys.getenv("R4LINEUPS_RUN_PYWITNESS"), "true"),
    "Set R4LINEUPS_RUN_PYWITNESS=true to run Python parity tests"
  )
  envname <- Sys.getenv("R4LINEUPS_PYWITNESS_ENV",
                        unset = "r4lineups-pywitness")
  dat <- simulate_lineup_data(
    n_tp = 150, n_ta = 150, lineup_size = 6, d_prime = 1.5,
    c_criterion = c(0.8, 1.4, 2), conf_levels = 3,
    decision_rule = "max", seed = 190
  )
  wrapper <- fit_lineup_models(
    dat, models = "independent", lineup_size = 6,
    variance = "equal", shared_variance = "zero",
    control = list(maxiter = 300L), envname = envname
  )
  unequal <- fit_lineup_models(
    dat, models = "independent", lineup_size = 6,
    variance = "unequal", shared_variance = "zero",
    control = list(maxiter = 500L), envname = envname
  )
  correlated <- fit_lineup_models(
    dat, models = "independent", lineup_size = 6,
    variance = "equal", shared_variance = "estimated",
    control = list(maxiter = 500L), envname = envname
  )
  expect_true(all(c(wrapper$comparison$converged,
                    unequal$comparison$converged,
                    correlated$comparison$converged)))
  target_mean <- subset(
    wrapper$parameters, parameter == "targetMean"
  )$estimate
  expect_equal(target_mean, 1.5, tolerance = 0.4)
  expect_gt(subset(
    correlated$parameters, parameter == "targetBetweenSigma"
  )$estimate, 0)

  prepared <- r4lineups:::.prepare_lineup_model_data(
    dat, lineup_size = 6, confidence_bins = NULL
  )
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(
    r4lineups:::.pywitness_data_frame(prepared), path, row.names = FALSE
  )
  pyw <- reticulate::import("pyWitness", convert = TRUE)
  raw <- NULL
  reticulate::py_capture_output(raw <- pyw$DataRaw(path), type = "stdout")
  processed <- raw$process()
  direct <- pyw$ModelFitIndependentObservationSimple(
    processed, integrationSigma = 8, chi2Var = "expected"
  )
  adapter <- new.env(parent = baseenv())
  reticulate::source_python(system.file("python", "chi_square_adapter.py",
                                       package = "r4lineups"), envir = adapter)
  adapter$correct_chi_square_objective(direct)
  direct$setEqualVariance()
  direct$targetBetweenSigma$value <- 0
  direct$targetBetweenSigma$fixed <- TRUE
  reticulate::py_capture_output(
    direct$fit(maxiter = 300L, method = "Nelder-Mead"), type = "stdout"
  )
  expect_equal(wrapper$comparison$pearson_chisq,
               as.numeric(direct$chi2), tolerance = 1e-8)
  cells <- wrapper$cells
  expect_equal(wrapper$comparison$pearson_chisq,
               sum((cells$observed - cells$expected)^2 / cells$expected),
               tolerance = 1e-8)

  # objective = "pywitness" leaves the engine untouched and reproduces it.
  uncorrected <- fit_lineup_models(
    dat, models = "independent", lineup_size = 6,
    variance = "equal", shared_variance = "zero",
    control = list(maxiter = 300L, objective = "pywitness"), envname = envname
  )
  plain <- pyw$ModelFitIndependentObservationSimple(
    processed, integrationSigma = 8, chi2Var = "expected"
  )
  plain$setEqualVariance()
  plain$targetBetweenSigma$value <- 0
  plain$targetBetweenSigma$fixed <- TRUE
  reticulate::py_capture_output(
    plain$fit(maxiter = 300L, method = "Nelder-Mead"), type = "stdout"
  )
  expect_equal(uncorrected$comparison$pearson_chisq,
               as.numeric(plain$chi2), tolerance = 1e-8)
  expect_match(uncorrected$specification$estimator, "uncorrected")

  expect_warning(
    failed <- fit_lineup_models(
      dat, models = "independent", lineup_size = 6,
      variance = "equal", shared_variance = "zero",
      control = list(maxiter = 1L), envname = envname
    ),
    "did not converge"
  )
  expect_false(failed$comparison$converged)
})
