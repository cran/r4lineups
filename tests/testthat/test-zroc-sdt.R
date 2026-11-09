# Tests for z-ROC and SDT parameter estimation functions

# Helper function to create test data
create_test_sdt_data <- function(n_tp = 100, n_ta = 100, d_prime = 1.5, seed = 123) {
  set.seed(seed)
  simulate_lineup_data(
    n_tp = n_tp,
    n_ta = n_ta,
    d_prime = d_prime,
    conf_levels = 5,
    lineup_size = 6
  )
}


test_that("fit_sdt_roc basic functionality", {
  test_data <- create_test_sdt_data()

  sdt_fit <- fit_sdt_roc(
    test_data,
    lineup_size = 6,
    bootstrap = FALSE
  )

  expect_s3_class(sdt_fit, "sdt_roc_fit")
  expect_true("dprime" %in% names(sdt_fit))
  expect_true("criteria" %in% names(sdt_fit))
  expect_true("slope" %in% names(sdt_fit))
  expect_true("intercept" %in% names(sdt_fit))
})


test_that("fit_sdt_roc returns valid parameters", {
  test_data <- create_test_sdt_data(d_prime = 2.0)

  sdt_fit <- fit_sdt_roc(test_data, lineup_size = 6, bootstrap = FALSE)

  # d' should be positive
  expect_true(sdt_fit$dprime > 0)

  # For equal variance, slope should be 1
  expect_equal(sdt_fit$slope, 1.0)

  # R-squared should be between 0 and 1
  expect_true(sdt_fit$fit_diagnostics$r_squared >= 0)
  expect_true(sdt_fit$fit_diagnostics$r_squared <= 1)

  # Should have criteria for each confidence level
  expect_true(length(sdt_fit$criteria) > 0)
})


test_that("fit_sdt_roc parameter recovery", {
  # Test if we can recover known d'
  # Note: Simulated data uses MAX rule, not pure SDT, so perfect recovery
  # is not expected. This test just checks that higher d' leads to higher estimates.

  low_data <- create_test_sdt_data(n_tp = 200, n_ta = 200, d_prime = 1.0, seed = 111)
  high_data <- create_test_sdt_data(n_tp = 200, n_ta = 200, d_prime = 2.0, seed = 222)

  sdt_low <- fit_sdt_roc(low_data, lineup_size = 6, bootstrap = FALSE)
  sdt_high <- fit_sdt_roc(high_data, lineup_size = 6, bootstrap = FALSE)

  # Higher true d' should lead to higher estimated d'
  expect_true(sdt_high$dprime > sdt_low$dprime)

  # Both should be positive
  expect_true(sdt_low$dprime > 0)
  expect_true(sdt_high$dprime > 0)
})


test_that("fit_sdt_roc equal vs unequal variance", {
  test_data <- create_test_sdt_data()

  sdt_equal <- fit_sdt_roc(
    test_data,
    lineup_size = 6,
    model = "equal_variance",
    bootstrap = FALSE
  )

  sdt_unequal <- fit_sdt_roc(
    test_data,
    lineup_size = 6,
    model = "unequal_variance",
    bootstrap = FALSE
  )

  # Both should return valid d'
  expect_true(sdt_equal$dprime > 0)
  expect_true(sdt_unequal$dprime > 0)

  # Equal variance slope should be exactly 1
  expect_equal(sdt_equal$slope, 1.0)

  # Unequal variance should have variance_ratio
  expect_true("variance_ratio" %in% names(sdt_unequal))
  expect_true(sdt_unequal$variance_ratio > 0)

  # Equal variance model has variance_ratio = 1
  expect_equal(sdt_equal$variance_ratio, 1.0)
})


test_that("fit_sdt_roc with bootstrap", {
  skip_on_cran()
  test_data <- create_test_sdt_data()

  sdt_boot <- fit_sdt_roc(
    test_data,
    lineup_size = 6,
    bootstrap = TRUE,
    n_bootstrap = 100,  # Small for testing speed
    seed = 789
  )

  expect_true("bootstrap_ci" %in% names(sdt_boot))
  expect_true("dprime_ci" %in% names(sdt_boot$bootstrap_ci))
  expect_true("dprime_se" %in% names(sdt_boot$bootstrap_ci))

  # CI should have two values
  expect_length(sdt_boot$bootstrap_ci$dprime_ci, 2)

  # Lower CI should be less than upper CI
  expect_true(sdt_boot$bootstrap_ci$dprime_ci[1] < sdt_boot$bootstrap_ci$dprime_ci[2])

  # Standard error should be positive
  expect_true(sdt_boot$bootstrap_ci$dprime_se > 0)
})


test_that("fit_sdt_roc with ROC data object", {
  test_data <- create_test_sdt_data()

  # Create ROC data first
  roc_data <- make_rocdata(test_data, lineup_size = 6)

  # Fit SDT model to ROC data
  sdt_fit <- fit_sdt_roc(roc_data, bootstrap = FALSE)

  expect_s3_class(sdt_fit, "sdt_roc_fit")
  expect_true(sdt_fit$dprime > 0)
})


test_that("print.sdt_roc_fit works", {
  test_data <- create_test_sdt_data()
  sdt_fit <- fit_sdt_roc(test_data, lineup_size = 6, bootstrap = FALSE)

  expect_output(print(sdt_fit), "SDT Model Fit")
  expect_output(print(sdt_fit), "d'")
  expect_output(print(sdt_fit), "Slope")
})


test_that("summary.sdt_roc_fit works", {
  test_data <- create_test_sdt_data()
  sdt_fit <- fit_sdt_roc(test_data, lineup_size = 6, bootstrap = FALSE)

  expect_output(summary(sdt_fit), "SDT Model Summary")
  expect_output(summary(sdt_fit), "Parameter Estimates")
  expect_output(summary(sdt_fit), "Model Fit")
})


test_that("plot.sdt_roc_fit works", {
  test_data <- create_test_sdt_data()
  sdt_fit <- fit_sdt_roc(test_data, lineup_size = 6, bootstrap = FALSE)

  # Should create a plot without error
  expect_silent(plot(sdt_fit))
})


test_that("fit_sdt_roc handles different sample sizes", {
  # Small sample
  small_data <- create_test_sdt_data(n_tp = 30, n_ta = 30)
  sdt_small <- fit_sdt_roc(small_data, lineup_size = 6, bootstrap = FALSE)

  # Large sample
  large_data <- create_test_sdt_data(n_tp = 300, n_ta = 300, seed = 999)
  sdt_large <- fit_sdt_roc(large_data, lineup_size = 6, bootstrap = FALSE)

  # Both should produce valid results
  expect_true(sdt_small$dprime > 0)
  expect_true(sdt_large$dprime > 0)

  # Larger sample should generally have better fit (higher R²)
  # But this is not always guaranteed, so we just check both are valid
  expect_true(sdt_small$fit_diagnostics$r_squared >= 0)
  expect_true(sdt_large$fit_diagnostics$r_squared >= 0)
})


test_that("fit_sdt_roc different d' values", {
  # Test with weak discriminability
  weak_data <- create_test_sdt_data(d_prime = 0.8, seed = 111)
  sdt_weak <- fit_sdt_roc(weak_data, lineup_size = 6, bootstrap = FALSE)

  # Test with strong discriminability
  strong_data <- create_test_sdt_data(d_prime = 2.5, seed = 222)
  sdt_strong <- fit_sdt_roc(strong_data, lineup_size = 6, bootstrap = FALSE)

  # Strong should have higher estimated d'
  expect_true(sdt_strong$dprime > sdt_weak$dprime)

  # Both should be positive
  expect_true(sdt_weak$dprime > 0)
  expect_true(sdt_strong$dprime > 0)
})


test_that("fit_sdt_roc confidence levels", {
  skip_on_cran()
  test_data <- create_test_sdt_data()

  # Fit with default confidence level
  sdt_95 <- fit_sdt_roc(
    test_data,
    lineup_size = 6,
    bootstrap = TRUE,
    n_bootstrap = 100,
    conf_level = 0.95,
    seed = 333
  )

  # Fit with different confidence level
  sdt_99 <- fit_sdt_roc(
    test_data,
    lineup_size = 6,
    bootstrap = TRUE,
    n_bootstrap = 100,
    conf_level = 0.99,
    seed = 333  # Same seed for comparison
  )

  # 99% CI should be wider than 95% CI
  width_95 <- diff(sdt_95$bootstrap_ci$dprime_ci)
  width_99 <- diff(sdt_99$bootstrap_ci$dprime_ci)

  expect_true(width_99 > width_95)
})


test_that("fit_sdt_roc seed reproducibility", {
  skip_on_cran()
  test_data <- create_test_sdt_data()

  sdt1 <- fit_sdt_roc(
    test_data,
    lineup_size = 6,
    bootstrap = TRUE,
    n_bootstrap = 100,
    seed = 444
  )

  sdt2 <- fit_sdt_roc(
    test_data,
    lineup_size = 6,
    bootstrap = TRUE,
    n_bootstrap = 100,
    seed = 444
  )

  # Should get identical results with same seed
  expect_equal(sdt1$bootstrap_ci$dprime_ci, sdt2$bootstrap_ci$dprime_ci)
  expect_equal(sdt1$bootstrap_ci$dprime_se, sdt2$bootstrap_ci$dprime_se)
})


test_that("fit_sdt_roc validates input", {
  test_data <- create_test_sdt_data()

  # Should error without lineup_size when data is dataframe
  expect_error(
    fit_sdt_roc(test_data),
    "lineup_size must be provided"
  )

  # Should error with invalid model type
  expect_error(
    fit_sdt_roc(test_data, lineup_size = 6, model = "invalid"),
    "'arg' should be one of"
  )
})


test_that("z-ROC transformations handle extreme values", {
  # This tests the internal .compute_zroc function indirectly
  # by using data that might produce extreme hit/false alarm rates

  test_data <- create_test_sdt_data(d_prime = 3.0, seed = 555)

  # Should handle without error even with high d' (might produce extreme rates)
  sdt_fit <- fit_sdt_roc(test_data, lineup_size = 6, bootstrap = FALSE)

  expect_s3_class(sdt_fit, "sdt_roc_fit")
  expect_true(is.finite(sdt_fit$dprime))
  expect_true(all(is.finite(sdt_fit$criteria)))
})


test_that("fit_sdt_roc model diagnostics", {
  test_data <- create_test_sdt_data()
  sdt_fit <- fit_sdt_roc(test_data, lineup_size = 6, bootstrap = FALSE)

  # Check fit diagnostics
  expect_true("fit_diagnostics" %in% names(sdt_fit))
  expect_true("r_squared" %in% names(sdt_fit$fit_diagnostics))
  expect_true("residuals" %in% names(sdt_fit$fit_diagnostics))
  expect_true("n_points" %in% names(sdt_fit$fit_diagnostics))

  # Residuals should have length equal to number of ROC points
  expect_equal(
    length(sdt_fit$fit_diagnostics$residuals),
    sdt_fit$fit_diagnostics$n_points
  )
})

test_that("0/1 target_present coding bootstraps like logical coding", {
  skip_on_cran()
  # Codex review 2, finding 4: which() on 0/1 coding failed in every
  # replicate, and fit_sdt_roc() silently returned NA intervals.
  d <- simulate_lineup_data(n_tp = 100, n_ta = 100, seed = 1)
  d01 <- d
  d01$target_present <- as.integer(d01$target_present)

  fit <- suppressWarnings(fit_sdt_roc(d, lineup_size = 6, n_bootstrap = 30, seed = 1))
  fit01 <- suppressWarnings(fit_sdt_roc(d01, lineup_size = 6, n_bootstrap = 30, seed = 1))
  expect_equal(fit01$bootstrap_ci$n_successful, fit$bootstrap_ci$n_successful)
  expect_gt(fit01$bootstrap_ci$n_successful, 0)
  expect_equal(fit01$bootstrap_ci$dprime_ci, fit$bootstrap_ci$dprime_ci)

  cmp <- suppressMessages(compare_pauc(d, d, n_bootstrap = 20, seed = 1))
  cmp01 <- suppressMessages(compare_pauc(d01, d01, n_bootstrap = 20, seed = 1))
  expect_equal(cmp01$se_diff, cmp$se_diff)
  expect_equal(cmp01$ci_diff, cmp$ci_diff)
})

test_that("fit_sdt_roc warns when bootstrap samples fail", {
  d <- simulate_lineup_data(n_tp = 100, n_ta = 100, seed = 1)
  roc <- make_rocdata(d, lineup_size = 6)
  roc$raw_data$identification <- NULL
  expect_warning(fit_sdt_roc(roc, n_bootstrap = 5, seed = 1),
                 "All 5 bootstrap samples failed")
})
