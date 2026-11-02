## Tests for calibration_bayes

make_calib_df <- function(n = 120, conf_levels = c(50, 70, 90),
                           acc_rates = c(0.4, 0.6, 0.8)) {
  bins <- lapply(seq_along(conf_levels), function(i) {
    m <- n / length(conf_levels)
    n_correct <- round(m * acc_rates[i])
    data.frame(
      confidence     = rep(conf_levels[i], m),
      target_present = c(rep(TRUE, n_correct), rep(FALSE, m - n_correct)),
      identification = "suspect"
    )
  })
  do.call(rbind, bins)
}

# --- class and structure ---

test_that("calibration_bayes returns correct S3 class", {
  df  <- make_calib_df()
  res <- calibration_bayes(df, confidence_bins = c(0, 60, 80, 100), S = 200)
  expect_s3_class(res, "calibration_bayes")
})

test_that("result has required fields", {
  df  <- make_calib_df()
  res <- calibration_bayes(df, confidence_bins = c(0, 60, 80, 100), S = 200)
  required <- c("C_draws", "OU_draws", "NRI_draws",
                "C_mean", "C_median", "C_ci",
                "OU_mean", "OU_median", "OU_ci",
                "NRI_mean", "NRI_median", "NRI_ci",
                "point_estimates", "bin_data",
                "prior_alpha", "S", "credible_mass", "n_total")
  expect_true(all(required %in% names(res)))
})

test_that("C_draws, OU_draws, NRI_draws have length S", {
  df  <- make_calib_df()
  res <- calibration_bayes(df, confidence_bins = c(0, 60, 80, 100), S = 300)
  expect_length(res$C_draws,   300)
  expect_length(res$OU_draws,  300)
  expect_length(res$NRI_draws, 300)
})

# --- posterior direction ---

test_that("overconfident data gives positive OU posterior mean", {
  # high confidence bins, low accuracy — witness overconfident
  df <- make_calib_df(n = 150, conf_levels = c(70, 90),
                      acc_rates = c(0.2, 0.3))
  res <- calibration_bayes(df, confidence_bins = c(0, 80, 100), S = 2000)
  expect_gt(res$OU_mean, 0)
})

test_that("underconfident data gives negative OU posterior mean", {
  # low confidence, high accuracy
  df <- make_calib_df(n = 150, conf_levels = c(50, 70),
                      acc_rates = c(0.9, 0.95))
  res <- calibration_bayes(df, confidence_bins = c(0, 60, 100), S = 2000)
  expect_lt(res$OU_mean, 0)
})

test_that("C posterior is non-negative", {
  df  <- make_calib_df()
  res <- calibration_bayes(df, confidence_bins = c(0, 60, 80, 100), S = 500)
  expect_true(all(res$C_draws >= 0))
})

test_that("good calibration gives C posterior near 0", {
  # accuracy approximately equals confidence proportion in each bin
  df <- rbind(
    data.frame(confidence=50, target_present=c(rep(TRUE,25), rep(FALSE,25)),
               identification="suspect"),
    data.frame(confidence=70, target_present=c(rep(TRUE,49), rep(FALSE,21)),
               identification="suspect"),
    data.frame(confidence=90, target_present=c(rep(TRUE,72), rep(FALSE,8)),
               identification="suspect")
  )
  res <- calibration_bayes(df, confidence_bins = c(0, 60, 80, 100), S = 3000)
  expect_lt(res$C_mean, 0.02)
})

# --- credible interval properties ---

test_that("C credible interval lower < upper", {
  df  <- make_calib_df()
  res <- calibration_bayes(df, confidence_bins = c(0, 60, 80, 100), S = 500)
  expect_lt(res$C_ci["lower"], res$C_ci["upper"])
})

test_that("OU credible interval lower < upper", {
  df  <- make_calib_df()
  res <- calibration_bayes(df, confidence_bins = c(0, 60, 80, 100), S = 500)
  expect_lt(res$OU_ci["lower"], res$OU_ci["upper"])
})

test_that("NRI credible interval lower < upper", {
  df  <- make_calib_df()
  res <- calibration_bayes(df, confidence_bins = c(0, 60, 80, 100), S = 500)
  expect_lt(res$NRI_ci["lower"], res$NRI_ci["upper"])
})

test_that("CI contains frequentist C point estimate", {
  df  <- make_calib_df()
  res <- calibration_bayes(df, confidence_bins = c(0, 60, 80, 100), S = 5000)
  expect_gte(res$point_estimates$C, res$C_ci["lower"])
  expect_lte(res$point_estimates$C, res$C_ci["upper"])
})

test_that("CI contains frequentist OU point estimate", {
  df  <- make_calib_df()
  res <- calibration_bayes(df, confidence_bins = c(0, 60, 80, 100), S = 5000)
  expect_gte(res$point_estimates$OU, res$OU_ci["lower"])
  expect_lte(res$point_estimates$OU, res$OU_ci["upper"])
})

# --- alpha shortcuts ---

test_that("alpha 'jeffreys' gives prior_alpha 0.5", {
  df  <- make_calib_df()
  res <- calibration_bayes(df, confidence_bins = c(0, 60, 80, 100),
                            alpha = "jeffreys", S = 100)
  expect_equal(res$prior_alpha, 0.5)
})

test_that("alpha 'uniform' gives prior_alpha 1", {
  df  <- make_calib_df()
  res <- calibration_bayes(df, confidence_bins = c(0, 60, 80, 100),
                            alpha = "uniform", S = 100)
  expect_equal(res$prior_alpha, 1)
})

# --- metadata ---

test_that("n_total and bin_data stored correctly", {
  df  <- make_calib_df(n = 120)
  res <- calibration_bayes(df, confidence_bins = c(0, 60, 80, 100), S = 100)
  expect_equal(res$n_total, 120)
  expect_true(is.data.frame(res$bin_data))
  expect_gt(nrow(res$bin_data), 0)
})

test_that("credible_mass stored correctly", {
  df  <- make_calib_df()
  res <- calibration_bayes(df, confidence_bins = c(0, 60, 80, 100),
                            credible_mass = 0.89, S = 100)
  expect_equal(res$credible_mass, 0.89)
})

# --- S3 methods ---

test_that("print runs without error", {
  df  <- make_calib_df()
  res <- calibration_bayes(df, confidence_bins = c(0, 60, 80, 100), S = 200)
  expect_output(print(res), "Bayesian Calibration")
})

test_that("plot returns ggplot for C", {
  df  <- make_calib_df()
  res <- calibration_bayes(df, confidence_bins = c(0, 60, 80, 100), S = 200)
  expect_s3_class(plot(res, metric = "C"), "gg")
})

test_that("plot returns ggplot for OU", {
  df  <- make_calib_df()
  res <- calibration_bayes(df, confidence_bins = c(0, 60, 80, 100), S = 200)
  expect_s3_class(plot(res, metric = "OU"), "gg")
})

test_that("plot returns ggplot for NRI", {
  df  <- make_calib_df()
  res <- calibration_bayes(df, confidence_bins = c(0, 60, 80, 100), S = 200)
  expect_s3_class(plot(res, metric = "NRI"), "gg")
})

# --- input validation ---

test_that("invalid S raises error", {
  df <- make_calib_df()
  expect_error(calibration_bayes(df, confidence_bins = c(0, 60, 80, 100), S = -1))
})

test_that("credible_mass outside (0,1) raises error", {
  df <- make_calib_df()
  expect_error(calibration_bayes(df, confidence_bins = c(0, 60, 80, 100),
                                  credible_mass = 1.5))
})
