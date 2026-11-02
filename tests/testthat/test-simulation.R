# Tests for data simulation functions

test_that("simulate_lineup_data basic functionality", {
  set.seed(123)
  sim_data <- simulate_lineup_data(
    n_tp = 100,
    n_ta = 100,
    d_prime = 1.5,
    lineup_size = 6
  )

  expect_s3_class(sim_data, "simulated_lineup_data")
  expect_equal(nrow(sim_data), 200)
  expect_true(all(c("target_present", "identification", "confidence") %in% names(sim_data)))
})

test_that("simulate_lineup_data generates correct proportions", {
  set.seed(456)
  sim_data <- simulate_lineup_data(
    n_tp = 150,
    n_ta = 150,
    d_prime = 1.5,
    lineup_size = 6
  )

  # Check target_present distribution
  expect_equal(sum(sim_data$target_present), 150)
  expect_equal(sum(!sim_data$target_present), 150)

  # Check identification categories exist
  ids <- table(sim_data$identification)
  # At least one of the expected categories should occur
  expect_true(any(c("suspect", "filler", "reject") %in% names(ids)))
  # All observed categories should be valid
  expect_true(all(names(ids) %in% c("suspect", "filler", "reject")))
})

test_that("simulate_lineup_data d_prime affects performance", {
  set.seed(789)

  # Weak discriminability
  weak_data <- simulate_lineup_data(
    n_tp = 200,
    n_ta = 200,
    d_prime = 0.8,
    lineup_size = 6
  )

  # Strong discriminability
  strong_data <- simulate_lineup_data(
    n_tp = 200,
    n_ta = 200,
    d_prime = 2.5,
    lineup_size = 6
  )

  # Calculate hit rates
  weak_hit_rate <- mean(weak_data$target_present & weak_data$identification == "suspect")
  strong_hit_rate <- mean(strong_data$target_present & strong_data$identification == "suspect")

  # Higher d' should lead to higher hit rate
  expect_true(strong_hit_rate > weak_hit_rate)
})

test_that("simulate_lineup_data criterion affects conservatism", {
  set.seed(111)

  # Liberal criterion
  liberal_data <- simulate_lineup_data(
    n_tp = 150,
    n_ta = 150,
    d_prime = 1.5,
    c_criterion = 0.0,
    lineup_size = 6
  )

  # Conservative criterion
  conservative_data <- simulate_lineup_data(
    n_tp = 150,
    n_ta = 150,
    d_prime = 1.5,
    c_criterion = 1.0,
    lineup_size = 6
  )

  # Calculate rejection rates
  liberal_reject <- mean(liberal_data$identification == "reject")
  conservative_reject <- mean(conservative_data$identification == "reject")

  # Higher criterion should lead to more rejections
  expect_true(conservative_reject > liberal_reject)
})

test_that("simulate_lineup_data lineup_size parameter", {
  set.seed(222)

  sim_data <- simulate_lineup_data(
    n_tp = 100,
    n_ta = 100,
    d_prime = 1.5,
    lineup_size = 8
  )

  # Should have identification decisions
  expect_true(all(sim_data$identification %in% c("suspect", "filler", "reject")))

  # Filler IDs should be possible with larger lineups
  expect_true(any(sim_data$identification == "filler"))
})

test_that("simulate_lineup_data confidence levels", {
  set.seed(333)

  # 3-point scale
  sim_3 <- simulate_lineup_data(
    n_tp = 100,
    n_ta = 100,
    d_prime = 1.5,
    conf_levels = 3
  )

  # 7-point scale
  sim_7 <- simulate_lineup_data(
    n_tp = 100,
    n_ta = 100,
    d_prime = 1.5,
    conf_levels = 7
  )

  expect_true(max(sim_3$confidence) <= 3)
  expect_true(min(sim_3$confidence) >= 1)
  expect_true(max(sim_7$confidence) <= 7)
  expect_true(min(sim_7$confidence) >= 1)
})

test_that("simulate_lineup_data with response times", {
  set.seed(444)

  sim_data <- simulate_lineup_data(
    n_tp = 100,
    n_ta = 100,
    d_prime = 1.5,
    include_response_time = TRUE
  )

  expect_true("response_time" %in% names(sim_data))
  expect_true(all(sim_data$response_time > 0))
  expect_true(is.numeric(sim_data$response_time))
})

test_that("simulate_lineup_data without response times", {
  set.seed(555)

  sim_data <- simulate_lineup_data(
    n_tp = 100,
    n_ta = 100,
    d_prime = 1.5,
    include_response_time = FALSE
  )

  expect_false("response_time" %in% names(sim_data))
})

test_that("simulate_lineup_data seed reproducibility", {
  sim1 <- simulate_lineup_data(
    n_tp = 100,
    n_ta = 100,
    d_prime = 1.5,
    seed = 999
  )

  sim2 <- simulate_lineup_data(
    n_tp = 100,
    n_ta = 100,
    d_prime = 1.5,
    seed = 999
  )

  expect_equal(sim1$identification, sim2$identification)
  expect_equal(sim1$confidence, sim2$confidence)
})

test_that("print.simulated_lineup_data works", {
  sim_data <- simulate_lineup_data(
    n_tp = 50,
    n_ta = 50,
    d_prime = 1.5,
    seed = 666
  )

  expect_output(print(sim_data), "Simulated Lineup Data")
  expect_output(print(sim_data), "d'")
})

test_that("simulate_lineup_data parameter validation", {
  # Negative sample size
  expect_error(
    simulate_lineup_data(n_tp = -10, n_ta = 100, d_prime = 1.5),
    "must be positive"
  )

  # Invalid lineup size
  expect_error(
    simulate_lineup_data(n_tp = 100, n_ta = 100, d_prime = 1.5, lineup_size = 1),
    "must be at least 2"
  )

  # Invalid conf_levels
  expect_error(
    simulate_lineup_data(n_tp = 100, n_ta = 100, d_prime = 1.5, conf_levels = 1),
    "must be at least 2"
  )
})

test_that("simulated data works with r4lineups functions", {
  set.seed(777)

  sim_data <- simulate_lineup_data(
    n_tp = 100,
    n_ta = 100,
    d_prime = 1.8,
    lineup_size = 6,
    conf_levels = 5
  )

  # Test with make_roc
  roc_result <- make_roc(sim_data, lineup_size = 6, show_plot = FALSE)
  expect_true("pauc" %in% names(roc_result))
  expect_true(roc_result$pauc >= 0)

  # Test with make_cac
  cac_result <- make_cac(sim_data, show_plot = FALSE)
  expect_s3_class(cac_result, "lineup_cac")

  # Test with make_fullroc
  fullroc_result <- make_fullroc(sim_data, show_plot = FALSE)
  expect_true("auc" %in% names(fullroc_result))
})

test_that("simulate_power_analysis returns the documented structure", {
  power_result <- simulate_power_analysis(
    sample_sizes = c(40),
    d_prime = 1.5,
    n_simulations = 6,
    n_bootstrap = 25,
    conf_levels = 5,
    seed = 888,
    verbose = FALSE
  )

  expect_s3_class(power_result, "power_analysis")
  expect_true(is.data.frame(power_result))
  expect_true(all(c("sample_size", "power", "power_se", "mean_effect",
                    "sd_effect", "n_valid") %in% names(power_result)))
  expect_true(power_result$power >= 0 && power_result$power <= 1)
  # A size n is n TP and n TA witnesses in each of two conditions.
  expect_equal(power_result$n_total, 4 * power_result$sample_size)
})

test_that("power is not trivially 1 when there is no effect to detect", {
  skip_on_cran()
  # Regression test. The previous implementation computed power as
  # mean(stats > 0) where stats were pAUC values. Because an area is always
  # positive, power was 1 for every sample size and every effect size, and the
  # power curve was a flat line at 1. With no true difference, power must sit
  # near alpha instead.
  no_effect <- suppressWarnings(simulate_power_analysis(
    sample_sizes = c(40),
    d_prime = 1.0,
    d_prime_null = 1.0,
    n_simulations = 12,
    n_bootstrap = 25,
    conf_levels = 5,
    alpha = 0.05,
    seed = 4242,
    verbose = FALSE
  ))

  expect_lt(no_effect$power, 0.5)
})

test_that("simulate_power_analysis validates its inputs", {
  expect_error(
    simulate_power_analysis(sample_sizes = numeric(0), d_prime = 1),
    "numeric vector"
  )
  expect_error(
    simulate_power_analysis(sample_sizes = c(50), d_prime = 1, alpha = 0),
    "strictly between 0 and 1"
  )
  expect_error(
    simulate_power_analysis(sample_sizes = c(50), d_prime = 1,
                            d_prime_null = -1),
    "non-negative"
  )
  # No detectable effect is a user error worth flagging, not a silent result
  expect_warning(
    simulate_power_analysis(sample_sizes = c(30), d_prime = 1,
                            d_prime_null = 1, n_simulations = 2,
                            n_bootstrap = 20, conf_levels = 5, verbose = FALSE),
    "null rejection rate"
  )
})
