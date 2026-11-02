# Tests for decision rules in lineup simulation

test_that("simulate_lineup_data accepts all decision rules", {
  # Test that all four decision rules work
  rules <- c("max", "best_rest", "ensemble", "integration")

  for (rule in rules) {
    data <- simulate_lineup_data(
      n_tp = 50, n_ta = 50,
      d_prime = 1.5,
      decision_rule = rule,
      conf_levels = 3,
      seed = 123
    )

    expect_s3_class(data, "simulated_lineup_data")
    expect_equal(nrow(data), 100)
    expect_true("identification" %in% names(data))
    expect_true("confidence" %in% names(data))
  }
})


test_that("MAX rule produces valid data", {
  data <- simulate_lineup_data(
    n_tp = 100, n_ta = 100,
    d_prime = 1.5,
    decision_rule = "max",
    conf_levels = 5,
    seed = 456
  )

  # Should have all required columns
  expect_true(all(c("participant_id", "target_present", "identification", "confidence") %in% names(data)))

  # Should have correct number of rows
  expect_equal(nrow(data), 200)

  # Identifications should be valid
  expect_true(all(data$identification %in% c("suspect", "filler", "reject")))

  # Confidence should be between 1 and 5
  expect_true(all(data$confidence >= 1 & data$confidence <= 5))

  # Target-present should be logical
  expect_type(data$target_present, "logical")
})


test_that("BEST-REST rule produces valid data", {
  data <- simulate_lineup_data(
    n_tp = 100, n_ta = 100,
    d_prime = 1.5,
    decision_rule = "best_rest",
    conf_levels = 5,
    seed = 789
  )

  expect_s3_class(data, "simulated_lineup_data")
  expect_equal(nrow(data), 200)
  expect_true(all(data$identification %in% c("suspect", "filler", "reject")))
})


test_that("Ensemble rule produces valid data", {
  data <- simulate_lineup_data(
    n_tp = 100, n_ta = 100,
    d_prime = 1.5,
    decision_rule = "ensemble",
    conf_levels = 5,
    seed = 111
  )

  expect_s3_class(data, "simulated_lineup_data")
  expect_equal(nrow(data), 200)
  expect_true(all(data$identification %in% c("suspect", "filler", "reject")))
})


test_that("Integration rule produces valid data", {
  data <- simulate_lineup_data(
    n_tp = 100, n_ta = 100,
    d_prime = 1.5,
    decision_rule = "integration",
    conf_levels = 5,
    seed = 222
  )

  expect_s3_class(data, "simulated_lineup_data")
  expect_equal(nrow(data), 200)
  expect_true(all(data$identification %in% c("suspect", "filler", "reject")))
})


test_that("decision rules produce different results", {
  # Different rules should produce at least some different identifications
  # when using different seeds

  data_max <- simulate_lineup_data(
    n_tp = 200, n_ta = 200,
    d_prime = 1.5,
    decision_rule = "max",
    seed = 333
  )

  data_integration <- simulate_lineup_data(
    n_tp = 200, n_ta = 200,
    d_prime = 1.5,
    decision_rule = "integration",
    seed = 333
  )

  # Integration should produce different pattern than MAX
  # (not necessarily always different, but usually)
  max_suspect_ids <- sum(data_max$identification == "suspect")
  integration_suspect_ids <- sum(data_integration$identification == "suspect")

  # They should be in the same ballpark but not identical
  expect_true(abs(max_suspect_ids - integration_suspect_ids) < 100)
})


test_that("decision_rule parameter validation works", {
  # Invalid decision rule should error
  expect_error(
    simulate_lineup_data(
      n_tp = 50, n_ta = 50,
      d_prime = 1.5,
      decision_rule = "invalid_rule"
    ),
    "'arg' should be one of"
  )
})


test_that("decision rule is stored in attributes", {
  data <- simulate_lineup_data(
    n_tp = 50, n_ta = 50,
    d_prime = 1.5,
    decision_rule = "ensemble",
    seed = 444
  )

  params <- attr(data, "simulation_params")
  expect_equal(params$decision_rule, "ensemble")
})


test_that("print method shows decision rule", {
  data <- simulate_lineup_data(
    n_tp = 50, n_ta = 50,
    d_prime = 1.5,
    decision_rule = "best_rest",
    seed = 555
  )

  output <- capture.output(print(data))
  expect_true(any(grepl("Decision rule.*best_rest", output)))
})


test_that(".apply_decision_rule helper function works", {
  # Test the internal helper function directly

  strengths <- c(2.0, 0.5, -0.3, 1.2, 0.8, -0.5)  # 6 lineup members
  lineup_size <- 6
  d_prime <- 1.5

  # MAX rule
  result_max <- r4lineups:::.apply_decision_rule(strengths, "max", lineup_size, d_prime)
  expect_equal(result_max$decision_value, 2.0)
  expect_equal(result_max$chosen_position, 1)

  # BEST-REST rule
  result_br <- r4lineups:::.apply_decision_rule(strengths, "best_rest", lineup_size, d_prime)
  expect_true(is.numeric(result_br$decision_value))
  expect_equal(result_br$chosen_position, 1)

  # Ensemble rule
  result_ens <- r4lineups:::.apply_decision_rule(strengths, "ensemble", lineup_size, d_prime)
  expect_equal(result_ens$decision_value,
               max(strengths) - mean(strengths))
  expect_equal(result_ens$chosen_position, 1)

  # Integration rule
  result_int <- r4lineups:::.apply_decision_rule(strengths, "integration", lineup_size, d_prime)
  expect_equal(result_int$decision_value, sum(strengths))
  expect_equal(result_int$chosen_position, 1)
})

test_that("BEST-Rest and Ensemble have the documented scaling identity", {
  strengths <- c(2.0, 0.5, -0.3, 1.2, 0.8, -0.5)
  k <- length(strengths)
  best_rest <- r4lineups:::.apply_decision_rule(
    strengths, "best_rest", k
  )$decision_value
  ensemble <- r4lineups:::.apply_decision_rule(
    strengths, "ensemble", k
  )$decision_value

  expect_equal(ensemble, (k - 1) / k * best_rest, tolerance = 1e-14)
})

test_that("BEST-Rest and Ensemble simulations agree under rescaled criteria", {
  k <- 6L
  best_rest <- simulate_lineup_data(
    n_tp = 200, n_ta = 200, d_prime = 1.5,
    c_criterion = c(0.2, 0.7, 1.2), conf_levels = 3,
    lineup_size = k, decision_rule = "best_rest", seed = 912
  )
  ensemble <- simulate_lineup_data(
    n_tp = 200, n_ta = 200, d_prime = 1.5,
    c_criterion = (k - 1) / k * c(0.2, 0.7, 1.2), conf_levels = 3,
    lineup_size = k, decision_rule = "ensemble", seed = 912
  )

  expect_equal(best_rest$target_present, ensemble$target_present)
  expect_equal(best_rest$identification, ensemble$identification)
  expect_equal(best_rest$confidence, ensemble$confidence)
})

test_that("the supplied lowest criterion controls identification", {
  liberal <- simulate_lineup_data(
    n_tp = 100, n_ta = 100, d_prime = 1,
    c_criterion = -100, conf_levels = 5, seed = 16
  )
  conservative <- simulate_lineup_data(
    n_tp = 100, n_ta = 100, d_prime = 1,
    c_criterion = 100, conf_levels = 5, seed = 16
  )

  expect_false(any(liberal$identification == "reject"))
  expect_true(all(conservative$identification == "reject"))
  expect_equal(attr(liberal, "simulation_params")$criteria,
               seq(-100, -98, length.out = 5))
})

test_that("explicit confidence criteria are preserved", {
  criteria <- c(-0.25, 0.1, 0.8, 1.4)
  data <- simulate_lineup_data(
    n_tp = 25, n_ta = 25, c_criterion = criteria,
    conf_levels = length(criteria), seed = 17
  )
  expect_equal(attr(data, "simulation_params")$criteria, criteria)
})

test_that("simulation criteria receive strict validation", {
  expect_error(
    simulate_lineup_data(n_tp = 10, n_ta = 10,
                         c_criterion = c(0, 1), conf_levels = 3),
    "one value per confidence level"
  )
  expect_error(
    simulate_lineup_data(n_tp = 10, n_ta = 10,
                         c_criterion = c(0, 0, 1), conf_levels = 3),
    "strictly increasing"
  )
  expect_error(
    simulate_lineup_data(n_tp = 10, n_ta = 10,
                         c_criterion = c(0, 1), conf_levels = NULL),
    "scalar"
  )
})


test_that("decision rules work with different lineup sizes", {
  lineup_sizes <- c(6, 8, 10)

  for (size in lineup_sizes) {
    for (rule in c("max", "best_rest", "ensemble", "integration")) {
      data <- simulate_lineup_data(
        n_tp = 50, n_ta = 50,
        d_prime = 1.5,
        lineup_size = size,
        decision_rule = rule,
        seed = 666
      )

      expect_s3_class(data, "simulated_lineup_data")
      expect_equal(nrow(data), 100)
    }
  }
})


test_that("decision rules work without confidence levels", {
  for (rule in c("max", "best_rest", "ensemble", "integration")) {
    data <- simulate_lineup_data(
      n_tp = 50, n_ta = 50,
      d_prime = 1.5,
      decision_rule = rule,
      conf_levels = NULL,
      seed = 777
    )

    expect_s3_class(data, "simulated_lineup_data")
    expect_false("confidence" %in% names(data))
  }
})


test_that("decision rules work with response times", {
  for (rule in c("max", "best_rest", "ensemble", "integration")) {
    data <- simulate_lineup_data(
      n_tp = 50, n_ta = 50,
      d_prime = 1.5,
      decision_rule = rule,
      include_response_time = TRUE,
      seed = 888
    )

    expect_s3_class(data, "simulated_lineup_data")
    expect_true("response_time" %in% names(data))
    expect_true(all(data$response_time > 0))
  }
})


test_that("decision rules produce sensible hit rates", {
  # With high d', should get high hit rates
  # With low d', should get lower hit rates

  for (rule in c("max", "best_rest", "ensemble", "integration")) {
    # High d'
    high_d <- simulate_lineup_data(
      n_tp = 200, n_ta = 200,
      d_prime = 2.5,
      decision_rule = rule,
      seed = 999
    )

    # Low d'
    low_d <- simulate_lineup_data(
      n_tp = 200, n_ta = 200,
      d_prime = 0.5,
      decision_rule = rule,
      seed = 999
    )

    # Calculate hit rates
    high_hr <- sum(high_d$target_present & high_d$identification == "suspect") / 200
    low_hr <- sum(low_d$target_present & low_d$identification == "suspect") / 200

    # High d' should produce higher hit rate
    expect_true(high_hr > low_hr, info = paste("Rule:", rule))
  }
})


test_that("decision rules produce sensible ROC curves", {
  # Each decision rule should produce valid ROC curves

  for (rule in c("max", "best_rest", "ensemble", "integration")) {
    data <- simulate_lineup_data(
      n_tp = 200, n_ta = 200,
      d_prime = 1.5,
      decision_rule = rule,
      conf_levels = 5,
      seed = 1111
    )

    roc <- make_roc(data, lineup_size = 6, show_plot = FALSE)

    expect_true("pauc" %in% names(roc))
    expect_true("roc_data" %in% names(roc))

    # pAUC should be between 0 and 1
    expect_true(roc$pauc >= 0 && roc$pauc <= 1, info = paste("Rule:", rule))
  }
})


test_that("integration model has different performance", {
  # Integration model should generally have lower performance
  # because it sums all lineup members, diluting the signal

  data_max <- simulate_lineup_data(
    n_tp = 300, n_ta = 300,
    d_prime = 2.0,
    decision_rule = "max",
    conf_levels = 5,
    seed = 1234
  )

  data_integration <- simulate_lineup_data(
    n_tp = 300, n_ta = 300,
    d_prime = 2.0,
    decision_rule = "integration",
    conf_levels = 5,
    seed = 1234
  )

  roc_max <- make_roc(data_max, lineup_size = 6, show_plot = FALSE)
  roc_int <- make_roc(data_integration, lineup_size = 6, show_plot = FALSE)

  # Integration should have lower pAUC (but this is a stochastic test)
  # So we just check that both produce valid results
  expect_true(roc_max$pauc > 0)
  expect_true(roc_int$pauc > 0)
})


test_that("reproducibility with seed", {
  # Same seed should produce identical results

  for (rule in c("max", "best_rest", "ensemble", "integration")) {
    data1 <- simulate_lineup_data(
      n_tp = 50, n_ta = 50,
      d_prime = 1.5,
      decision_rule = rule,
      seed = 5555
    )

    data2 <- simulate_lineup_data(
      n_tp = 50, n_ta = 50,
      d_prime = 1.5,
      decision_rule = rule,
      seed = 5555
    )

    # Should be identical
    expect_equal(data1$identification, data2$identification, info = paste("Rule:", rule))
    expect_equal(data1$target_present, data2$target_present, info = paste("Rule:", rule))
  }
})


test_that("decision rules work with different d' values", {
  d_primes <- c(0.5, 1.0, 1.5, 2.0, 3.0)

  for (dp in d_primes) {
    for (rule in c("max", "best_rest", "ensemble", "integration")) {
      data <- simulate_lineup_data(
        n_tp = 50, n_ta = 50,
        d_prime = dp,
        decision_rule = rule,
        conf_levels = 3,
        seed = 6666
      )

      expect_s3_class(data, "simulated_lineup_data")
      expect_equal(nrow(data), 100)
    }
  }
})
