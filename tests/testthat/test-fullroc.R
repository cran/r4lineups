# Tests for Full ROC analysis (Smith & Yang, 2020)

make_sim_data <- function(n = 200, seed = 123,
                          tp_probs = c(0.6, 0.2, 0.2),
                          ta_probs = c(0.2, 0.3, 0.5)) {
  set.seed(seed)
  data.frame(
    target_present = rep(c(TRUE, FALSE), each = n / 2),
    identification = c(
      sample(c("suspect", "filler", "reject"), n / 2, replace = TRUE, prob = tp_probs),
      sample(c("suspect", "filler", "reject"), n / 2, replace = TRUE, prob = ta_probs)
    ),
    confidence = sample(seq(20, 100, by = 20), n, replace = TRUE)
  )
}

test_that("make_fullroc_data returns a complete curve ending at (1,1)", {
  data <- make_sim_data()
  result <- make_fullroc_data(data)

  expect_true(is.list(result))
  expect_true(all(c("roc_data", "auc", "diagnosticity_table") %in% names(result)))

  roc <- result$roc_data
  expect_equal(max(roc$cumulative_hit_rate), 1, tolerance = 1e-10)
  expect_equal(max(roc$cumulative_false_alarm_rate), 1, tolerance = 1e-10)
  expect_true(result$auc >= 0 && result$auc <= 1)
})

test_that("perfect separation yields AUC = 1", {
  n <- 100
  data <- data.frame(
    target_present = rep(c(TRUE, FALSE), each = n / 2),
    identification = rep(c("suspect", "reject"), each = n / 2),
    confidence = rep(c(90, 90), each = n / 2)
  )
  result <- make_fullroc_data(data)
  expect_equal(result$auc, 1, tolerance = 1e-10)
})

test_that("identical TP/TA distributions give AUC near 0.5 (with known upward bias)", {
  n <- 400
  set.seed(42)
  ids <- sample(c("suspect", "filler", "reject"), n / 2, replace = TRUE)
  confs <- sample(c(30, 60, 90), n / 2, replace = TRUE)
  data <- data.frame(
    target_present = rep(c(TRUE, FALSE), each = n / 2),
    identification = rep(ids, 2),
    confidence = rep(confs, 2)
  )
  result <- make_fullroc_data(data, order = "diagnosticity")
  # Identical distributions: true discriminability is nil. Diagnosticity
  # ordering is optimistically biased, so AUC lands at or slightly above 0.5.
  expect_gte(result$auc, 0.5 - 1e-8)
  expect_lt(result$auc, 0.6)

  # A-priori ordering has no selection bias: AUC should be ~0.5 exactly
  result_ap <- make_fullroc_data(data, order = "apriori")
  expect_equal(result_ap$auc, 0.5, tolerance = 0.02)
})

test_that("apriori ordering with conf_bins does not warn (regression for NA coercion bug)", {
  data <- make_sim_data()
  expect_no_warning(
    result <- make_fullroc_data(data, order = "apriori",
                                conf_bins = c(0, 60, 80, 100))
  )
  expect_equal(max(result$roc_data$cumulative_hit_rate), 1, tolerance = 1e-10)
  expect_equal(max(result$roc_data$cumulative_false_alarm_rate), 1, tolerance = 1e-10)
})

test_that("make_fullroc wrapper returns plot and summary", {
  data <- make_sim_data()
  result <- make_fullroc(data, show_plot = TRUE)
  expect_s3_class(result, "lineup_fullroc")
  expect_s3_class(result$plot, "ggplot")
  expect_true(is.numeric(result$auc))
  expect_equal(result$summary$n_target_present, 100)
  expect_equal(result$summary$n_target_absent, 100)
})

test_that("make_fullroc_data errors without both lineup types", {
  data <- make_sim_data()
  expect_error(make_fullroc_data(data[data$target_present, ]),
               "target-present and target-absent")
})

test_that("full ROC splits TA filler IDs when no innocent suspect is designated", {
  dat <- data.frame(
    target_present = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
    identification = c("suspect", "filler", "reject", "filler", "filler", "reject"),
    confidence = rep(80, 6)
  )
  result <- make_fullroc_data(dat, lineup_size = 4, order = "apriori")
  tab <- result$diagnosticity_table
  expect_equal(tab$n_false_alarms[tab$decision == "suspect"], 0.5)
  expect_equal(tab$n_false_alarms[tab$decision == "filler"], 1.5)
  expect_equal(sum(tab$false_alarm_rate), 1)
  expect_equal(result$innocent_suspect_method, "estimated_from_fillers")
})
