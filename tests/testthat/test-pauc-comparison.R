# Tests for pAUC comparison functions

# Create test data
create_test_data_pauc <- function(n_tp = 80, n_ta = 80, seed = 123) {
  set.seed(seed)
  data.frame(
    target_present = rep(c(TRUE, FALSE), each = n_tp),
    identification = c(
      sample(c("suspect", "filler", "reject"), n_tp, replace = TRUE,
             prob = c(0.6, 0.2, 0.2)),
      sample(c("suspect", "filler", "reject"), n_ta, replace = TRUE,
             prob = c(0.2, 0.3, 0.5))
    ),
    confidence = sample(seq(0, 100, 20), n_tp + n_ta, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

test_that("compare_pauc basic functionality", {
  skip_on_cran()
  data1 <- create_test_data_pauc(seed = 111)
  data2 <- create_test_data_pauc(seed = 222)

  comparison <- compare_pauc(
    data1,
    data2,
    lineup_size = 6,
    n_bootstrap = 100,  # Small for testing speed
    seed = 123
  )

  expect_s3_class(comparison, "pauc_comparison")
  expect_true("pauc1" %in% names(comparison))
  expect_true("pauc2" %in% names(comparison))
  expect_true("pauc_diff" %in% names(comparison))
  expect_true("z_score" %in% names(comparison))
  expect_true("p_value" %in% names(comparison))
})

test_that("compare_pauc returns valid statistics", {
  skip_on_cran()
  data1 <- create_test_data_pauc(seed = 333)
  data2 <- create_test_data_pauc(seed = 444)

  comparison <- compare_pauc(
    data1,
    data2,
    n_bootstrap = 100,
    seed = 456
  )

  # pAUC values should be between 0 and 1
  expect_true(comparison$pauc1 >= 0 & comparison$pauc1 <= 1)
  expect_true(comparison$pauc2 >= 0 & comparison$pauc2 <= 1)

  # Standard errors should be positive
  expect_true(comparison$se_pauc1 > 0)
  expect_true(comparison$se_pauc2 > 0)
  expect_true(comparison$se_diff > 0)

  # P-value should be between 0 and 1
  expect_true(comparison$p_value >= 0 & comparison$p_value <= 1)

  # CI should have two values
  expect_length(comparison$ci_diff, 2)
  expect_true(comparison$ci_diff["lower"] < comparison$ci_diff["upper"])
})

test_that("compare_pauc with custom cutoff", {
  skip_on_cran()
  data1 <- create_test_data_pauc()
  data2 <- create_test_data_pauc(seed = 999)

  comparison <- compare_pauc(
    data1,
    data2,
    max_false_id_rate = 0.20,
    n_bootstrap = 100,
    seed = 789
  )

  expect_equal(comparison$max_false_id_rate, 0.20)
  expect_true(comparison$pauc1 <= 0.20)  # pAUC up to cutoff
})

test_that("compare_pauc with custom labels", {
  skip_on_cran()
  data1 <- create_test_data_pauc()
  data2 <- create_test_data_pauc(seed = 888)

  comparison <- compare_pauc(
    data1,
    data2,
    label1 = "Condition A",
    label2 = "Condition B",
    n_bootstrap = 100,
    seed = 111
  )

  expect_equal(comparison$label1, "Condition A")
  expect_equal(comparison$label2, "Condition B")
})

test_that("compare_pauc bootstrap results", {
  skip_on_cran()
  data1 <- create_test_data_pauc()
  data2 <- create_test_data_pauc(seed = 777)

  comparison <- compare_pauc(
    data1,
    data2,
    n_bootstrap = 100,
    seed = 222
  )

  boot_results <- comparison$bootstrap_results

  expect_true("pauc1_boot" %in% names(boot_results))
  expect_true("pauc2_boot" %in% names(boot_results))
  expect_true("diff_boot" %in% names(boot_results))

  # Should have approximately n_bootstrap samples (some may be removed if failed)
  expect_true(length(boot_results$pauc1_boot) >= 90)
  expect_true(length(boot_results$pauc1_boot) <= 100)
})

test_that("print.pauc_comparison works", {
  skip_on_cran()
  data1 <- create_test_data_pauc()
  data2 <- create_test_data_pauc(seed = 666)

  comparison <- compare_pauc(
    data1, data2,
    n_bootstrap = 50,
    seed = 333
  )

  expect_output(print(comparison), "pAUC Comparison Analysis")
  expect_output(print(comparison), "Z =")
  expect_output(print(comparison), "p-value")
})

test_that("summary.pauc_comparison works", {
  skip_on_cran()
  data1 <- create_test_data_pauc()
  data2 <- create_test_data_pauc(seed = 555)

  comparison <- compare_pauc(
    data1, data2,
    n_bootstrap = 50,
    seed = 444
  )

  expect_output(summary(comparison), "pAUC Comparison Summary")
  expect_output(summary(comparison), "Sample size")
  expect_output(summary(comparison), "SE-standardized difference")
})

test_that("compare_pauc validates input data", {
  # Missing columns
  bad_data <- data.frame(x = 1:10, y = 1:10)
  good_data <- create_test_data_pauc()

  expect_error(
    compare_pauc(bad_data, good_data, n_bootstrap = 50),
    "Missing required columns"
  )
})

test_that("compare_pauc with different sample sizes", {
  skip_on_cran()
  data1 <- create_test_data_pauc(n_tp = 60, n_ta = 60)
  data2 <- create_test_data_pauc(n_tp = 100, n_ta = 100, seed = 321)

  comparison <- compare_pauc(
    data1, data2,
    n_bootstrap = 50,
    seed = 555
  )

  expect_s3_class(comparison, "pauc_comparison")
  expect_equal(comparison$roc1$n_target_present, 60)
  expect_equal(comparison$roc2$n_target_present, 100)
})

test_that("compare_pauc confidence level", {
  skip_on_cran()
  data1 <- create_test_data_pauc()
  data2 <- create_test_data_pauc(seed = 444)

  comparison_95 <- compare_pauc(
    data1, data2,
    conf_level = 0.95,
    n_bootstrap = 50,
    seed = 666
  )

  comparison_99 <- compare_pauc(
    data1, data2,
    conf_level = 0.99,
    n_bootstrap = 50,
    seed = 666
  )

  # 99% CI should be wider
  width_95 <- comparison_95$ci_diff["upper"] - comparison_95$ci_diff["lower"]
  width_99 <- comparison_99$ci_diff["upper"] - comparison_99$ci_diff["lower"]

  expect_true(width_99 > width_95)
})

test_that("compare_pauc seed reproducibility", {
  skip_on_cran()
  data1 <- create_test_data_pauc()
  data2 <- create_test_data_pauc(seed = 123)

  comp1 <- compare_pauc(data1, data2, n_bootstrap = 50, seed = 777)
  comp2 <- compare_pauc(data1, data2, n_bootstrap = 50, seed = 777)

  expect_equal(comp1$pauc_diff, comp2$pauc_diff)
  expect_equal(comp1$z_score, comp2$z_score)
  expect_equal(comp1$p_value, comp2$p_value)
})

test_that("compare_pauc ROC objects included", {
  skip_on_cran()
  data1 <- create_test_data_pauc()
  data2 <- create_test_data_pauc(seed = 222)

  comparison <- compare_pauc(
    data1, data2,
    n_bootstrap = 50,
    seed = 888
  )

  expect_true("roc1" %in% names(comparison))
  expect_true("roc2" %in% names(comparison))
  expect_true("roc_data" %in% names(comparison$roc1))
  expect_true("roc_data" %in% names(comparison$roc2))
})

test_that("default pAUC region is one both curves actually reach", {
  # Regression test. The default cutoff used to be the LARGER of the two
  # curves' maximum false-ID rates. The curve that stopped earlier was then
  # integrated only as far as its own data extended, so the two areas covered
  # different intervals and were not comparable. The default is now the largest
  # region both conditions attain.
  data1 <- create_test_data_pauc(seed = 311)
  data2 <- create_test_data_pauc(seed = 312)

  comparison <- suppressMessages(compare_pauc(
    data1, data2, n_bootstrap = 30, seed = 913
  ))

  reach1 <- max(comparison$roc1$roc_data$false_id_rate)
  reach2 <- max(comparison$roc2$roc_data$false_id_rate)

  expect_equal(comparison$max_false_id_rate, min(reach1, reach2))
  expect_lte(comparison$max_false_id_rate, reach1)
  expect_lte(comparison$max_false_id_rate, reach2)
})

test_that("a cutoff beyond one curve's reach is warned about", {
  data1 <- create_test_data_pauc(seed = 321)
  data2 <- create_test_data_pauc(seed = 322)

  reach <- min(
    max(make_rocdata(data1, lineup_size = 6)$roc_data$false_id_rate),
    max(make_rocdata(data2, lineup_size = 6)$roc_data$false_id_rate)
  )

  expect_warning(
    compare_pauc(data1, data2, max_false_id_rate = reach + 0.05,
                 n_bootstrap = 20, seed = 914),
    "cover different intervals"
  )
})
