test_that("esize_T_bayes returns correct class and structure", {
  tab <- as.table(setNames(c(18L, 7L, 5L, 4L, 3L, 3L), 1:6))
  result <- esize_T_bayes(tab, S = 500)

  expect_s3_class(result, "esize_T_bayes")
  expect_named(result, c("E_draws", "posterior_mean", "posterior_median",
                          "credible_interval", "prior_alpha",
                          "n", "k", "S", "credible_mass",
                          "threshold", "threshold_probs"))
  expect_length(result$E_draws, 500L)
  expect_equal(result$k, 6L)
  expect_equal(result$n, 40L)
  expect_null(result$threshold_probs)
})

test_that("uniform lineup gives posterior mean near k", {
  # When all k positions are equally chosen, E' should be near k
  k <- 6L
  counts <- rep(100L, k)
  tab <- as.table(setNames(counts, 1:k))
  set.seed(1)
  result <- esize_T_bayes(tab, S = 5000)
  expect_true(abs(result$posterior_mean - k) < 0.2)
})

test_that("credible interval contains the frequentist point estimate", {
  tab <- as.table(setNames(c(18L, 7L, 5L, 4L, 3L, 3L), 1:6))
  set.seed(42)
  result <- esize_T_bayes(tab, S = 5000, credible_mass = 0.95)
  freq_est <- esize_T(tab)
  expect_true(
    result$credible_interval["lower"] <= freq_est &&
      freq_est <= result$credible_interval["upper"]
  )
})

test_that("threshold argument produces probabilities in [0, 1]", {
  tab <- as.table(setNames(c(18L, 7L, 5L, 4L, 3L, 3L), 1:6))
  set.seed(7)
  result <- esize_T_bayes(tab, S = 1000, threshold = 3.5)
  expect_equal(result$threshold, 3.5)
  expect_true(result$threshold_probs$P_below >= 0 &&
                result$threshold_probs$P_below <= 1)
  expect_true(result$threshold_probs$P_above >= 0 &&
                result$threshold_probs$P_above <= 1)
  expect_true(abs(result$threshold_probs$P_below +
                    result$threshold_probs$P_above - 1) < 1e-6)
})

test_that("alpha as vector of length k works", {
  tab <- as.table(setNames(c(10L, 8L, 7L, 6L, 5L, 4L), 1:6))
  alpha_vec <- c(0.5, 0.5, 0.5, 0.5, 0.5, 1.0)
  set.seed(3)
  result <- esize_T_bayes(tab, alpha = alpha_vec, S = 500)
  expect_equal(result$prior_alpha, alpha_vec)
})

test_that("prior sensitivity: smaller alpha gives wider posterior", {
  tab <- as.table(setNames(c(18L, 7L, 5L, 4L, 3L, 3L), 1:6))
  set.seed(99)
  r_weak    <- esize_T_bayes(tab, alpha = 0.1, S = 5000)
  set.seed(99)
  r_jeffreys <- esize_T_bayes(tab, alpha = 0.5, S = 5000)
  set.seed(99)
  r_uniform  <- esize_T_bayes(tab, alpha = 1.0, S = 5000)

  width <- function(x) x$credible_interval["upper"] - x$credible_interval["lower"]
  # Weaker priors (smaller alpha) should produce wider or comparable CIs
  expect_true(width(r_weak) >= width(r_uniform) - 0.05)
})

test_that("esize_T_bayes errors on bad inputs", {
  tab_ok <- as.table(setNames(c(10L, 5L, 5L), 1:3))
  expect_error(esize_T_bayes(as.table(setNames(5L, 1L))), "at least 2 positions")
  expect_error(esize_T_bayes(as.table(setNames(c(0L, 0L, 0L), 1:3))), "at least one observation")
  expect_error(esize_T_bayes(tab_ok, alpha = c(0.5, 0.5)), "scalar or a vector of length k")
  expect_error(esize_T_bayes(tab_ok, alpha = -1), "positive")
  expect_error(esize_T_bayes(tab_ok, credible_mass = 1.5), "strictly between 0 and 1")
  expect_error(esize_T_bayes(tab_ok, threshold = c(1, 2)), "finite scalar")
})

test_that("print.esize_T_bayes runs without error", {
  tab <- as.table(setNames(c(18L, 7L, 5L, 4L, 3L, 3L), 1:6))
  result <- esize_T_bayes(tab, S = 200)
  expect_output(print(result), "Bayesian Posterior")
  expect_output(print(result), "credible interval")
})

test_that("plot.esize_T_bayes returns a ggplot", {
  tab <- as.table(setNames(c(18L, 7L, 5L, 4L, 3L, 3L), 1:6))
  result <- esize_T_bayes(tab, S = 200)
  p <- plot(result)
  expect_s3_class(p, "ggplot")
})

# ── esize_T_bayes_compare ─────────────────────────────────────────────────────

test_that("esize_T_bayes_compare returns correct class and structure", {
  tab_a <- as.table(setNames(c(20L, 5L, 5L, 5L, 5L, 5L), 1:6))
  tab_b <- as.table(setNames(c(10L, 8L, 8L, 8L, 8L, 8L), 1:6))
  set.seed(55)
  cmp <- esize_T_bayes_compare(tab_a, tab_b, S = 500)

  expect_s3_class(cmp, "esize_T_bayes_compare")
  expect_length(cmp$delta, 500L)
  expect_true(cmp$P_A_greater >= 0 && cmp$P_A_greater <= 1)
  expect_true(cmp$P_B_greater >= 0 && cmp$P_B_greater <= 1)
  expect_true(abs(cmp$P_A_greater + cmp$P_B_greater - 1) < 1e-6)
})

test_that("compare function: swap A and B flips sign of delta mean", {
  tab_a <- as.table(setNames(c(20L, 5L, 5L, 5L, 5L, 5L), 1:6))
  tab_b <- as.table(setNames(c(10L, 8L, 8L, 8L, 8L, 8L), 1:6))
  set.seed(11)
  ab <- esize_T_bayes_compare(tab_a, tab_b, S = 5000)
  set.seed(11)
  ba <- esize_T_bayes_compare(tab_b, tab_a, S = 5000)
  expect_true(sign(ab$posterior_mean) == -sign(ba$posterior_mean))
})

test_that("concentrated lineup A gives P_A_greater < 0.5", {
  # tab_a has suspect dominating (low E'), tab_b is near-uniform (high E')
  tab_a <- as.table(setNames(c(40L, 2L, 2L, 2L, 2L, 2L), 1:6))
  tab_b <- as.table(setNames(c(10L, 8L, 8L, 8L, 8L, 8L), 1:6))
  set.seed(22)
  cmp <- esize_T_bayes_compare(tab_a, tab_b, S = 5000)
  expect_true(cmp$P_A_greater < 0.5)
})

test_that("print.esize_T_bayes_compare runs without error", {
  tab_a <- as.table(setNames(c(20L, 5L, 5L, 5L, 5L, 5L), 1:6))
  tab_b <- as.table(setNames(c(10L, 8L, 8L, 8L, 8L, 8L), 1:6))
  cmp <- esize_T_bayes_compare(tab_a, tab_b, S = 200)
  expect_output(print(cmp), "Bayesian Comparison")
  expect_output(print(cmp), "P\\(E\\'_A")
})

test_that("plot.esize_T_bayes_compare returns a ggplot", {
  tab_a <- as.table(setNames(c(20L, 5L, 5L, 5L, 5L, 5L), 1:6))
  tab_b <- as.table(setNames(c(10L, 8L, 8L, 8L, 8L, 8L), 1:6))
  cmp <- esize_T_bayes_compare(tab_a, tab_b, S = 200)
  p <- plot(cmp)
  expect_s3_class(p, "ggplot")
})

test_that("a threshold at the nominal size warns that P(E' < k) is 1", {
  # Third review, finding 8: E' <= k, with equality only at exact uniformity,
  # so P(E' < k | data) = 1 even for perfectly balanced counts.
  tab <- as.table(setNames(rep(1000, 6), 1:6))
  expect_warning(res <- esize_T_bayes(tab, S = 500, threshold = 6),
                 "cannot exceed the nominal size")
  expect_equal(res$threshold_probs$P_below, 1)
  expect_silent(esize_T_bayes(tab, S = 500, threshold = 4.8))
})
