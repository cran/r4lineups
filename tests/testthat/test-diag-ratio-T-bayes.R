## Tests for diag_ratio_T_bayes and diag_ratio_T_bayes_compare

# Helper: build simple lineup vectors
make_vecs <- function(n_s, N, pos = 1, k = 6) {
  others <- setdiff(1:k, pos)
  n_o <- N - n_s
  # spread non-suspect choices across ALL other positions so datacheck2 passes
  per <- floor(n_o / length(others))
  extra <- n_o - per * length(others)
  counts <- rep(per, length(others))
  counts[seq_len(extra)] <- counts[seq_len(extra)] + 1
  c(rep(pos, n_s), rep(others, counts))
}

# --- class and structure ---

test_that("diag_ratio_T_bayes returns correct S3 class", {
  pres <- make_vecs(20, 100, pos=3, k=6)
  abs_ <- make_vecs(10, 80,  pos=3, k=6)
  res  <- diag_ratio_T_bayes(pres, abs_, 3, 3, 6, 6, S=500)
  expect_s3_class(res, "diag_ratio_T_bayes")
})

test_that("diag_ratio_T_bayes result has required fields", {
  pres <- make_vecs(20, 100, pos=3); abs_ <- make_vecs(10, 80, pos=3)
  res  <- diag_ratio_T_bayes(pres, abs_, 3, 3, 6, 6, S=500)
  expect_true(all(c("DR_draws","lnDR_draws","posterior_mean","posterior_median",
                    "credible_interval","prior_alpha","n_tp","n_ta","S") %in% names(res)))
})

test_that("DR_draws has length S", {
  pres <- make_vecs(20, 100, pos=3); abs_ <- make_vecs(10, 80, pos=3)
  res  <- diag_ratio_T_bayes(pres, abs_, 3, 3, 6, 6, S=300)
  expect_length(res$DR_draws, 300)
})

# --- posterior direction ---

test_that("when TP rate >> TA rate, posterior DR median > 1", {
  pres <- make_vecs(50, 100, pos=3)   # 50% TP rate
  abs_ <- make_vecs(5,  80,  pos=3)   # ~6% TA rate
  res  <- diag_ratio_T_bayes(pres, abs_, 3, 3, 6, 6, S=2000)
  expect_gt(res$posterior_median, 1)
})

test_that("when TP rate == TA rate, posterior DR median near 1", {
  set.seed(42)
  pres <- make_vecs(20, 100, pos=3)   # 20% rate
  abs_ <- make_vecs(16, 80,  pos=3)   # 20% rate
  res  <- diag_ratio_T_bayes(pres, abs_, 3, 3, 6, 6, S=5000)
  expect_equal(res$posterior_median, 1, tolerance = 0.3)
})

test_that("CI contains frequentist point estimate", {
  pres <- make_vecs(30, 100, pos=3)
  abs_ <- make_vecs(15, 80,  pos=3)
  res  <- diag_ratio_T_bayes(pres, abs_, 3, 3, 6, 6, S=5000)
  freq <- diag_ratio_T(pres, abs_, 3, 3, 6, 6)
  expect_gte(freq, res$credible_interval["lower"])
  expect_lte(freq, res$credible_interval["upper"])
})

# --- credible interval properties ---

test_that("credible interval lower < upper", {
  pres <- make_vecs(20, 100, pos=3); abs_ <- make_vecs(10, 80, pos=3)
  res  <- diag_ratio_T_bayes(pres, abs_, 3, 3, 6, 6, S=500)
  expect_lt(res$credible_interval["lower"], res$credible_interval["upper"])
})

test_that("all DR draws are positive", {
  pres <- make_vecs(20, 100, pos=3); abs_ <- make_vecs(10, 80, pos=3)
  res  <- diag_ratio_T_bayes(pres, abs_, 3, 3, 6, 6, S=500)
  expect_true(all(res$DR_draws > 0))
})

# --- threshold ---

test_that("threshold_probs sum to 1 when threshold is supplied", {
  pres <- make_vecs(30, 100, pos=3); abs_ <- make_vecs(10, 80, pos=3)
  res  <- diag_ratio_T_bayes(pres, abs_, 3, 3, 6, 6, S=1000, threshold=2)
  expect_equal(res$threshold_probs$P_below + res$threshold_probs$P_above, 1,
               tolerance=1/1000)
})

test_that("threshold_probs are in [0,1]", {
  pres <- make_vecs(30, 100, pos=3); abs_ <- make_vecs(10, 80, pos=3)
  res  <- diag_ratio_T_bayes(pres, abs_, 3, 3, 6, 6, S=1000, threshold=2)
  expect_gte(res$threshold_probs$P_below, 0); expect_lte(res$threshold_probs$P_below, 1)
  expect_gte(res$threshold_probs$P_above, 0); expect_lte(res$threshold_probs$P_above, 1)
})

test_that("threshold_probs is NULL when threshold not supplied", {
  pres <- make_vecs(20, 100, pos=3); abs_ <- make_vecs(10, 80, pos=3)
  res  <- diag_ratio_T_bayes(pres, abs_, 3, 3, 6, 6, S=500)
  expect_null(res$threshold_probs)
})

# --- alpha shortcuts ---

test_that("alpha = 'jeffreys' sets prior_alpha to 0.5", {
  pres <- make_vecs(20, 100, pos=3); abs_ <- make_vecs(10, 80, pos=3)
  res  <- diag_ratio_T_bayes(pres, abs_, 3, 3, 6, 6, alpha="jeffreys", S=200)
  expect_equal(res$prior_alpha, 0.5)
})

test_that("alpha = 'uniform' sets prior_alpha to 1", {
  pres <- make_vecs(20, 100, pos=3); abs_ <- make_vecs(10, 80, pos=3)
  res  <- diag_ratio_T_bayes(pres, abs_, 3, 3, 6, 6, alpha="uniform", S=200)
  expect_equal(res$prior_alpha, 1)
})

# --- lnDR = log(DR) ---

test_that("lnDR_draws equals log of DR_draws", {
  pres <- make_vecs(20, 100, pos=3); abs_ <- make_vecs(10, 80, pos=3)
  res  <- diag_ratio_T_bayes(pres, abs_, 3, 3, 6, 6, S=200)
  expect_equal(res$lnDR_draws, log(res$DR_draws))
})

# --- S3 methods do not error ---

test_that("print.diag_ratio_T_bayes runs without error", {
  pres <- make_vecs(20, 100, pos=3); abs_ <- make_vecs(10, 80, pos=3)
  res  <- diag_ratio_T_bayes(pres, abs_, 3, 3, 6, 6, S=200)
  expect_output(print(res), "Bayesian Diagnosticity Ratio")
})

test_that("plot.diag_ratio_T_bayes returns a ggplot", {
  pres <- make_vecs(20, 100, pos=3); abs_ <- make_vecs(10, 80, pos=3)
  res  <- diag_ratio_T_bayes(pres, abs_, 3, 3, 6, 6, S=200)
  p    <- plot(res)
  expect_s3_class(p, "gg")
})

# --- comparison function ---

test_that("diag_ratio_T_bayes_compare returns correct class", {
  pres_A <- make_vecs(30, 100, pos=3); abs_A <- make_vecs(10, 80, pos=3)
  pres_B <- make_vecs(20, 100, pos=3); abs_B <- make_vecs(15, 80, pos=3)
  res <- diag_ratio_T_bayes_compare(pres_A, abs_A, 3, 3, 6, 6,
                                     pres_B, abs_B, 3, 3, 6, 6, S=500)
  expect_s3_class(res, "diag_ratio_T_bayes_compare")
})

test_that("swapping A and B flips sign of posterior mean delta", {
  set.seed(7)
  pres_A <- make_vecs(40, 100, pos=3); abs_A <- make_vecs(8, 80, pos=3)
  pres_B <- make_vecs(20, 100, pos=3); abs_B <- make_vecs(20, 80, pos=3)
  AB <- diag_ratio_T_bayes_compare(pres_A, abs_A, 3, 3, 6, 6,
                                    pres_B, abs_B, 3, 3, 6, 6, S=3000)
  BA <- diag_ratio_T_bayes_compare(pres_B, abs_B, 3, 3, 6, 6,
                                    pres_A, abs_A, 3, 3, 6, 6, S=3000)
  expect_equal(AB$posterior_mean, -BA$posterior_mean, tolerance=0.2)
})

test_that("P_A_greater + P_B_greater == 1 (ignoring ties)", {
  pres_A <- make_vecs(30, 100, pos=3); abs_A <- make_vecs(10, 80, pos=3)
  pres_B <- make_vecs(20, 100, pos=3); abs_B <- make_vecs(15, 80, pos=3)
  res <- diag_ratio_T_bayes_compare(pres_A, abs_A, 3, 3, 6, 6,
                                     pres_B, abs_B, 3, 3, 6, 6, S=1000)
  expect_lte(abs(res$P_A_greater + res$P_B_greater - 1), 0.01)
})

test_that("comparison print runs without error", {
  pres_A <- make_vecs(30, 100, pos=3); abs_A <- make_vecs(10, 80, pos=3)
  pres_B <- make_vecs(20, 100, pos=3); abs_B <- make_vecs(15, 80, pos=3)
  res <- diag_ratio_T_bayes_compare(pres_A, abs_A, 3, 3, 6, 6,
                                     pres_B, abs_B, 3, 3, 6, 6, S=200)
  expect_output(print(res), "Bayesian comparison")
})

test_that("comparison plot returns a ggplot", {
  pres_A <- make_vecs(30, 100, pos=3); abs_A <- make_vecs(10, 80, pos=3)
  pres_B <- make_vecs(20, 100, pos=3); abs_B <- make_vecs(15, 80, pos=3)
  res <- diag_ratio_T_bayes_compare(pres_A, abs_A, 3, 3, 6, 6,
                                     pres_B, abs_B, 3, 3, 6, 6, S=200)
  p <- plot(res)
  expect_s3_class(p, "gg")
})

# --- input validation ---

test_that("invalid alpha string raises error", {
  pres <- make_vecs(20, 100, pos=3); abs_ <- make_vecs(10, 80, pos=3)
  expect_error(diag_ratio_T_bayes(pres, abs_, 3, 3, 6, 6, alpha="bad"), "Unknown alpha")
})

test_that("negative alpha raises error", {
  pres <- make_vecs(20, 100, pos=3); abs_ <- make_vecs(10, 80, pos=3)
  expect_error(diag_ratio_T_bayes(pres, abs_, 3, 3, 6, 6, alpha=-1))
})

test_that("the posterior mean of DR is exact and infinite when it does not exist", {
  set.seed(2)
  pres <- c(rep(3, 40), rep(c(1, 2, 4, 5, 6), 12))
  abs <- c(rep(3, 10), rep(c(1, 2, 4, 5, 6), 18))
  res <- diag_ratio_T_bayes(pres, abs, 3, 3, 6, 6, S = 1e5)
  expected <- 40.5 / 101 * (10.5 + 90.5 - 1) / (10.5 - 1)
  expect_equal(res$posterior_mean, expected)
  expect_equal(res$posterior_mean, mean(res$DR_draws), tolerance = 0.01)

  abs0 <- rep(c(1, 2, 4, 5, 6), 20)
  inf <- diag_ratio_T_bayes(pres, abs0, 3, 3, 6, 6, S = 1000)
  expect_equal(inf$posterior_mean, Inf)
  cmp <- diag_ratio_T_bayes_compare(pres, abs0, 3, 3, 6, 6,
                                    pres, abs0, 3, 3, 6, 6, S = 1000)
  expect_true(is.na(cmp$posterior_mean))
  expect_output(print(cmp), "both means are infinite")
  finite_vs_inf <- diag_ratio_T_bayes_compare(pres, abs, 3, 3, 6, 6,
                                              pres, abs0, 3, 3, 6, 6, S = 1000)
  expect_equal(finite_vs_inf$posterior_mean, -Inf)
})
