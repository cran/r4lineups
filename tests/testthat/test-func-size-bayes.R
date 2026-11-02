## Tests for func_size_bayes

make_lineup <- function(n_s, N, pos = 3, k = 6) {
  others <- setdiff(1:k, pos)
  c(rep(pos, n_s), rep(others[1], N - n_s))
}

# --- class and structure ---

test_that("func_size_bayes returns correct S3 class", {
  v   <- make_lineup(20, 100)
  res <- func_size_bayes(v, target_pos = 3, S = 300)
  expect_s3_class(res, "func_size_bayes")
})

test_that("result has required fields", {
  v   <- make_lineup(20, 100)
  res <- func_size_bayes(v, target_pos = 3, S = 300)
  expect_true(all(c("F_draws","posterior_mean","posterior_median",
                    "credible_interval","n","n_suspect","S") %in% names(res)))
})

test_that("F_draws has length S", {
  v   <- make_lineup(20, 100)
  res <- func_size_bayes(v, target_pos = 3, S = 400)
  expect_length(res$F_draws, 400)
})

# --- posterior direction ---

test_that("uniform selection gives posterior median near k", {
  # 100 people, 6 positions, exactly 1/6 each
  v   <- rep(1:6, each = 50)[1:300]
  # n_s = 50 out of 300 → p = 1/6 → F = 6
  res <- func_size_bayes(v, target_pos = 3, S = 5000)
  expect_equal(res$posterior_median, 6, tolerance = 0.5)
})

test_that("high suspect selection gives posterior F close to 1", {
  v   <- make_lineup(90, 100, pos = 3)
  res <- func_size_bayes(v, target_pos = 3, S = 3000)
  expect_lt(res$posterior_median, 2)
})

test_that("CI lower < CI upper", {
  v   <- make_lineup(20, 100)
  res <- func_size_bayes(v, target_pos = 3, S = 500)
  expect_lt(res$credible_interval["lower"], res$credible_interval["upper"])
})

test_that("CI contains func_size point estimate", {
  v   <- make_lineup(20, 100, pos = 3)
  res <- func_size_bayes(v, target_pos = 3, S = 5000)
  pt  <- func_size(v, 3)
  expect_gte(pt, res$credible_interval["lower"])
  expect_lte(pt, res$credible_interval["upper"])
})

# --- threshold ---

test_that("threshold_probs sum to 1 when threshold supplied", {
  v   <- make_lineup(20, 100)
  res <- func_size_bayes(v, target_pos = 3, S = 1000, threshold = 6)
  expect_equal(res$threshold_probs$P_below + res$threshold_probs$P_above, 1,
               tolerance = 1/1000)
})

test_that("threshold_probs NULL when not supplied", {
  v   <- make_lineup(20, 100)
  res <- func_size_bayes(v, target_pos = 3, S = 300)
  expect_null(res$threshold_probs)
})

# --- alpha shortcuts ---

test_that("alpha 'jeffreys' gives prior_alpha 0.5", {
  v   <- make_lineup(20, 100)
  res <- func_size_bayes(v, 3, alpha = "jeffreys", S = 100)
  expect_equal(res$prior_alpha, 0.5)
})

# --- S3 methods ---

test_that("print runs without error", {
  v   <- make_lineup(20, 100)
  res <- func_size_bayes(v, target_pos = 3, S = 200)
  expect_output(print(res), "Bayesian Functional Size")
})

test_that("plot returns a ggplot", {
  v   <- make_lineup(20, 100)
  res <- func_size_bayes(v, target_pos = 3, S = 200)
  p   <- plot(res)
  expect_s3_class(p, "gg")
})

# --- all draws positive ---

test_that("all F_draws are positive", {
  v   <- make_lineup(20, 100)
  res <- func_size_bayes(v, target_pos = 3, S = 500)
  expect_true(all(res$F_draws > 0))
})

# --- metadata stored correctly ---

test_that("n and n_suspect are stored correctly", {
  v   <- make_lineup(17, 100, pos = 3)
  res <- func_size_bayes(v, target_pos = 3, S = 100)
  expect_equal(res$n, 100)
  expect_equal(res$n_suspect, 17)
})

test_that("the posterior mean of F is exact and infinite when it does not exist", {
  set.seed(1)
  vec <- c(rep(1, 30), rep(2:6, 14))
  res <- func_size_bayes(vec, target_pos = 1, S = 1e5)
  a <- 30.5
  b <- 70.5
  expect_equal(res$posterior_mean, (a + b - 1) / (a - 1))
  expect_equal(res$posterior_mean, mean(res$F_draws), tolerance = 0.01)
  none <- suppressWarnings(func_size_bayes(rep(2:6, 20), target_pos = 1, S = 1000))
  expect_equal(none$posterior_mean, Inf)
  expect_output(print(none), "does not exist")
})
