testthat::test_that("max filler distribution matches standard normal for m=2", {
  x <- seq(-2, 2, length.out = 5)
  expect_equal(dmax_filler(x, lineup_size = 2), stats::dnorm(x))
  expect_equal(pmax_filler(x, lineup_size = 2), stats::pnorm(x))
})

testthat::test_that("qmax_filler inverts pmax_filler", {
  p <- c(0.1, 0.5, 0.9)
  q <- qmax_filler(p, lineup_size = 6)
  p_back <- pmax_filler(q, lineup_size = 6)
  expect_equal(p_back, p, tolerance = 1e-8)
})

testthat::test_that("msdt parameter estimation recovers gamma and dprime", {
  m <- 6
  gamma <- 0.4
  dprime <- 1.1
  pr_rej_I <- stats::pnorm(gamma)^m
  pr_rej_G <- stats::pnorm(gamma - dprime) * stats::pnorm(gamma)^(m - 1)

  est <- estimate_msdt_params(pr_rej_I, pr_rej_G, lineup_size = m, eps = 1e-10)
  expect_equal(est$gamma, gamma, tolerance = 1e-6)
  expect_equal(est$dprime, dprime, tolerance = 1e-6)
})

testthat::test_that("max filler mean increases with lineup size", {
  m2 <- max_filler_moments(2, moments = "mean")$mean
  m6 <- max_filler_moments(6, moments = "mean")$mean
  expect_true(m6 > m2)
})
