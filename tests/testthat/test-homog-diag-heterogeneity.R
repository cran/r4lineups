# A helper matching the vignette: n1 witnesses choose position 1, the rest
# spread over positions 2-6, for a total of 100 witnesses.
choose_pos1 <- function(n1) c(rep(1, n1), rep(2:6, length.out = 100 - n1))

# Ratios differing in degree only: d = 6.0, 3.0, 1.6
het_pres <- list(choose_pos1(60), choose_pos1(45), choose_pos1(32))
het_abs  <- list(choose_pos1(10), choose_pos1(15), choose_pos1(20))

# Ratios differing in direction: d = 6.0, 1.0, 0.44, which cancel on the log
# scale and pool to roughly 1 despite a very large Q.
mixed_pres <- list(choose_pos1(60), choose_pos1(30), choose_pos1(20))
mixed_abs  <- list(choose_pos1(10), choose_pos1(30), choose_pos1(45))

testthat::test_that("homog_diag reports the ratio for each lineup pair", {
  res <- utils::capture.output(
    out <- homog_diag(het_pres, het_abs, pos_list = c(1, 1, 1), k = c(6, 6, 6))
  )

  testthat::expect_equal(out$parameters$d, c(6, 3, 1.6))
  testthat::expect_true(all(c("d", "ln_d", "var_lnd", "wi", "lower", "upper") %in%
                              names(out$parameters)))
  # The raw cell counts are still there for anyone relying on them.
  testthat::expect_equal(out$parameters$n11, c(60, 45, 32))

  res <- paste(res, collapse = "\n")
  testthat::expect_match(res, "Diagnosticity ratio for each lineup pair")
  testthat::expect_match(res, "6\\.000")
  testthat::expect_match(res, "95% CI")
})

testthat::test_that("per-pair intervals are the normal-theory interval on ln(d)", {
  linedf <- diag_param(het_pres, het_abs, c(1, 1, 1), c(6, 6, 6))
  pairs <- diag_pair_summary(linedf)

  ln_d <- ln_diag_ratio(linedf, correction = FALSE)$lnd
  se <- sqrt(var_lnd(linedf)$var)
  testthat::expect_equal(pairs$lower, exp(ln_d - stats::qnorm(0.975) * se))
  testthat::expect_equal(pairs$upper, exp(ln_d + stats::qnorm(0.975) * se))
  testthat::expect_equal(pairs$wi, 1 / var_lnd(linedf)$var)

  # conf.level widens the interval as expected, and is validated.
  wide <- diag_pair_summary(linedf, conf.level = 0.99)
  testthat::expect_true(all(wide$lower < pairs$lower))
  testthat::expect_true(all(wide$upper > pairs$upper))
  testthat::expect_error(diag_pair_summary(linedf, conf.level = 1), "conf.level")
})

testthat::test_that("homog_diag warns against pooling when Q is significant", {
  res <- paste(utils::capture.output(
    out <- homog_diag(het_pres, het_abs, pos_list = c(1, 1, 1), k = c(6, 6, 6))
  ), collapse = " ")

  testthat::expect_true(out$heterogeneous)
  testthat::expect_lt(out$p_value, 0.05)
  testthat::expect_match(res, "heterogeneous")
  testthat::expect_match(res, "should not be reported as a single summary")
})

testthat::test_that("homog_diag says pooling is reasonable when Q is not significant", {
  # Three pairs all with d = 3 exactly.
  pres <- list(choose_pos1(60), choose_pos1(45), choose_pos1(30))
  abs_ <- list(choose_pos1(20), choose_pos1(15), choose_pos1(10))

  res <- paste(utils::capture.output(
    out <- homog_diag(pres, abs_, pos_list = c(1, 1, 1), k = c(6, 6, 6))
  ), collapse = " ")

  testthat::expect_equal(out$parameters$d, c(3, 3, 3))
  testthat::expect_false(out$heterogeneous)
  testthat::expect_match(res, "no evidence of heterogeneity")
  testthat::expect_match(res, "reasonable summary")
})

testthat::test_that("Q and the pooled ratio answer different questions", {
  # The case that motivated the note: ratios pointing in opposite directions
  # give an enormous Q while pooling to roughly 1.
  out <- suppressWarnings(utils::capture.output(
    mixed <- homog_diag(mixed_pres, mixed_abs, pos_list = c(1, 1, 1),
                        k = c(6, 6, 6))
  ))

  testthat::expect_equal(mixed$parameters$d, c(6, 1, 4 / 9))
  testthat::expect_gt(mixed$chi_square, 40)
  testthat::expect_lt(mixed$p_value, 1e-6)
  # ... yet the pooled ratio is indistinguishable from no diagnosticity.
  testthat::expect_equal(mixed$mean_diagnosticity, 1, tolerance = 0.1)
  testthat::expect_true(mixed$heterogeneous)
})

testthat::test_that("chi_diag is Cochran's Q", {
  linedf <- diag_param(het_pres, het_abs, c(1, 1, 1), c(6, 6, 6))
  parts <- t(cbind(var_lnd(linedf),
                   ln_diag_ratio(linedf, correction = FALSE),
                   d_weights(linedf)))

  pairs <- diag_pair_summary(linedf)
  expected <- sum(pairs$wi * (pairs$ln_d - log(d_bar(parts)))^2)
  testthat::expect_equal(chi_diag(parts), expected)
})

testthat::test_that("homog_diag_boot carries the same diagnosis as homog_diag", {
  normal <- utils::capture.output(
    ref <- homog_diag(het_pres, het_abs, pos_list = c(1, 1, 1), k = c(6, 6, 6))
  )
  res <- utils::capture.output(
    out <- homog_diag_boot(het_pres, het_abs, pos_list = c(1, 1, 1),
                           k = c(6, 6, 6), R = 100, seed = 1)
  )

  testthat::expect_equal(out$chi_square, ref$chi_square)
  testthat::expect_equal(out$p_value, ref$p_value)
  testthat::expect_equal(out$df, ref$df)
  testthat::expect_equal(out$parameters$d, ref$parameters$d)
  testthat::expect_true(out$heterogeneous)

  res <- paste(res, collapse = " ")
  testthat::expect_match(res, "Diagnosticity ratio for each lineup pair")
  testthat::expect_match(res, "heterogeneous")

  # The percentile interval on the pooled ratio excludes 1 here: these lineups
  # are diagnostic on average even though the ratios are not interchangeable.
  testthat::expect_gt(out$mean_ci[1], 1)
})

testthat::test_that("homog_diag_boot conf.level drives the percentile interval", {
  narrow <- utils::capture.output(
    a <- homog_diag_boot(het_pres, het_abs, pos_list = c(1, 1, 1),
                         k = c(6, 6, 6), R = 200, seed = 1, conf.level = 0.80)
  )
  wide <- utils::capture.output(
    b <- homog_diag_boot(het_pres, het_abs, pos_list = c(1, 1, 1),
                         k = c(6, 6, 6), R = 200, seed = 1, conf.level = 0.99)
  )

  testthat::expect_gt(a$mean_ci[1], b$mean_ci[1])
  testthat::expect_lt(a$mean_ci[2], b$mean_ci[2])
})

testthat::test_that("conf.level shifts the threshold for declaring heterogeneity", {
  # Q = 11.19 on 2 df, p = .0037: heterogeneous at .05, not at .001.
  strict <- utils::capture.output(
    out <- homog_diag(het_pres, het_abs, pos_list = c(1, 1, 1), k = c(6, 6, 6),
                      conf.level = 0.999)
  )
  testthat::expect_false(out$heterogeneous)
  testthat::expect_match(paste(strict, collapse = " "), "99.9% CI")
})
