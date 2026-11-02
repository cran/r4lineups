## Regression tests for diagnosticity ratio bug fixes (Tredoux, 2026-05)

# ln_diag_ratio: Haldane continuity-corrected formula
# Correct formula: d = ((n11+0.5)/(N_TP+0.5)) / ((n12+0.5)/(N_TA+0.5))
# Buggy formula:   d = (n11 + 0.5/N_TP) / (n12 + 0.5/N_TA)   ← operator precedence error

test_that("ln_diag_ratio uses correct Haldane continuity correction", {
  linedf <- data.frame(n11 = 20, n21 = 80, n12 = 10, n22 = 90)
  # Correct: d = (20.5/100.5) / (10.5/100.5) = 20.5/10.5
  expected_lnd <- log(20.5 / 10.5)
  result <- ln_diag_ratio(linedf)
  expect_equal(result$lnd, expected_lnd, tolerance = 1e-10)
})

test_that("ln_diag_ratio gives lnd > 0 when TP rate > TA rate", {
  linedf <- data.frame(n11 = 40, n21 = 60, n12 = 10, n22 = 90)
  result <- ln_diag_ratio(linedf)
  expect_true(all(result$lnd > 0))
})

test_that("ln_diag_ratio gives lnd < 0 when TP rate < TA rate", {
  linedf <- data.frame(n11 = 5, n21 = 95, n12 = 30, n22 = 70)
  result <- ln_diag_ratio(linedf)
  expect_true(all(result$lnd < 0))
})

test_that("ln_diag_ratio handles multiple lineups", {
  linedf <- data.frame(
    n11 = c(20, 30), n21 = c(80, 70),
    n12 = c(10, 15), n22 = c(90, 85)
  )
  expected <- c(
    log((20.5 / 100.5) / (10.5 / 100.5)),
    log((30.5 / 100.5) / (15.5 / 100.5))
  )
  result <- ln_diag_ratio(linedf)
  expect_equal(result$lnd, expected, tolerance = 1e-10)
})

# homog_diag row-order bug: par1 was lnd, par2 was var → wrong row order passed to chi_diag/d_bar
# After fix: par1 = var, par2 = lnd → Row1=var, Row2=lnd, Row3=wi (matches chi_diag/d_bar expectations)

test_that("d_bar returns correct pooled estimate (row-order fix)", {
  # Two lineups with known diagnosticity ratios both ≈ 2
  # d_bar should be > 1 (closer to 2), not < 1 (inverted)
  linedf <- data.frame(
    n11 = c(20, 30), n21 = c(80, 70),
    n12 = c(10, 15), n22 = c(90, 85)
  )
  var  <- var_lnd(linedf)
  lnd  <- ln_diag_ratio(linedf)
  wi   <- d_weights(linedf)
  df   <- t(cbind(var, lnd, wi))

  result <- d_bar(df)
  # Both lineups have d ≈ 2; pooled estimate should be > 1
  expect_gt(result, 1)
  # Should be in a sensible range (between 1.5 and 3)
  expect_lt(result, 3)
})

test_that("chi_diag returns non-negative chi-square (row-order fix)", {
  linedf <- data.frame(
    n11 = c(20, 30, 15), n21 = c(80, 70, 85),
    n12 = c(10, 14,  8), n22 = c(90, 86, 92)
  )
  var <- var_lnd(linedf)
  lnd <- ln_diag_ratio(linedf)
  wi  <- d_weights(linedf)
  df  <- t(cbind(var, lnd, wi))

  q <- chi_diag(df)
  expect_true(is.numeric(q))
  expect_gte(q, 0)
})

test_that("homog_diag runs without error and produces sensible output", {
  set.seed(42)
  pres <- list(
    sample(1:6, 100, replace = TRUE),
    sample(1:6,  80, replace = TRUE)
  )
  abs_ <- list(
    sample(1:6, 100, replace = TRUE),
    sample(1:6,  80, replace = TRUE)
  )
  pos <- c(1, 1)
  k   <- c(6, 6)
  # Should complete without error
  expect_output(homog_diag(pres, abs_, pos, k), "Mean diagnosticity ratio")
})
