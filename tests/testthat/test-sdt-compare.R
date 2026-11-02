## Tests for sdt_compare

# --- class and structure ---

test_that("sdt_compare returns correct S3 class", {
  res <- sdt_compare(hits_A=40, misses_A=60, fas_A=10, crs_A=90,
                      hits_B=30, misses_B=70, fas_B=10, crs_B=90, S=300)
  expect_s3_class(res, "sdt_compare")
})

test_that("result has required fields", {
  res <- sdt_compare(hits_A=40, misses_A=60, fas_A=10, crs_A=90,
                      hits_B=30, misses_B=70, fas_B=10, crs_B=90, S=300)
  required <- c("delta_dprime_draws", "dprime_A_draws", "dprime_B_draws",
                "posterior_mean", "posterior_median", "credible_interval",
                "P_A_greater", "P_B_greater",
                "dprime_A_mean", "dprime_B_mean",
                "prior_alpha", "S", "credible_mass")
  expect_true(all(required %in% names(res)))
})

test_that("delta_dprime_draws has length S", {
  res <- sdt_compare(hits_A=40, misses_A=60, fas_A=10, crs_A=90,
                      hits_B=30, misses_B=70, fas_B=10, crs_B=90, S=400)
  expect_length(res$delta_dprime_draws, 400)
})

# --- posterior direction ---

test_that("identical conditions give delta d' posterior centered near 0", {
  set.seed(42)
  res <- sdt_compare(hits_A=40, misses_A=60, fas_A=10, crs_A=90,
                      hits_B=40, misses_B=60, fas_B=10, crs_B=90, S=5000)
  expect_equal(res$posterior_mean, 0, tolerance = 0.15)
})

test_that("strong A superiority gives P(d'A > d'B) > 0.9", {
  res <- sdt_compare(hits_A=80, misses_A=20, fas_A=10, crs_A=90,
                      hits_B=30, misses_B=70, fas_B=30, crs_B=70, S=5000)
  expect_gt(res$P_A_greater, 0.9)
})

test_that("strong B superiority gives P(d'B > d'A) > 0.9", {
  res <- sdt_compare(hits_A=30, misses_A=70, fas_A=30, crs_A=70,
                      hits_B=80, misses_B=20, fas_B=10, crs_B=90, S=5000)
  expect_gt(res$P_B_greater, 0.9)
})

test_that("P_A_greater + P_B_greater approx 1 (ignoring exact ties)", {
  res <- sdt_compare(hits_A=40, misses_A=60, fas_A=10, crs_A=90,
                      hits_B=35, misses_B=65, fas_B=15, crs_B=85, S=5000)
  expect_lte(abs(res$P_A_greater + res$P_B_greater - 1), 0.01)
})

# --- CI properties ---

test_that("credible interval lower < upper", {
  res <- sdt_compare(hits_A=40, misses_A=60, fas_A=10, crs_A=90,
                      hits_B=30, misses_B=70, fas_B=10, crs_B=90, S=500)
  expect_lt(res$credible_interval["lower"], res$credible_interval["upper"])
})

test_that("CI contains the frequentist delta d' point estimate", {
  res  <- sdt_compare(hits_A=45, misses_A=55, fas_A=12, crs_A=88,
                       hits_B=38, misses_B=62, fas_B=10, crs_B=90, S=5000)
  freq_diff <- res$frequentist$diff
  expect_gte(freq_diff, res$credible_interval["lower"])
  expect_lte(freq_diff, res$credible_interval["upper"])
})

test_that("swapping A and B flips sign of posterior mean", {
  set.seed(7)
  AB <- sdt_compare(hits_A=60, misses_A=40, fas_A=10, crs_A=90,
                     hits_B=30, misses_B=70, fas_B=20, crs_B=80, S=3000)
  BA <- sdt_compare(hits_A=30, misses_A=70, fas_A=20, crs_A=80,
                     hits_B=60, misses_B=40, fas_B=10, crs_B=90, S=3000)
  expect_equal(AB$posterior_mean, -BA$posterior_mean, tolerance = 0.2)
})

# --- alpha shortcuts ---

test_that("alpha 'jeffreys' gives prior_alpha 0.5", {
  res <- sdt_compare(hits_A=40, misses_A=60, fas_A=10, crs_A=90,
                      hits_B=30, misses_B=70, fas_B=10, crs_B=90,
                      alpha = "jeffreys", S = 100)
  expect_equal(res$prior_alpha, 0.5)
})

test_that("alpha 'uniform' gives prior_alpha 1", {
  res <- sdt_compare(hits_A=40, misses_A=60, fas_A=10, crs_A=90,
                      hits_B=30, misses_B=70, fas_B=10, crs_B=90,
                      alpha = "uniform", S = 100)
  expect_equal(res$prior_alpha, 1)
})

# --- label storage ---

test_that("labels are stored correctly", {
  res <- sdt_compare(hits_A=40, misses_A=60, fas_A=10, crs_A=90,
                      hits_B=30, misses_B=70, fas_B=10, crs_B=90,
                      label_A = "Simultaneous", label_B = "Sequential", S = 100)
  expect_equal(res$label_A, "Simultaneous")
  expect_equal(res$label_B, "Sequential")
})

# --- S3 methods ---

test_that("print runs without error and mentions label", {
  res <- sdt_compare(hits_A=40, misses_A=60, fas_A=10, crs_A=90,
                      hits_B=30, misses_B=70, fas_B=10, crs_B=90,
                      label_A="Sim", label_B="Seq", S=200)
  expect_output(print(res), "Sim")
})

test_that("plot returns a ggplot", {
  res <- sdt_compare(hits_A=40, misses_A=60, fas_A=10, crs_A=90,
                      hits_B=30, misses_B=70, fas_B=10, crs_B=90, S=200)
  p <- plot(res)
  expect_s3_class(p, "gg")
})

# --- input validation ---

test_that("negative count raises error", {
  expect_error(
    sdt_compare(hits_A=-1, misses_A=60, fas_A=10, crs_A=90,
                 hits_B=30, misses_B=70, fas_B=10, crs_B=90, S=100)
  )
})

test_that("invalid credible_mass raises error", {
  expect_error(
    sdt_compare(hits_A=40, misses_A=60, fas_A=10, crs_A=90,
                 hits_B=30, misses_B=70, fas_B=10, crs_B=90,
                 credible_mass=1.5, S=100)
  )
})
