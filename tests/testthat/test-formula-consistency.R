testthat::test_that("esize_T matches Tredoux transformed index of diversity", {
  lineup_table <- table(factor(c(1, 1, 2, 3, 3, 3, 4, 4), levels = 1:4))
  n <- sum(lineup_table)
  index_diversity <- 1 - sum(lineup_table^2) / n^2
  expected <- 1 / (1 - index_diversity)

  testthat::expect_equal(esize_T(lineup_table), expected)
  testthat::expect_equal(esize_T(lineup_table), n^2 / sum(lineup_table^2))
})

testthat::test_that("diag_ratio_T matches documented corrected count formula", {
  lineup_pres <- c(1, 2, 3, 3, 3, 4, 5, 6)
  lineup_abs <- c(1, 2, 2, 3, 4, 5)
  pos_pres <- 3
  pos_abs <- 2

  expected <- ((sum(lineup_pres == pos_pres) + 0.5) / (length(lineup_pres) + 0.5)) /
    ((sum(lineup_abs == pos_abs) + 0.5) / (length(lineup_abs) + 0.5))

  testthat::expect_equal(
    diag_ratio_T(lineup_pres, lineup_abs, pos_pres, pos_abs, k1 = 6, k2 = 5),
    expected
  )
})

testthat::test_that("make_dpp implements Smith et al. (2019) Equation 2", {
  # One operating point: culprit rate .8, innocent-suspect rate .1
  d <- data.frame(
    target_present = rep(c(TRUE, FALSE), each = 20),
    identification = c(rep("suspect", 16), rep("reject", 4),
                       rep("suspect", 2), rep("reject", 18)),
    confidence = 50
  )
  testthat::expect_equal(make_dpp(d)$dpp, 1 - 0.8 + 0.1)

  # Perfect performance is 0
  perfect <- d
  perfect$identification <- c(rep("suspect", 20), rep("reject", 20))
  testthat::expect_equal(make_dpp(perfect)$dpp, 0)

  # Every point on the chance diagonal is 1
  chance <- data.frame(
    target_present = rep(c(TRUE, FALSE), each = 10),
    identification = rep(c(rep("suspect", 4), rep("reject", 6)), 2),
    confidence = rep(c(90, 70, 50, 50, rep(30, 6)), 2)
  )
  dpp_chance <- make_dpp(chance)
  # Confidence 30 belongs only to rejections, so it is not a criterion.
  testthat::expect_equal(dpp_chance$dpp_by_point$dpp, rep(1, 3))
})

# Smith et al. (2019), Table 4: operating points from 100% to 0% confidence
smith_table4 <- list(
  high = list(culprit = c(.032, .065, .099, .128, .160, .217, .236, .263, .278, .282, .287),
              innocent = c(.004, .008, .017, .027, .036, .060, .067, .076, .083, .085, .089)),
  low = list(culprit = c(.164, .240, .326, .379, .420, .520, .534, .551, .561, .566, .567),
             innocent = c(.081, .120, .158, .193, .228, .303, .322, .340, .350, .356, .358))
)
smith_roc <- function(x) {
  list(roc_data = tibble::tibble(
    confidence = c(-10, seq(100, 0, by = -10)),
    correct_id_rate = c(0, x$culprit),
    false_id_rate = c(0, x$innocent)
  ), n_target_present = NA, n_target_absent = NA)
}

testthat::test_that("make_dpp reproduces Smith et al. (2019) Table 4", {
  high <- make_dpp(smith_roc(smith_table4$high), use_roc_obj = TRUE)
  low <- make_dpp(smith_roc(smith_table4$low), use_roc_obj = TRUE)
  testthat::expect_equal(high$dpp, .864, tolerance = .002 / .864)
  testthat::expect_equal(low$dpp, .816, tolerance = .002 / .816)
  # Published per-point values for the first and last operating points. They
  # were computed from unrounded rates, so agree only to about .002.
  testthat::expect_lt(max(abs(high$dpp_by_point$dpp[c(1, 11)] - c(.970, .802))), .003)
  testthat::expect_lt(max(abs(low$dpp_by_point$dpp[c(1, 11)] - c(.917, .791))), .003)
  testthat::expect_equal(nrow(high$dpp_by_point), 11)
})

testthat::test_that("make_dpp Equation 3 weights behave as documented", {
  roc <- smith_roc(smith_table4$low)
  eq2 <- make_dpp(roc, use_roc_obj = TRUE)
  eq3 <- make_dpp(roc, use_roc_obj = TRUE, alpha = 1, beta = 0.5)
  testthat::expect_equal(eq3$dpp, eq2$dpp / 2)
  eq3b <- make_dpp(roc, use_roc_obj = TRUE, alpha = 3, beta = 0.2)
  testthat::expect_equal(
    eq3b$dpp_by_point$dpp,
    0.2 * (1 - smith_table4$low$culprit) + 3 * 0.8 * smith_table4$low$innocent
  )
  testthat::expect_error(make_dpp(roc, use_roc_obj = TRUE, alpha = 1), "both")
  testthat::expect_error(make_dpp(roc, use_roc_obj = TRUE, alpha = 1, beta = 2), "beta")
})

testthat::test_that("compare_dpp bootstraps the difference", {
  skip_on_cran()
  data(lineup_example, package = "r4lineups", envir = environment())
  odd <- seq(1, nrow(lineup_example), by = 2)
  a <- lineup_example[odd, ]
  b <- lineup_example[-odd, ]
  cmp <- compare_dpp(a, b, n_bootstrap = 100, seed = 1)
  testthat::expect_s3_class(cmp, "lineup_dpp_comparison")
  testthat::expect_equal(cmp$dpp_difference, make_dpp(a)$dpp - make_dpp(b)$dpp)
  testthat::expect_length(cmp$boot_differences, 100)
  testthat::expect_true(cmp$se_difference > 0)
  testthat::expect_true(cmp$ci_difference[["lower"]] < cmp$ci_difference[["upper"]])
  testthat::expect_identical(compare_dpp(a, b, n_bootstrap = 100, seed = 1)$boot_differences,
                             cmp$boot_differences)
  none <- compare_dpp(a, b, n_bootstrap = 0)
  testthat::expect_true(is.na(none$se_difference))
})

test_that("esize_T default is unchanged by the unbiased argument", {
  cnt <- c(17, 17, 17, 17, 16, 16)
  expect_equal(esize_T(cnt), sum(cnt)^2 / sum(cnt^2))
  expect_equal(esize_T(cnt), esize_T(cnt, unbiased = FALSE))
})

test_that("esize_T(unbiased = TRUE) removes the small-sample downward bias", {
  # The plug-in estimator of E' is biased downward, by approximately
  # k(k-1)/(N + k - 1). With k = 6 and 50 choices a perfectly fair lineup
  # returns about 5.47 rather than 6. The unbiased estimator of sum(p^2)
  # removes essentially all of it.
  set.seed(4242)
  k <- 6L
  n <- 50L

  est <- replicate(1500, {
    cnt <- tabulate(sample.int(k, n, replace = TRUE), nbins = k)
    c(naive = esize_T(cnt), unbiased = esize_T(cnt, unbiased = TRUE))
  })

  naive_mean    <- mean(est["naive", ])
  unbiased_mean <- mean(est["unbiased", ])

  # Naive is detectably below the true value, and close to the predicted bias
  expect_lt(naive_mean, 5.7)
  expect_equal(naive_mean, n * k / (n + k - 1), tolerance = 0.02)

  # Corrected is close to the truth, and closer than the naive estimator
  expect_equal(unbiased_mean, k, tolerance = 0.02)
  expect_lt(abs(unbiased_mean - k), abs(naive_mean - k))
})

test_that("esize_T bias correction degrades gracefully", {
  # Fewer than two choices: correction undefined
  expect_warning(one <- esize_T(c(1, 0, 0), unbiased = TRUE), "undefined")
  expect_true(is.na(one))

  # Every choice on a different member: sum(o(o-1)) is 0, correction unbounded
  expect_warning(distinct <- esize_T(c(1, 1, 1, 1), unbiased = TRUE),
                 "different lineup member")
  expect_true(is.infinite(distinct))

  expect_error(esize_T(c(5, 5), unbiased = "yes"), "TRUE or FALSE")
})
