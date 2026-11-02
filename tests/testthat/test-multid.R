idx <- function(md) stats::setNames(md$indices$estimate, md$indices$index)

# Expand counts to trial-level data with one confidence value per row
counts_to_data <- function(tp, ta, confidence = NULL) {
  ids <- c("suspect", "filler", "reject")
  d <- data.frame(
    target_present = rep(c(TRUE, FALSE), c(sum(tp), sum(ta))),
    identification = c(rep(ids, tp), rep(ids, ta)),
    stringsAsFactors = FALSE
  )
  if (!is.null(confidence)) d$confidence <- confidence
  d
}

testthat::test_that("multid reproduces Lee & Penrod (2019) Table 4", {
  md <- make_multid(counts = c(G = 45, Fp = 24, Rp = 31, I = 17, Fa = 26, Ra = 57),
                    correction = "none")
  est <- idx(md)
  testthat::expect_equal(round(unname(est[c("GI", "GFp", "IFa", "FaFp", "RaRp")]), 2),
                         c(0.83, 0.58, -0.31, 0.06, 0.67))
})

testthat::test_that("multid reproduces Lee & Penrod (2019) Table 3", {
  # Published rates are rounded, so allow a small tolerance
  rows <- list(
    list(c(G = .32, Fp = .44, Rp = .24, I = .06, Fa = .64, Ra = .30), c(1.07, -0.30, -1.87, 0.50)),
    list(c(G = .25, Fp = .55, Rp = .20, I = .07, Fa = .69, Ra = .24), c(0.81, -0.80, -1.99, 0.37)),
    list(c(G = .52, Fp = .25, Rp = .24, I = .28, Fa = .26, Ra = .46), c(0.63, 0.72, 0.06, 0.03)),
    list(c(G = .44, Fp = .19, Rp = .39, I = .15, Fa = .17, Ra = .68), c(0.89, 0.73, -0.08, -0.08))
  )
  for (r in rows) {
    est <- idx(make_multid(counts = r[[1]], correction = "none"))
    testthat::expect_equal(unname(est[c("GI", "GFp", "IFa", "FaFp")]), r[[2]],
                           tolerance = 0.05)
  }
})

testthat::test_that("multid by_confidence reproduces Lee & Penrod (2019) Table 9", {
  levels <- seq(100, 0, by = -10)
  cum <- list(
    G = c(243, 415, 710, 1046, 1391, 1769, 1998, 2256, 2444, 2567, 2622),
    Fp = c(33, 66, 186, 416, 768, 1270, 1652, 2058, 2439, 2713, 2922),
    Rp = c(123, 235, 398, 613, 821, 1201, 1423, 1652, 1860, 2043, 2437),
    I = c(12, 25, 77, 128, 191, 296, 384, 456, 542, 596, 632),
    Fa = c(36, 82, 237, 527, 933, 1551, 2051, 2601, 3046, 3372, 3613),
    Ra = c(304, 511, 826, 1204, 1558, 2082, 2373, 2718, 2984, 3235, 3735)
  )
  per_level <- lapply(cum, function(x) diff(c(0, x)))
  conf_for <- function(nm) rep(levels, per_level[[nm]])
  d <- data.frame(
    target_present = rep(c(TRUE, FALSE), c(sum(per_level$G, per_level$Fp, per_level$Rp),
                                          sum(per_level$I, per_level$Fa, per_level$Ra))),
    identification = c(rep(c("suspect", "filler", "reject"),
                           c(2622, 2922, 2437)),
                       rep(c("suspect", "filler", "reject"),
                           c(632, 3613, 3735))),
    confidence = c(conf_for("G"), conf_for("Fp"), conf_for("Rp"),
                   conf_for("I"), conf_for("Fa"), conf_for("Ra"))
  )
  md <- make_multid(d, correction = "none", by_confidence = TRUE)
  bc <- md$by_confidence
  published <- rbind(
    c(1.09, 0.77, -0.36, 0.03, 0.39),
    c(1.11, 0.77, -0.42, 0.08, 0.37),
    c(0.99, 0.64, -0.45, 0.10, 0.38),
    c(1.02, 0.50, -0.64, 0.12, 0.39),
    c(1.04, 0.37, -0.79, 0.11, 0.41),
    c(1.02, 0.23, -0.92, 0.14, 0.39),
    c(0.99, 0.14, -1.01, 0.16, 0.39),
    c(1.00, 0.08, -1.13, 0.20, 0.41),
    c(0.98, 0.00, -1.19, 0.21, 0.41),
    c(0.98, -0.05, -1.25, 0.22, 0.42),
    c(0.97, -0.10, -1.29, 0.22, 0.43)
  )
  testthat::expect_equal(bc$threshold, levels)
  testthat::expect_equal(unname(as.matrix(bc[, c("GI", "GFp", "IFa", "FaFp", "RaRp")])),
                         published, tolerance = 0.02)
  testthat::expect_equal(round(bc$c[1], 2), 2.42)
})

testthat::test_that("d'(GI) decomposition holds exactly", {
  for (fr in c("total", "per_filler")) {
    for (corr in c("loglinear", "half", "none")) {
      est <- idx(make_multid(counts = c(G = 30, Fp = 12, Rp = 8, I = 5, Fa = 20, Ra = 25),
                             filler_rate = fr, correction = corr))
      testthat::expect_equal(unname(est["GI"]),
                             unname(est["GFp"] - est["IFa"] - est["FaFp"]),
                             tolerance = 1e-12)
    }
  }
})

testthat::test_that("fair reference matches the average-filler values in the paper", {
  # No rejections in a 6-person lineup: d'(IFa) = z(1/6) - z(5/6)
  md <- make_multid(counts = c(G = 50, Fp = 50, Rp = 0, Fa = 60, Ra = 0),
                    correction = "none")
  est <- idx(md)
  testthat::expect_equal(md$innocent_method, "estimated_from_fillers")
  testthat::expect_equal(unname(est["fair_reference"]),
                         stats::qnorm(1 / 6) - stats::qnorm(5 / 6), tolerance = 1e-5)
  # The average-filler practice makes the TA lineup exactly fair
  testthat::expect_equal(unname(est["IFa"]), unname(est["fair_reference"]), tolerance = 1e-5)
  testthat::expect_equal(unname(est["ifa_vs_fair"]), 0, tolerance = 1e-5)
  # Paper: d'(IFa) is -1.71 when the rejection rate is .10
  est10 <- idx(make_multid(counts = c(G = 50, Fp = 40, Rp = 10, Fa = 90, Ra = 10),
                           correction = "none"))
  testthat::expect_equal(round(unname(est10["IFa"]), 2), -1.71)
})

testthat::test_that("per-filler rates give a fair reference of zero", {
  # Exact only without a rate correction, which shifts I and Fa differently
  md <- make_multid(counts = c(G = 40, Fp = 30, Rp = 30, Fa = 60, Ra = 40),
                    filler_rate = "per_filler", correction = "none")
  est <- idx(md)
  testthat::expect_equal(unname(est["fair_reference"]), 0)
  testthat::expect_equal(unname(est["IFa"]), 0, tolerance = 1e-12)
})

testthat::test_that("data and count inputs agree", {
  tp <- c(40, 25, 35)
  ta <- c(10, 45, 45)
  from_data <- make_multid(counts_to_data(tp, ta))
  from_counts <- make_multid(counts = c(G = 40, Fp = 25, Rp = 35, I = 10, Fa = 45, Ra = 45))
  testthat::expect_equal(from_data$indices$estimate, from_counts$indices$estimate)

  # Without designated innocent-suspect rows, I is estimated from fillers
  no_suspect <- counts_to_data(tp, c(0, 55, 45))
  est <- make_multid(no_suspect)
  testthat::expect_equal(est$innocent_method, "estimated_from_fillers")
  testthat::expect_equal(unname(est$counts[c("I", "Fa")]), c(55 / 6, 55 * 5 / 6))
})

testthat::test_that("bootstrap intervals bracket the estimates", {
  md <- make_multid(counts = c(G = 120, Fp = 40, Rp = 40, I = 30, Fa = 90, Ra = 80),
                    n_bootstrap = 300, seed = 42)
  tab <- md$indices
  core <- tab$index %in% c("GI", "GFp", "IFa", "FaFp", "RaRp")
  testthat::expect_true(all(tab$ci_lower[core] <= tab$estimate[core]))
  testthat::expect_true(all(tab$ci_upper[core] >= tab$estimate[core]))
  testthat::expect_equal(dim(md$boot), c(300L, nrow(tab)))
  again <- make_multid(counts = c(G = 120, Fp = 40, Rp = 40, I = 30, Fa = 90, Ra = 80),
                       n_bootstrap = 300, seed = 42)
  testthat::expect_equal(again$indices, tab)
})

testthat::test_that("compare_multid returns differences and bootstrap tests", {
  a <- make_multid(counts = c(G = 52, Fp = 25, Rp = 24, I = 28, Fa = 26, Ra = 46),
                   n_bootstrap = 200, seed = 1)
  b <- make_multid(counts = c(G = 44, Fp = 19, Rp = 39, I = 15, Fa = 17, Ra = 68),
                   n_bootstrap = 200, seed = 2)
  cmp <- compare_multid(a, b, labels = c("sim", "seq"))
  testthat::expect_s3_class(cmp, "multid_comparison")
  testthat::expect_equal(cmp$difference, cmp$sim - cmp$seq)
  testthat::expect_true(all(c("se", "ci_lower", "ci_upper", "p_value") %in% names(cmp)))
  testthat::expect_output(print(cmp), "sim minus seq")
  testthat::expect_error(compare_multid(a, make_multid(counts = c(G = 1, Fp = 1, Rp = 1, Fa = 1, Ra = 1),
                                                       filler_rate = "per_filler")),
                         "same filler_rate")
})

testthat::test_that("compare_multid treats same-seed fits as independent samples", {
  # Codex review 2, finding 1: row-by-row differences of same-seed draws gave
  # SE .0136 and p < .0001 for GFp; the independent-sample SE is about .35.
  a <- make_multid(counts = c(G = 60, Fp = 25, Rp = 15, I = 10, Fa = 30, Ra = 60),
                   n_bootstrap = 1000, seed = 1)
  b <- make_multid(counts = c(G = 60, Fp = 27, Rp = 13, I = 10, Fa = 30, Ra = 60),
                   n_bootstrap = 1000, seed = 1)
  cmp <- compare_multid(a, b, seed = 1)
  gfp <- cmp[cmp$index == "GFp", ]
  testthat::expect_equal(gfp$se, sqrt(stats::var(a$boot[, "GFp"]) + stats::var(b$boot[, "GFp"])))
  testthat::expect_gt(gfp$se, 0.25)
  testthat::expect_gt(gfp$p_value, 0.7)
  testthat::expect_lt(gfp$ci_lower, 0)
  testthat::expect_gt(gfp$ci_upper, 0)

  # Identical data and seed: the difference is 0 but its uncertainty is not.
  same <- compare_multid(a, a, seed = 1)
  se_a <- stats::sd(a$boot[, "GFp"])
  testthat::expect_equal(same$se[same$index == "GFp"], sqrt(2) * se_a)
  testthat::expect_lt(same$ci_lower[same$index == "GFp"], -se_a)
  testthat::expect_identical(compare_multid(a, b, seed = 7), compare_multid(a, b, seed = 7))
})

testthat::test_that("multid input validation", {
  testthat::expect_error(make_multid(), "exactly one")
  testthat::expect_error(make_multid(counts = c(G = 1, Fp = 1)), "named numeric")
  testthat::expect_error(make_multid(counts = c(G = -1, Fp = 1, Rp = 1, Fa = 1, Ra = 1)),
                         "non-negative")
  testthat::expect_error(make_multid(counts = c(G = 1, Fp = 1, Rp = 1, Fa = 1, Ra = 1),
                                     lineup_size = 1), "lineup_size")
  testthat::expect_error(make_multid(counts = c(G = .5, Fp = .2, Rp = .3, I = .1, Fa = .4, Ra = .5),
                                     n_bootstrap = 10), "integer counts")
  testthat::expect_error(make_multid(counts = c(G = 1, Fp = 1, Rp = 1, Fa = 1, Ra = 1),
                                     by_confidence = TRUE), "trial-level")
})

testthat::test_that("multid print, summary and plot methods run", {
  data(lineup_example, package = "r4lineups", envir = environment())
  md <- make_multid(lineup_example, by_confidence = TRUE, n_bootstrap = 50, seed = 3)
  testthat::expect_output(print(md), "Decomposition")
  testthat::expect_output(summary(md), "Cumulative indices by confidence")
  testthat::expect_s3_class(plot(md), "ggplot")
  testthat::expect_s3_class(plot(md, type = "indices"), "ggplot")
  testthat::expect_s3_class(plot(md, type = "confidence"), "ggplot")
  no_conf <- make_multid(lineup_example)
  testthat::expect_error(plot(no_conf, type = "confidence"), "by_confidence")
})
