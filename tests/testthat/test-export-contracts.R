audit_choice_data <- function() {
  c(1, 1, 1, 2, 2, 3, 4, 4, 5, 6, 1, 3)
}

audit_confidence_data <- function() {
  data(lineup_example, package = "r4lineups", envir = environment())
  x <- lineup_example
  x$condition <- rep(c("A", "B"), length.out = nrow(x))
  x
}

test_that("remaining core fairness exports have behavioral contracts", {
  x <- audit_choice_data()
  tab <- table(factor(x, levels = 1:6))
  expect_equal(makevec_prop(0.25, 20), c(rep(1, 5), rep(0, 15)))
  expect_equal(rep_index(3, 4), rep(3, 4))
  expect_equal(rot_vector(1:4), c(4, 1, 2, 3))
  expect_silent(datacheck3(tab, 6))
  expect_error(datacheck3(c(a = -1), 1), "counts")

  set.seed(2)
  boot_df <- gen_boot_samples(x, 12)
  expect_equal(dim(boot_df), c(length(x), 12))
  boot_list <- gen_boot_samples_list(list(x, rev(x)), 4)
  expect_equal(length(boot_list), 2)
  expect_equal(ncol(boot_list[[1]]), 4)
  expect_length(gen_lineup_prop(boot_df, 1, 6), 12)
  expect_length(gen_esize_m(boot_df, 6), 12)
  expect_length(gen_esize_m_ci(gen_esize_m(boot_df, 6), 0.5), 1)
  expect_equal(gen_linevec(tab, 6), x[order(x)])

  expect_equal(lineup_prop_boot(x, seq_along(x), 1), mean(x == 1))
  expect_equal(func_size.boot(x, seq_along(x), 1), 1 / mean(x == 1))
  expect_equal(i_esize_T(tab), 1 - sum(tab^2) / sum(tab)^2)
  ci <- esize_T_ci_n(tab, 0.95)
  expect_true(ci$ci_low <= ci$ci_high)
  expect_error(esize_T_ci_n(tab, 1), "alpha")

  expect_output(fs <- suppressWarnings(func_size_report(x, 1, 6, R = 30)),
                "Functional size")
  expect_equal(fs$R, 30)
  expect_equal(nrow(lineup_boot_allprop(x, 6, R = 30)), 6)
  expect_length(suppressWarnings(eff_size_per_foils(x, 1:6, 6, R = 30)), 1)
  expect_length(allfoil_cihigh(c(0.5, 0.3, 0.2), 20), 3)
  expect_equal(compare_eff_sizes.boot(data.frame(x, rev(x)), seq_along(x)), 0)
  expect_output(ec <- effsize_compare(data.frame(x, rev(x)), R = 30), "Effective sizes")
  expect_equal(ec$R, 30)
  expect_s3_class(plot_lineup_bias_distribution(c(0.1, 0.2, 0.3), 1), "ggplot")
  expect_s3_class(plot_esize_distribution(c(3.5, 4, 4.5), "tredoux"), "ggplot")
})

test_that("legacy diagnosticity exports match hand calculations", {
  tp <- c(1, 1, 1, 2, 2, 3, 4, 5)
  ta <- c(1, 2, 2, 2, 3, 4, 5, 6)
  expect_equal(diag_ratio_W(tp, ta, 1, 2, 6, 6), (3 / 8) / (3 / 8))
  expect_equal(var_diag_ratio(tp, ta, 1, 2, 6, 6),
               5 / (3 * 8) + 5 / (3 * 8))
  expect_equal(rep_index(2, 3), c(2, 2, 2))
})

test_that("calibration, ANRI, EIG, PPV, and decision wrappers compose", {
  x <- audit_confidence_data()
  bins <- c(0, 60, 80, 100)

  cal_data <- make_calibration_data(x, bins)
  expect_true(is.list(cal_data))
  cal <- make_calibration(x, bins, show_plot = FALSE)
  expect_s3_class(cal, "lineup_calibration")
  expect_s3_class(make_calibration_gg(cal), "ggplot")
  by_cond <- make_calibration_by_condition(x, "condition", bins)
  expect_equal(nrow(by_cond$condition_summary), 2)
  expect_s3_class(make_calibration_by_condition_gg(by_cond), "ggplot")

  anri <- compute_anri(x, bins)
  expect_true(is.finite(anri$anri))
  boot_anri <- bootstrap_anri(x, bins, n_bootstrap = 20, seed = 4)
  expect_equal(boot_anri$n_bootstrap, 20)
  cmp_anri <- compare_anri(x, "condition", bins, n_bootstrap = 20, seed = 4)
  expect_s3_class(plot_anri_comparison(cmp_anri), "ggplot")
  expect_s3_class(plot_anri_difference_distribution(cmp_anri), "ggplot")

  eig_data <- make_eig_data(x, bins)
  expect_true(nrow(eig_data$response_data) > 0)
  eig <- make_eig(x, confidence_bins = bins, show_plot = FALSE)
  expect_s3_class(eig, "lineup_eig")
  expect_s3_class(plot_eig(eig), "ggplot")
  expect_s3_class(plot_eig_posteriors(eig), "ggplot")

  ppv <- make_ppv_range(x, confidence_bins = bins, show_plots = FALSE)
  expect_s3_class(ppv, "lineup_ppv_range")
  expect_s3_class(plot_ppv_range(ppv), "ggplot")
  expect_s3_class(plot_effective_size_conf(ppv), "ggplot")
  expect_s3_class(plot_error_rate_conf(ppv), "ggplot")
})

test_that("Bayes, utility, and DPP wrapper exports compose", {
  x <- audit_confidence_data()
  x_a <- x[x$condition == "A", ]
  x_b <- x[x$condition == "B", ]
  bins <- c(0, 60, 80, 100)

  bayes <- make_bayes_curves(x, "confidence", bins, prior_grid = c(0.2, 0.5, 0.8))
  expect_s3_class(plot_bayes_prior_posterior(bayes), "ggplot")
  expect_s3_class(plot_bayes_information_gain(bayes), "ggplot")
  bree <- make_bree_curve(x, x, prior_grid = seq(0.1, 0.9, by = 0.2))
  expect_s3_class(plot_bree(bree), "ggplot")

  util <- make_utility_curves(x)
  expect_s3_class(plot_utility_curves(util), "ggplot")
  util_diff <- make_utility_difference(x_a, x_b, base_rate_grid = c(0.2, 0.5, 0.8))
  expect_s3_class(plot_utility_difference(util_diff), "ggplot")
  cmp_util <- compare_utility(x_a, x_b, base_rate_grid = c(0.2, 0.5, 0.8),
                              show_plot = FALSE)
  expect_true("difference_curve" %in% names(cmp_util))

  dpp <- compute_dpp(x, show_plot = FALSE)
  expect_s3_class(dpp, "lineup_dpp")
  expect_s3_class(plot_dpp(dpp), "ggplot")
  cmp_dpp <- compare_dpp(x_a, x_b, n_bootstrap = 20, seed = 1)
  expect_s3_class(plot_dpp_comparison(cmp_dpp), "ggplot")
})

test_that("ROC-family plot helpers and full ROC plot are callable", {
  x <- audit_confidence_data()
  x$response_time <- seq_len(nrow(x)) / 10
  roc <- make_rocdata(x)
  cac <- make_cacdata(x)
  rac <- make_racdata(x)
  expect_s3_class(make_roc_gg(roc), "ggplot")
  expect_s3_class(make_cac_gg(cac), "ggplot")
  expect_s3_class(make_rac_gg(rac), "ggplot")
  full <- make_fullroc(x, show_plot = FALSE)
  expect_s3_class(plot_fullroc(full), "ggplot")
})

test_that("summary-level SDT exports agree with their returned variance", {
  s <- sdt_summary_from_counts(40, 10, 10, 40)
  expect_true(all(is.finite(unlist(s[c("dprime", "criterion", "beta")]))) )
  v <- sdt_summary_variance(40, 10, 10, 40, method = "gourevitch")
  expect_true(v$var_dprime > 0)
  cmp <- compare_sdt_summary(40, 10, 10, 40, 35, 15, 15, 35,
                             metric = "dprime", method = "gourevitch")
  expect_true(is.finite(cmp$z))
})

test_that("mSDT distribution and inverse estimators have exact contracts", {
  set.seed(8)
  draws <- rmax_filler(20, 6)
  expect_length(draws, 20)
  gamma <- msdt_gamma_from_rej(0.5^6, 6)
  expect_equal(gamma, 0, tolerance = 1e-10)
  expect_equal(msdt_dprime_from_rej(stats::pnorm(-1) * 0.5^5, gamma, 6),
               1, tolerance = 1e-8)
  expect_s3_class(plot_msdt_joint(1, 0, 6, n = 30), "ggplot")
})

test_that("face-vector helpers work without optional Python", {
  expect_true("ArcFace" %in% available_models())
  expect_true("retinaface" %in% available_detectors())
  empty <- batch_embeddings(character(), progress = FALSE)
  expect_equal(nrow(empty), 0)
  expect_error(get_embedding("file-that-does-not-exist.jpg"), "not found")
  expect_error(face_similarity("missing-a.jpg", "missing-b.jpg"), "not found")
  expect_error(lineup_similarity("missing.jpg", "also-missing.jpg"), "not found")
  expect_error(lineup_pairwise_matrix("missing.jpg", "also-missing.jpg"), "not found")
  expect_error(detect_faces("missing.jpg"), "not found")
  expect_error(get_face_landmarks("missing.jpg"), "not found")
  expect_error(get_key_landmarks("missing.jpg"), "not found")
  expect_false(has_valid_face("missing.jpg"))

  lineup <- data.frame(foil_name = c("a", "b"), similarity = c(0.7, 0.2),
                       distance = c(0.3, 0.8), rank = 1:2)
  class(lineup) <- c("lineup_similarity", "data.frame")
  attr(lineup, "threshold") <- 0.5
  attr(lineup, "model") <- "test"
  attr(lineup, "metric") <- "cosine"
  expect_s3_class(plot_lineup_similarity(lineup), "ggplot")
  expect_s3_class(plot_similarity_distribution(c(0.1, 0.4, 0.7)), "ggplot")
  emb <- data.frame(name = letters[1:4])
  emb$embedding <- list(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1), c(1, 1, 0))
  expect_s3_class(plot_embedding_space(emb, method = "pca", labels = emb$name), "ggplot")
})

test_that("optional environment entry points have explicit CRAN exemptions", {
  # These are deliberately not launched during package checks: they install
  # external Python software, open an interactive Shiny session, render image
  # grids, or require lme4 mixed-model data. Their validation/error paths and
  # non-environment-dependent helpers are tested above.
  expect_true(is.function(install_r4lineups_python))
  expect_s3_class(check_python_deps(verbose = FALSE), "data.frame")
  expect_true(is.function(run_r4lineups_app))
  expect_true(is.function(display_lineup))
  expect_true(is.function(fit_sdt_glmm))
  expect_true(is.function(extract_sdt_metrics))
})

test_that("Winter bootstrap export returns finite intervals", {
  counts <- c(n_tp_suspect = 60, n_tp_filler = 25, n_tp_reject = 15,
              n_ta_suspect = 12, n_ta_filler = 38, n_ta_reject = 50)
  fit <- fit_winter_2ht(counts)
  boot <- boot_winter_2ht(fit, nboot = 20, seed = 3)
  expect_s3_class(boot, "winter_2ht_boot")
  expect_true(all(is.finite(boot$ci)))
})

test_that("func_size_report keeps resamples with no target choices", {
  # Codex review 2, finding 3: boot.ci() dropped the infinite replicates of
  # 1/p, giving a percentile interval of [10, 30] for one choice in 30.
  x <- c(1, rep(2:6, length.out = 29))
  set.seed(1)
  expect_output(fs <- func_size_report(x, target_pos = 1, k = 6, R = 1000),
                "no target choices")
  expect_gt(fs$n_infinite, 300)
  expect_equal(fs$n_infinite, sum(fs$boot$t[, 1] == 0))
  expect_equal(fs$percentile[2], Inf)
  expect_equal(fs$bca[2], Inf)
  expect_equal(fs$exact, 1 / rev(stats::binom.test(1, 30)$conf.int))
  expect_equal(fs$exact, c(5.81, 1185.44), tolerance = 1e-3)

  # With no resample lacking target choices, the limits are those of 1/p.
  y <- rep(1:3, 20)
  set.seed(2)
  expect_output(fs <- func_size_report(y, 1, 3, R = 200), "Exact")
  expect_equal(fs$n_infinite, 0)
  expect_equal(fs$percentile,
               unname(stats::quantile(1 / fs$boot$t[, 1], c(0.025, 0.975), type = 6)),
               tolerance = 0.05)
  expect_true(fs$percentile[1] < 3 && fs$percentile[2] > 3)
})
