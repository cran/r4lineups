test_that("legacy lineup helpers retain integer vectors and allow unchosen members", {
  choices <- c(1L, 1L, 3L, 3L)
  expect_equal(lineup_prop_vec(choices, 3, 4), 0.5)
  expect_silent(datacheck1(choices, 4))
  expect_equal(allprop(choices, 4)$prop, c(0.5, 0, 0.5, 0))
  expect_error(datacheck1(c(1, 5), 4), "between 1 and k")
})

test_that("named count tables use member labels rather than storage positions", {
  tab <- structure(c(4L, 6L), names = c("1", "3"), class = "table")
  expect_equal(lineup_prop_tab(tab, 2), 0)
  expect_equal(lineup_prop_tab(tab, 3), 0.6)
  expect_equal(allfoilbias(tab, target_pos = 1, k = 3),
               c(member_1 = 0.4, member_2 = 0, member_3 = 0.6))
})

test_that("Malpass estimators use complete nominal counts and bootstrap witnesses", {
  choices <- c(rep(1, 6), rep(2, 3), 3)
  counts <- table(factor(choices, levels = 1:4))
  expected <- 4 - sum(abs(as.numeric(counts) - 2.5) / 5)
  expect_equal(esize_m(counts, 4), expected)
  expect_equal(esize_m(choices, 4), expected)
  expect_equal(esize_m_boot(choices, seq_along(choices), 4), expected)
  expect_equal(esize_T_boot(choices, seq_along(choices)), esize_T(table(choices)))
})

test_that("bootstrap SD is reported as the statistic standard error", {
  draws <- c(0.1, 0.2, 0.3, 0.4)
  result <- expect_output(gen_boot_propmean_se(draws), "SE of boot prop")
  result <- suppressWarnings(capture.output(obj <- gen_boot_propmean_se(draws)))
  expect_equal(obj$se, stats::sd(draws))
  expect_error(gen_boot_propci(draws, 2), "between 0 and 1")
})

test_that("diagnosticity parameters use designated scalar suspect positions", {
  tp <- list(c(1, 2, 2, 3), c(1, 1, 2))
  ta <- list(c(1, 2, 3, 3), c(1, 2, 2))
  params <- diag_param(tp, ta, pos_list = list(c(2, 3), 1), k = c(3, 2))
  expect_equal(params$n11, c(2, 2))
  expect_equal(params$n12, c(2, 1))
  expected_var <- c(
    1 / 2 - 1 / 4 + 1 / 2 - 1 / 4,
    1 / 2 - 1 / 3 + 1 / 1 - 1 / 3
  )
  expect_equal(var_lnd(params)$var, expected_var)
  expect_equal(d_weights(params)$wi, 1 / expected_var)
  expect_error(var_lnd(data.frame(n11 = 0, n21 = 4, n12 = 1, n22 = 3)),
               "positive suspect-ID")
})

test_that("homogeneity uses number of lineup pairs for degrees of freedom", {
  tp <- list(rep(c(1, 2), c(20, 80)), rep(c(1, 2), c(30, 70)))
  ta <- list(rep(c(1, 2), c(10, 90)), rep(c(1, 2), c(15, 85)))
  out <- capture.output(result <- homog_diag(tp, ta, pos_list = c(1, 1), k = c(2, 2)))
  expect_equal(result$df, 1L)
  expect_match(paste(out, collapse = " "), "Mean diagnosticity ratio")
  expect_error(homog_diag_boot(tp, ta, k = c(2, 2), R = 10), "pos_list is required")
  set.seed(1)
  boot <- capture.output(boot_result <- homog_diag_boot(
    tp, ta, k = c(2, 2), R = 20, pos_list = c(1, 1), seed = 1
  ))
  expect_equal(nrow(boot_result$draws), 20)
})

test_that("ROC-family estimators do not double count fillers with a designated suspect", {
  dat <- data.frame(
    target_present = c(TRUE, TRUE, FALSE, FALSE, FALSE),
    identification = c("suspect", "filler", "suspect", "filler", "filler"),
    confidence = c(90, 80, 90, 90, 80),
    response_time = c(1000, 2000, 1000, 1000, 2000)
  )
  roc <- make_rocdata(dat, lineup_size = 3)
  expect_equal(roc$innocent_suspect_method, "designated")
  # Confidence 80 belongs only to filler IDs, which cannot move this ROC.
  expect_equal(roc$thresholds, 90)
  expect_equal(utils::tail(roc$roc_data$false_id_rate, 1), 1 / 3)
  expect_equal(roc$roc_data[1, c("correct_id_rate", "false_id_rate")],
               tibble::tibble(correct_id_rate = 0, false_id_rate = 0))

  cac <- make_cacdata(dat, lineup_size = 3)
  rac <- make_racdata(dat, lineup_size = 3)
  expect_equal(cac$overall_accuracy, 0.5)
  expect_equal(rac$overall_accuracy, 0.5)
})

test_that("filler correction is used only when no designated suspect exists", {
  dat <- data.frame(
    target_present = c(TRUE, TRUE, FALSE, FALSE),
    identification = c("suspect", "filler", "filler", "reject"),
    confidence = c(90, 80, 90, 80)
  )
  roc <- make_rocdata(dat, lineup_size = 4)
  expect_equal(roc$innocent_suspect_method, "estimated_from_fillers")
  expect_equal(max(roc$roc_data$false_id_rate), 0.125)
})

test_that("utility all-IDs value uses the lowest confidence threshold", {
  dat <- data.frame(
    target_present = c(TRUE, TRUE, FALSE, FALSE),
    identification = c("suspect", "suspect", "suspect", "reject"),
    confidence = c(90, 50, 90, 50)
  )
  util <- make_utility_curves(dat)
  confidence_rows <- util$utility_data$criterion %in% c(90, 50)
  expect_equal(util$utility_all_ids,
               tail(util$utility_data$expected_utility[confidence_rows], 1))
  util_all <- make_utility_curves(dat, criteria = "all")
  expect_equal(nrow(util_all$utility_data), 2)
})

test_that("pAUC cutoff interpolation is independent of row order", {
  roc <- data.frame(
    confidence = c(1, 2, 3),
    correct_id_rate = c(0, 0.8, 0.4),
    false_id_rate = c(0, 0.4, 0.2),
    n_correct_ids = c(0, 8, 4),
    n_false_ids = c(0, 4, 2)
  )
  expected <- 0.5 * 0.3 * 0.6
  expect_equal(r4lineups:::.compute_pauc_with_cutoff(roc, 0.3), expected)
  expect_equal(r4lineups:::.compute_pauc_with_cutoff(roc[c(3, 1, 2), ], 0.3), expected)
})

test_that("embedding distances implement standard finite-vector identities", {
  expect_equal(embedding_distance(c(1, 0), c(1, 0), "cosine"), 0)
  expect_equal(embedding_distance(c(1, 0), c(-1, 0), "cosine"), 2)
  expect_equal(cosine_to_similarity(c(0, 1, 2)), c(1, 0, -1))
  expect_error(embedding_distance(c(0, 0), c(1, 0), "cosine"), "zero vectors")
})

test_that("SDT GLM rejects ambiguous factor coding", {
  dat <- data.frame(is_old = factor(c("new", "old")), said_old = c(0, 1))
  expect_error(fit_sdt_glm(dat, "is_old", "said_old"), "0/1")
})

test_that("unequal-variance zROC reports symmetric d-a", {
  fit <- list(slope = 0.8, intercept = 1.2)
  zdat <- data.frame(z_hr = c(0, 1), z_far = c(-1, 0))
  params <- r4lineups:::.extract_sdt_parameters(fit, zdat, "unequal_variance")
  expect_equal(params$dprime, sqrt(2) * 1.2 / sqrt(1 + 0.8^2))
})

test_that("seed arguments reproduce results without changing caller RNG", {
  set.seed(91)
  before <- .Random.seed
  a <- simulate_lineup_data(20, 20, seed = 7)
  expect_equal(.Random.seed, before)
  b <- simulate_lineup_data(20, 20, seed = 7)
  expect_equal(a, b)

  x <- data.frame(
    target_present = rep(c(TRUE, FALSE), each = 20),
    identification = rep(c("suspect", "filler", "reject", "suspect"), 10),
    confidence = rep(c(90, 70, 50, 30), 10)
  )
  set.seed(92)
  before <- .Random.seed
  suppressMessages(compare_pauc(x, x, n_bootstrap = 10, seed = 4))
  expect_equal(.Random.seed, before)
})

test_that("legacy proportion and quantile helpers reject silent truncation", {
  expect_identical(makevec_prop(0.25, 20), c(rep(1L, 5), rep(0L, 15)))
  expect_error(makevec_prop(0.26, 20), "whole number")
  expect_error(makevec_prop(NA_real_, 20), "finite")

  draws <- c(2, 3, 4, 5)
  expect_equal(gen_esize_m_ci(draws, 0.5), stats::quantile(draws, 0.5))
  expect_error(gen_esize_m_ci(draws, 1.1), "between 0 and 1")
  expect_error(gen_esize_m_ci(numeric(), 0.5), "non-empty")
})

test_that("every utility criterion, reject-all included, reproduces its rates", {
  # Third review, finding 4: reject-all was stored at min(confidence) - 1, and
  # applying "confidence >= criterion" there accepted every identification.
  dat <- data.frame(
    target_present = rep(c(TRUE, FALSE), each = 20),
    identification = c(rep("suspect", 16), rep("reject", 4),
                       rep("suspect", 2), rep("reject", 18)),
    confidence = 80
  )
  util <- make_utility_curves(dat, base_rate = .01)
  expect_equal(util$max_utility$strategy, "reject_all")
  expect_equal(unname(util$max_utility$expected_utility), .49)

  tp <- dat[dat$target_present, ]
  ta <- dat[!dat$target_present, ]
  for (i in seq_len(nrow(util$utility_data))) {
    crit <- util$utility_data$criterion[i]
    expect_equal(util$utility_data$hit_rate[i],
                 mean(tp$identification == "suspect" & tp$confidence >= crit))
    expect_equal(util$utility_data$false_alarm_rate[i],
                 mean(ta$identification == "suspect" & ta$confidence >= crit))
  }
})
