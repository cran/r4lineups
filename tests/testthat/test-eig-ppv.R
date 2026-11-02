# Tests for Expected Information Gain and PPV-range analyses

test_that("EIG is ~0 for uninformative responses", {
  n <- 400
  set.seed(1)
  ids <- sample(c("suspect", "filler", "reject"), n / 2, replace = TRUE)
  confs <- sample(c(30, 60, 90), n / 2, replace = TRUE)
  data <- data.frame(
    target_present = rep(c(TRUE, FALSE), each = n / 2),
    identification = rep(ids, 2),
    confidence = rep(confs, 2)
  )
  result <- compute_eig(data, prior_guilt = 0.5)
  expect_lt(result$eig, 0.01)
})

test_that("EIG equals prior entropy (1 bit) for perfect separation at prior 0.5", {
  n <- 200
  data <- data.frame(
    target_present = rep(c(TRUE, FALSE), each = n / 2),
    identification = rep(c("suspect", "reject"), each = n / 2),
    confidence = rep(90, n)
  )
  result <- compute_eig(data, prior_guilt = 0.5)
  expect_equal(result$eig, 1, tolerance = 1e-8)
  expect_equal(result$prior_entropy, 1, tolerance = 1e-8)
})

test_that("EIG is bounded by prior entropy and non-negative", {
  data(lineup_example)
  result <- compute_eig(lineup_example, confidence_bins = c(0, 60, 80, 100))
  expect_gte(result$eig, 0)
  expect_lte(result$eig, result$prior_entropy + 1e-10)
})

test_that("ppv corrections are ordered: none <= effective <= nominal for fair lineups", {
  data(lineup_example)
  lineup_example$identification[
    !lineup_example$target_present & lineup_example$identification == "suspect"
  ] <- "filler"
  bins <- c(0, 60, 80, 100)
  ppv_none <- ppv_by_confidence(lineup_example, correction = "none",
                                confidence_bins = bins)
  ppv_eff <- ppv_by_confidence(lineup_example, correction = "effective",
                               confidence_bins = bins)
  ppv_nom <- ppv_by_confidence(lineup_example, correction = "nominal",
                               confidence_bins = bins)

  # "none" treats every mistaken ID as an innocent-suspect ID (worst case),
  # so it gives the lowest PPV; nominal spreads errors over the full lineup
  # (best case); effective sits between them.
  expect_lte(ppv_none$overall_ppv, ppv_eff$overall_ppv)
  expect_lte(ppv_eff$overall_ppv, ppv_nom$overall_ppv + 1e-10)
})

test_that("effective correction computes a plausible effective size (regression)", {
  # Regression for the bug where the fallback tabulated the values of the
  # per-position count vector, collapsing E' to ~1.38 regardless of data.
  data(lineup_example)
  lineup_example$identification[
    !lineup_example$target_present & lineup_example$identification == "suspect"
  ] <- "filler"
  ppv_eff <- ppv_by_confidence(lineup_example, correction = "effective",
                               confidence_bins = c(0, 60, 80, 100))
  esizes <- ppv_eff$ppv_data$effective_size
  esizes <- esizes[!is.na(esizes)]
  expect_true(all(esizes > 2))
  expect_true(all(esizes <= 6 + 1e-10))
})

test_that("designated innocent-suspect IDs supersede size approximations", {
  data(lineup_example)
  bins <- c(0, 60, 80, 100)
  nominal <- ppv_by_confidence(lineup_example, correction = "nominal",
                               confidence_bins = bins)
  effective <- ppv_by_confidence(lineup_example, correction = "effective",
                                 confidence_bins = bins)
  uncorrected <- ppv_by_confidence(lineup_example, correction = "none",
                                   confidence_bins = bins)
  expect_equal(nominal$overall_ppv, effective$overall_ppv)
  expect_equal(effective$overall_ppv, uncorrected$overall_ppv)
  expect_equal(nominal$innocent_suspect_method, "designated")
})

test_that("EIG splits filler outcomes when no innocent suspect is designated", {
  dat <- data.frame(
    target_present = c(TRUE, TRUE, FALSE, FALSE),
    identification = c("suspect", "reject", "filler", "reject"),
    confidence = rep(80, 4)
  )
  prepared <- make_eig_data(dat, lineup_size = 4)
  suspect <- prepared$response_data[
    prepared$response_data$identification == "suspect", ]
  filler <- prepared$response_data[
    prepared$response_data$identification == "filler", ]
  expect_equal(suspect$n_innocent, 0.25)
  expect_equal(filler$n_innocent, 0.75)
  expect_equal(sum(prepared$response_data$p_x_given_innocent), 1)
  expect_equal(prepared$innocent_suspect_method, "estimated_from_fillers")

  bayes <- make_bayes_curves(dat, lineup_size = 4, prior_grid = 0.5)
  suspect_lik <- bayes$likelihoods[
    bayes$likelihoods$response == "suspect", ]
  expect_equal(suspect_lik$n_innocent, 0.25)
  expect_equal(sum(bayes$likelihoods$p_x_given_innocent), 1)
})

test_that("innocent_id_rate helpers compute the documented formulas", {
  expect_equal(innocent_id_rate_nominal(0.30, 6), 0.05)
  expect_equal(innocent_id_rate_effective(0.30, 4), 0.075)
  expect_equal(innocent_id_rate_uncorrected(0.30), 0.30)
  expect_error(innocent_id_rate_nominal(0.30, 0))
  expect_error(innocent_id_rate_effective(0.30, 0))
})

test_that("ppv_range_by_confidence returns all three corrections", {
  data(lineup_example)
  result <- ppv_range_by_confidence(lineup_example,
                                    confidence_bins = c(0, 60, 80, 100))
  expect_true(all(c("ppv_nominal", "ppv_effective", "ppv_none",
                    "ppv_range_data") %in% names(result)))
  expect_true(all(c("ppv_nominal", "ppv_effective", "ppv_none") %in%
                    names(result$ppv_range_data)))
})

test_that("PPV keeps confidence bins that contain no target-absent trials", {
  # All 90 TP suspect IDs at 100; the 10 TA suspect IDs and all rejections at 50
  x <- data.frame(
    target_present = rep(c(TRUE, FALSE), each = 100),
    identification = c(rep("suspect", 100), rep("suspect", 10), rep("reject", 90)),
    confidence = c(rep(100, 90), rep(50, 110))
  )
  ppv <- ppv_by_confidence(x, confidence_bins = c(0, 60, 80, 100), quiet = TRUE)
  expect_equal(ppv$overall_ppv, 100 / 110)
  expect_equal(ppv$overall_ppv, make_cacdata(x)$overall_accuracy)
  top <- ppv$ppv_data[ppv$ppv_data$confidence == "(80,100]", ]
  expect_equal(top$ppv, 1)
  expect_true(is.na(top$error_rate))
  # The empty middle bin is still dropped
  expect_false("(60,80]" %in% ppv$ppv_data$confidence)

  # A TA-only bin has PPV 0
  y <- x
  y$confidence[y$target_present & y$confidence == 50] <- 100
  y$confidence[!y$target_present & y$identification == "suspect"] <- 70
  ppv_y <- ppv_by_confidence(y, confidence_bins = c(0, 60, 80, 100), quiet = TRUE)
  expect_equal(ppv_y$ppv_data$ppv[ppv_y$ppv_data$confidence == "(60,80]"], 0)

  # Unbinned PPV agrees with the overall counts
  expect_equal(ppv_by_confidence(x, quiet = TRUE)$overall_ppv, 100 / 110)

  # The range and its plots cope with the NA rows
  rng <- suppressMessages(make_ppv_range(x, confidence_bins = c(0, 60, 80, 100),
                                         show_plots = FALSE))
  expect_s3_class(plot_error_rate_conf(rng), "ggplot")
  expect_s3_class(plot_ppv_range(rng), "ggplot")
})
