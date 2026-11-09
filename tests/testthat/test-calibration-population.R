test_that("calibration populations score responses as in the literature", {
  dat <- data.frame(
    target_present = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
    identification = c("suspect", "filler", "reject", "suspect", "filler", "reject"),
    confidence = 90, suspect_pos = 1
  )
  sus <- make_calibration_data(dat, confidence_scale = "0-100")
  expect_equal(sus$population, "suspect_ids")
  expect_equal(sus$n_total, 2)
  expect_equal(sus$overall_accuracy, 0.5)

  # Choosers (Brewer & Wells, 2006): target-present filler IDs are excluded;
  # the TP suspect ID is correct and both TA identifications are errors.
  cho <- make_calibration_data(dat, confidence_scale = "0-100", population = "choosers")
  expect_equal(cho$tp_fillers, "exclude")
  expect_equal(cho$n_total, 3)
  expect_equal(cho$overall_accuracy, 1 / 3)
  # tp_fillers = "error" keeps the TP filler ID as an error.
  cho_all <- make_calibration_data(dat, confidence_scale = "0-100",
                                   population = "choosers", tp_fillers = "error")
  expect_equal(cho_all$n_total, 4)
  expect_equal(cho_all$overall_accuracy, 0.25)

  # Nonchoosers: a target-absent rejection is correct.
  non <- make_calibration_data(dat, confidence_scale = "0-100", population = "nonchoosers")
  expect_equal(non$n_total, 2)
  expect_equal(non$overall_accuracy, 0.5)

  # The deprecated FALSE maps to choosers and no longer scores correct
  # rejections as errors.
  old <- suppressWarnings(make_calibration_data(dat, choosers_only = FALSE,
                                                confidence_scale = "0-100"))
  expect_equal(old$population, "choosers")
  expect_error(make_calibration_data(dat, population = "everyone"))
})

test_that("make_calibration() keeps tp_fillers and prints the population used", {
  dat <- data.frame(
    target_present = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
    identification = c("suspect", "filler", "reject", "suspect", "filler", "reject"),
    confidence = 90, suspect_pos = 1
  )
  all_ids <- make_calibration(dat, confidence_scale = "0-100", population = "choosers",
                              tp_fillers = "error", show_plot = FALSE)
  expect_equal(all_ids$tp_fillers, "error")
  expect_equal(all_ids$n_total, 4)
  expect_output(print(all_ids), "all suspect and filler IDs")
  excl <- make_calibration(dat, confidence_scale = "0-100", population = "choosers",
                           show_plot = FALSE)
  expect_equal(excl$tp_fillers, "exclude")
  expect_output(print(excl), "TP filler IDs excluded")
})

test_that("functions without a population argument do not raise the deprecation warning", {
  set.seed(1)
  dat <- data.frame(
    target_present = rep(c(TRUE, FALSE), each = 60),
    identification = sample(c("suspect", "filler", "reject"), 120, replace = TRUE),
    confidence = sample(c(20, 40, 60, 80, 100), 120, replace = TRUE),
    suspect_pos = 1
  )
  expect_no_warning(a <- compute_anri(dat, confidence_bins = c(0, 40, 60, 80, 100),
                                      choosers_only = FALSE, lineup_size = 6))
  ref <- make_calibration_data(dat, confidence_bins = c(0, 40, 60, 80, 100),
                               population = "choosers")
  expect_equal(a$n_total, ref$n_total)
  expect_output(print(structure(a, class = c("lineup_anri", "list"))),
                "Choosers \\(target-present filler IDs excluded\\)")
})
