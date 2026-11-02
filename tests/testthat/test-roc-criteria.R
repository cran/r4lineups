test_that("rejection confidence does not reweight ROC summaries", {
  # Codex review 2, finding 5: recoding only the rejection confidences changed
  # average DPP from .417 to .647 and z-ROC d' from 1.879 to 1.398.
  base <- data.frame(
    target_present = rep(c(TRUE, FALSE), each = 100),
    identification = c(rep(c("suspect", "reject"), c(80, 20)),
                       rep(c("suspect", "reject"), c(10, 90))),
    confidence = c(rep(c(100, 50), 40), rep(0, 20),
                   rep(c(100, 50), 5), rep(0, 90))
  )
  recoded <- base
  recoded$confidence[recoded$identification == "reject"] <- seq(51, 99, length.out = 110)

  dpp0 <- make_dpp(base)
  dpp1 <- make_dpp(recoded)
  expect_equal(nrow(dpp1$dpp_by_point), 2)
  expect_equal(dpp1$dpp, dpp0$dpp)
  expect_equal(dpp0$dpp, mean(c(1 - .4 + .05, 1 - .8 + .1)))
  expect_equal(make_rocdata(recoded)$thresholds, c(100, 50))
  expect_equal(make_rocdata(recoded)$pauc, make_rocdata(base)$pauc)

  z0 <- fit_sdt_roc(base, lineup_size = 6, bootstrap = FALSE)
  z1 <- fit_sdt_roc(recoded, lineup_size = 6, bootstrap = FALSE)
  expect_equal(z1$dprime, z0$dprime)

  u0 <- make_utility_curves(base)
  u1 <- make_utility_curves(recoded)
  expect_equal(u1$avg_utility, u0$avg_utility)
  expect_equal(nrow(u1$utility_data), 3)

  # Filler-ID confidence defines criteria only when it estimates false IDs.
  est <- recoded
  est$identification[est$target_present == FALSE & est$identification == "suspect"] <- "filler"
  roc_est <- suppressMessages(make_rocdata(est, innocent_suspect = "estimated"))
  expect_equal(roc_est$thresholds, c(100, 50))

  # A supplied grid is used as given, and bootstraps reuse the full-sample one.
  expect_equal(make_rocdata(base, thresholds = c(50, 100, 75))$thresholds, c(100, 75, 50))
  expect_error(make_rocdata(base, thresholds = numeric()), "thresholds")
  cmp <- compare_dpp(base, recoded, n_bootstrap = 50, seed = 1)
  expect_equal(cmp$dpp_difference, 0)
  expect_equal(cmp$dpp_obj_b$thresholds, c(100, 50))
})

test_that("data with no suspect IDs still give a single (0, 0) criterion", {
  d <- data.frame(
    target_present = rep(c(TRUE, FALSE), each = 4),
    identification = c("filler", "reject", "filler", "reject",
                       "reject", "reject", "reject", "reject"),
    confidence = c(90, 80, 70, 60, 50, 40, 30, 20),
    suspect_pos = 1
  )
  roc <- make_rocdata(d)
  expect_equal(roc$thresholds, 90)
  expect_equal(make_dpp(d)$dpp, 1)
})
