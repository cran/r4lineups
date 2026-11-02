# CAC accuracy and PPV from counts depend on the TP:TA split; base_rate computes
# them from rates instead (Mickes, 2015, assumes equal arms).

# HR = .6 at confidence 90, FAR = .1 (designated suspect) at confidence 90
rate_data <- function(n_tp, n_ta) {
  data.frame(
    target_present = rep(c(TRUE, FALSE), c(n_tp, n_ta)),
    identification = c(rep(c("suspect", "reject"), c(0.6 * n_tp, 0.4 * n_tp)),
                       rep(c("suspect", "reject"), c(0.1 * n_ta, 0.9 * n_ta))),
    confidence = c(rep(c(90, 30), c(0.6 * n_tp, 0.4 * n_tp)),
                   rep(c(90, 30), c(0.1 * n_ta, 0.9 * n_ta)))
  )
}

test_that("count-based CAC shifts with the TP:TA split and says so", {
  balanced <- make_cacdata(rate_data(100, 100))
  expect_equal(balanced$overall_accuracy, .6 / .7)
  expect_null(balanced$base_rate)
  expect_message(unbal <- make_cacdata(rate_data(200, 100)), "differ in size")
  expect_equal(unbal$overall_accuracy, 120 / 130)
  expect_silent(make_cacdata(rate_data(100, 100)))
})

test_that("base_rate gives the rate-based posterior whatever the sample sizes", {
  for (n in list(c(100, 100), c(200, 100), c(100, 300))) {
    d <- rate_data(n[1], n[2])
    cac <- make_cacdata(d, base_rate = 0.5)
    expect_equal(cac$overall_accuracy, .6 / .7)
    expect_equal(cac$cac_data$accuracy[cac$cac_data$confidence == "90"], .6 / .7)
    ppv <- ppv_by_confidence(d, base_rate = 0.5, quiet = TRUE)
    expect_equal(ppv$overall_ppv, .6 / .7)
    expect_equal(cac$overall_accuracy, ppv$overall_ppv)
  }
  # Another prior matches Bayes' rule by hand
  cac <- make_cacdata(rate_data(200, 100), base_rate = 0.2)
  expect_equal(cac$overall_accuracy, .2 * .6 / (.2 * .6 + .8 * .1))
  expect_equal(cac$base_rate, 0.2)
  # Balanced data: base_rate = .5 leaves the count result unchanged
  d <- rate_data(100, 100)
  expect_equal(make_cacdata(d, base_rate = 0.5)$cac_data$accuracy,
               make_cacdata(d)$cac_data$accuracy)
})

test_that("base_rate works without a designated suspect", {
  # 60 TA filler IDs in 6-person lineups -> estimated FAR = .1
  d <- data.frame(
    target_present = rep(c(TRUE, FALSE), c(200, 100)),
    identification = c(rep(c("suspect", "reject"), c(120, 80)),
                       rep(c("filler", "reject"), c(60, 40))),
    confidence = c(rep(c(90, 30), c(120, 80)), rep(c(90, 30), c(60, 40)))
  )
  cac <- make_cacdata(d, innocent_suspect = "estimated", base_rate = 0.5)
  expect_equal(cac$overall_accuracy, .6 / .7)
  ppv <- ppv_by_confidence(d, innocent_suspect = "estimated", base_rate = 0.5,
                           quiet = TRUE)
  expect_equal(ppv$overall_ppv, .6 / .7)
  rng <- ppv_range_by_confidence(d, innocent_suspect = "estimated",
                                 base_rate = 0.5)
  expect_equal(rng$ppv_nominal$overall_ppv, .6 / .7)
  expect_equal(rng$base_rate, 0.5)
})

test_that("rate-based CAC SE is the delta-method SE", {
  d <- rate_data(200, 100)
  se <- make_cacdata(d, base_rate = 0.5)$cac_data
  se <- se$se[se$confidence == "90"]
  hr <- .6; far <- .1; a <- .5 * hr; b <- .5 * far
  expected <- sqrt((.5 * b / (a + b)^2)^2 * hr * (1 - hr) / 200 +
                   (.5 * a / (a + b)^2)^2 * far * (1 - far) / 100)
  expect_equal(se, expected)
  # Monte Carlo check of the approximation
  set.seed(1)
  hr_s <- rbinom(2e4, 200, hr) / 200
  far_s <- rbinom(2e4, 100, far) / 100
  expect_equal(sd(hr_s / (hr_s + far_s)), expected, tolerance = 0.1)
})

test_that("base_rate is validated and passed through the wrappers", {
  d <- rate_data(200, 100)
  expect_error(make_cacdata(d, base_rate = 1), "base_rate")
  expect_error(ppv_by_confidence(d, base_rate = c(.2, .3)), "base_rate")
  cac <- make_cac(d, base_rate = 0.5, show_plot = FALSE)
  expect_equal(cac$overall_accuracy, .6 / .7)
  expect_output(print(cac), "base rate: 0.5")
  rng <- make_ppv_range(d, base_rate = 0.5, show_plots = FALSE)
  expect_equal(rng$ppv_none$overall_ppv, .6 / .7)
  expect_message(make_ppv_range(d, show_plots = FALSE), "differ in size")
})
