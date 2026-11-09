# Primary-source benchmarks. See notes/validation-2.3.1-2026-11-07/models/
# source-record.md for access, conventions and independent calculations.

test_that("max-filler moments reproduce Yang et al. (2025) Table A1", {
  # Appendix A, Table A1, printed p21: values rounded to three decimals.
  # This is a published-number benchmark, not a simulator recovery check.
  reference <- rbind(
    c(0.000, 1.000, 0.000), c(0.564, 0.682, 0.137),
    c(1.163, 0.448, 0.303), c(1.586, 0.333, 0.423)
  )
  actual <- t(vapply(c(2, 3, 6, 12), function(k) {
    unlist(max_filler_moments(k), use.names = FALSE)
  }, numeric(3)))
  expect_equal(actual, reference, tolerance = 0.00051, scale = 1)
})

test_that("Miller binomial variances reproduce the published Table 2", {
  # Miller (1996), Perception & Psychophysics 58:65-72, Table 2 p69.
  # Convention .5: only counts 0 and n are replaced by .5 and n-.5.
  # At unbiased criterion H=Phi(d'/2), F=Phi(-d'/2); n is per arm.
  # The table gives true binomial probabilities, not observed integer counts.
  # Test the probability-based computational component directly; test the
  # exported count-based path independently below.
  reference <- rbind(c(.4913, .3583, .0560),
                     c(.1065, .1733, .1213),
                     c(.0255, .0372, .1409))
  actual <- sapply(c(.5, 2, 4), function(d) {
    vapply(c(8, 32, 128), function(n) {
      r4lineups:::sdt_var_z_from_binom(n, pnorm(d / 2), "half") +
        r4lineups:::sdt_var_z_from_binom(n, pnorm(-d / 2), "half")
    }, numeric(1))
  })
  expect_equal(actual, reference, tolerance = 0.000051, scale = 1)
})

test_that("count-based SDT variances implement the primary enumeration and delta equations", {
  # Equation checks, not printed-value reproductions: Miller (1996), pp67-69,
  # binomial moments; Gourevitch & Galanter (1967), Eq6 pp30-32.
  # Reference is written directly in terms of binomial outcomes and weights.
  count <- c(hit = 19, fa = 2, miss = 1, cr = 28)
  ns <- 20; nn <- 30
  p <- c((19 + .5) / 21, (2 + .5) / 31)
  exact_var <- function(n, probability) {
    outcomes <- 0:n
    values <- qnorm((outcomes + .5) / (n + 1))
    weights <- dbinom(outcomes, n, probability)
    sum(weights * (values - sum(weights * values))^2)
  }
  exact <- c(exact_var(ns, p[1]), exact_var(nn, p[2]))
  delta <- p * (1 - p) / (c(ns, nn) * dnorm(qnorm(p))^2)
  for (method in c("miller", "gourevitch")) {
    ref <- if (method == "miller") exact else delta
    result <- sdt_summary_variance(count[1], count[2], count[3], count[4],
                                  method = method, correction = "loglinear")
    expect_equal(unname(c(result$var_zH, result$var_zF)), unname(ref), tolerance = 1e-12)
    expect_equal(unname(result$var_dprime), unname(sum(ref)), tolerance = 1e-12)
    expect_equal(unname(result$var_c), unname(sum(ref) / 4), tolerance = 1e-12)
  }
})

test_that("Winter Table 2 counts reproduce the unrestricted tree solution", {
  # Winter et al. (2022), Scientific Reports 12:15571, Figure1 and Table2.
  # Exp1 simultaneous short-exposure counts; source equation check on
  # published counts. NOT a reproduction of the paper's constrained Table3.
  counts <- c(n_tp_suspect = 64, n_tp_filler = 152, n_tp_reject = 166,
              n_ta_suspect = 44, n_ta_filler = 157, n_ta_reject = 181)
  # Derive the inverse from the primary tree independently: the TP:TA
  # filler-rate ratio is (1-dP)/(1-dA); then solve the suspect equation.
  tp <- unname(counts[1:3] / 382)
  ta <- unname(counts[4:6] / 382)
  ratio <- tp[2] / ta[2]
  non_detection_ta <- (1 - tp[1]) / ratio + ta[1]
  non_detection_tp <- ratio * non_detection_ta
  q <- ta[1] / non_detection_ta
  u <- ta[2] / non_detection_ta
  bias <- q - u / 5
  reference <- c(dP = 1 - non_detection_tp, dA = 1 - non_detection_ta,
                 b = bias, g = u * 6 / ((1 - bias) * 5))
  fit <- fit_winter_2ht(counts, lineup_size = 6)
  expect_equal(fit$parameters, reference, tolerance = 1e-8)
  expect_equal(unname(fit$expected_counts$tp), unname(counts[1:3]), tolerance = 1e-7)
  expect_equal(unname(fit$expected_counts$ta), unname(counts[4:6]), tolerance = 1e-7)
})

test_that("recognition GLM metrics equal the transformed observed probabilities", {
  # DeCarlo (1998) / Wright et al. (2009) probit representation:
  # Phi^-1 P(old response) = -c + d' * centered item status.
  # Equation check with constructed observations; no published sample claimed.
  dat <- data.frame(old = rep(c(0, 1), each = 100),
                    response = c(rep(c(1, 0), c(20, 80)),
                                 rep(c(1, 0), c(70, 30))))
  for (link in c("probit", "logit")) {
    transform <- if (link == "probit") qnorm else qlogis
    z <- transform(c(.2, .7))
    fit <- fit_sdt_glm(dat, "old", "response", link = link)
    metric <- extract_sdt_metrics(fit)
    expect_equal(metric$estimate, z[2] - z[1], tolerance = 1e-7)
    expect_equal(metric$criterion, -mean(z), tolerance = 1e-7)
    expect_equal(unname(predict(fit, data.frame(.is_old_c = c(-.5, .5)),
                               type = "response")), c(.2, .7), tolerance = 1e-7)
    uncentered <- fit_sdt_glm(dat, "old", "response", link = link,
                             center_isold = FALSE)
    expect_equal(extract_sdt_metrics(uncentered)$estimate,
                 metric$estimate, tolerance = 1e-7)
    expect_true(is.na(extract_sdt_metrics(uncentered)$criterion))
    expect_equal(metric$metric, if (link == "probit") "dprime" else "ln_or")
  }
})

test_that("crossed recognition GLMM preserves the conditional SDT interpretation", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  # A finite, reproducible recovery illustration, not a coverage simulation.
  # Random intercepts create subject/item response tendencies; truth is d'=1.2,
  # c=.25 conditional on zero random effects. No random slopes are generated.
  set.seed(20261107)
  dat <- expand.grid(subject = factor(1:40), item = factor(1:24))
  dat$old <- rbinom(nrow(dat), 1, .5)
  us <- rnorm(40, 0, .6); ui <- rnorm(24, 0, .4)
  # Centre realized effects so the sample design has the stated midpoint.
  us <- us - mean(us); ui <- ui - mean(ui)
  eta <- -.25 + 1.2 * (dat$old - .5) +
    us[as.integer(dat$subject)] + ui[as.integer(dat$item)]
  dat$response <- rbinom(nrow(dat), 1, pnorm(eta))
  fit <- fit_sdt_glmm(dat, "old", "response", "subject", item_id = "item",
                      random_slope = FALSE)
  metric <- extract_sdt_metrics(fit)
  conditional <- unname(predict(fit,
    newdata = data.frame(.is_old_c = c(-.5, .5)), re.form = NA, type = "link"))
  expect_equal(metric$estimate, conditional[2] - conditional[1], tolerance = 1e-10)
  expect_equal(metric$criterion, -mean(conditional), tolerance = 1e-10)
  # Wide enough for sample fluctuation, narrow enough to reject a sign/coding
  # error or interpreting a coefficient as its opposite/half value.
  expect_lt(abs(metric$estimate - 1.2), .35)
  expect_lt(abs(metric$criterion - .25), .25)
  expect_equal(sort(names(lme4::ranef(fit))), c(".item_id", ".subject_id"))
  expect_equal(fit@optinfo$conv$opt, 0)
})

test_that("computing interfaces reject undefined bootstrap and fractional lineup inputs", {
  for (nboot in list(0, 1, NA_real_, Inf, 2.5)) {
    expect_error(sdt_summary_variance(7, 2, 3, 8, method = "bootstrap",
                                      nboot = nboot), "nboot")
  }
  for (size in c(2.5, Inf)) {
    expect_error(fit_max_sdt(45, 70, 25, 100, 100, n = size), "n")
  }
  # A two-person lineup is supported; this guard must not reject it.
  expect_true(is.finite(fit_max_sdt(45, 70, 25, 100, 100, n = 2)$dprime_1))
})

test_that("logical and numeric recognition inputs share the missing-value policy", {
  dat <- data.frame(old = rep(c(FALSE, TRUE), each = 10),
                    response = rep(c(FALSE, TRUE), 10),
                    subject = rep(1:5, 4))
  valid <- fit_sdt_glm(dat, "old", "response")
  expect_equal(nobs(valid), nrow(dat))
  for (column in c("old", "response")) {
    incomplete <- dat
    incomplete[[column]][1] <- NA
    expect_error(fit_sdt_glm(incomplete, "old", "response"), column)
    # Validation occurs before fitting, so this check requires no GLMM run.
    if (requireNamespace("lme4", quietly = TRUE)) {
      expect_error(fit_sdt_glmm(incomplete, "old", "response", "subject",
                                random_slope = FALSE), column)
    }
  }
})
