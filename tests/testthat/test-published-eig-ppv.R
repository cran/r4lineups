# Published-value benchmarks for the EIG and PPV-range families.

# Starns, Cohen and Rotello (2023), Psychological Review, 130(3), 677-719,
# Table 1 (p. 682): showup data from Colloff and Wixted (2020, Exp. 3), joint
# response proportions. Guilty: .31 .10 .03 .06 (high ID, low ID, high reject,
# low reject), total .50; innocent: .11 .05 .23 .11, total .50. The text on
# p. 687 (Equation 23): "This process produces EIG values of 0.22 and 0.18 for
# the showup and lineup data ... respectively". The posterior for a
# high-confidence ID is .74 (p. 682) and its information gain is
# "1.00 - 0.82 = 0.18" (Equation 22, p. 687).
testthat::test_that("compute_eig reproduces the showup EIG of 0.22 (Starns et al., 2023, Table 1)", {
  g <- c(31, 10, 3, 6)
  i <- c(11, 5, 23, 11)
  ids <- c("suspect", "suspect", "reject", "reject")
  conf <- c(2, 1, 2, 1)
  d <- data.frame(
    target_present = rep(c(TRUE, FALSE), c(sum(g), sum(i))),
    identification = c(rep(ids, g), rep(ids, i)),
    confidence = c(rep(conf, g), rep(conf, i)),
    stringsAsFactors = FALSE
  )
  res <- compute_eig(d, prior_guilt = 0.5)
  testthat::expect_equal(res$eig, 0.22, tolerance = 0.0050001, scale = 1)
  rd <- res$response_data
  hid <- rd[rd$response == "suspect_2", ]
  testthat::expect_equal(hid$posterior_guilty, 0.74, tolerance = 0.0050001, scale = 1)
  # Check the entropy from Table 1 probabilities at full precision.
  p_hid <- 0.31 / 0.42
  testthat::expect_equal(hid$information_gain,
                         1 - (-(p_hid * log2(p_hid) + (1 - p_hid) * log2(1 - p_hid))),
                         tolerance = 1e-10, scale = 1)
  testthat::expect_equal(hid$p_response, 0.42, tolerance = 1e-8)
  # Posterior probabilities of guilt in the unshaded part of Table 1
  post <- stats::setNames(rd$posterior_guilty, rd$response)
  testthat::expect_equal(unname(post[c("suspect_2", "suspect_1", "reject_2", "reject_1")]),
                         c(.74, .67, .12, .35), tolerance = 0.0050001, scale = 1)
})

# Equation check: Starns et al. (2023), Equations 21-23 (pp. 686-687) with
# EIG = H(prior) - sum p(r) H(posterior | r), computed here in base R from the
# Table 1 proportions at a prior of .30 (not a published value).
testthat::test_that("compute_eig equals H(prior) - sum p(r) H(post | r) by hand (equation check)", {
  g <- c(31, 10, 3, 6) / 50
  i <- c(11, 5, 23, 11) / 50
  H <- function(p) -(p * log2(p) + (1 - p) * log2(1 - p))
  prior <- 0.3
  pr <- prior * g + (1 - prior) * i
  post <- prior * g / pr
  by_hand <- H(prior) - sum(pr * H(post))
  d <- data.frame(
    target_present = rep(c(TRUE, FALSE), c(50, 50)),
    identification = c(rep(c("suspect", "suspect", "reject", "reject"), c(31, 10, 3, 6)),
                       rep(c("suspect", "suspect", "reject", "reject"), c(11, 5, 23, 11))),
    confidence = c(rep(c(2, 1, 2, 1), c(31, 10, 3, 6)), rep(c(2, 1, 2, 1), c(11, 5, 23, 11)))
  )
  testthat::expect_equal(compute_eig(d, prior_guilt = prior)$eig, by_hand, tolerance = 1e-10, scale = 1)
  # Equation 7: a prior of .5 has entropy 1 bit
  testthat::expect_equal(compute_eig(d, prior_guilt = 0.5)$prior_entropy, 1)
})

# Fitzgerald, Tredoux and Juncu (2023), Law and Human Behavior, 47(4),
# 463-483, Table 2 (p. 467): hypothetical culprit-absent lineup choices for
# a fair lineup (10 10 10 10 10 10), a partially biased lineup (0 20 20 20 0 0)
# and a biased lineup (0 0 60 0 0 0). Estimated innocent-suspect IDs are
# nominal 10, 10, 10; effective 10, 20, 60; no correction 60, 60, 60. Text
# (p. 467): "In the fair lineup ... the effective size correction classifies
# 10 as innocent-suspect IDs. In the partially biased lineup ... 20 ... In the
# maximally biased lineup ... all misidentifications".
testthat::test_that("esize_T and the innocent-ID rate functions reproduce Fitzgerald et al. (2023) Table 2", {
  tab <- list(fair = rep(10, 6),
              partial = c(0, 20, 20, 20, 0, 0),
              biased = c(0, 0, 60, 0, 0, 0))
  eff <- vapply(tab, esize_T, numeric(1))
  testthat::expect_equal(unname(eff), c(6, 3, 1))
  nominal <- vapply(tab, function(x) innocent_id_rate_nominal(sum(x), 6), numeric(1))
  effective <- mapply(function(x, e) innocent_id_rate_effective(sum(x), e), tab, eff)
  none <- vapply(tab, function(x) innocent_id_rate_uncorrected(sum(x)), numeric(1))
  testthat::expect_equal(unname(nominal), c(10, 10, 10))
  testthat::expect_equal(unname(effective), c(10, 20, 60))
  testthat::expect_equal(unname(none), c(60, 60, 60))
})

# Same Table 2 data pushed through ppv_by_confidence(): 60 culprit-absent
# lineups that all end in a mistaken ID, in one confidence bin, with
# lineup_member recording the position chosen. The number of estimated
# innocent-suspect IDs should be 10 (nominal), 20 (effective, partially
# biased) and 60 (none). Fitzgerald et al. (2023, Table 2, p. 467).
testthat::test_that("ppv_by_confidence reproduces the Table 2 innocent-suspect ID counts", {
  mk <- function(members) {
    n <- length(members)
    data.frame(
      target_present = c(rep(TRUE, 60), rep(FALSE, n)),
      identification = c(rep("suspect", 60), rep("filler", n)),
      confidence = 1,
      lineup_member = c(rep(1, 60), members)
    )
  }
  partial <- mk(rep(2:4, each = 20))
  nom <- ppv_by_confidence(partial, correction = "nominal", quiet = TRUE)
  eff <- ppv_by_confidence(partial, correction = "effective", quiet = TRUE)
  non <- ppv_by_confidence(partial, correction = "none", quiet = TRUE)
  testthat::expect_equal(nom$ppv_data$n_innocent_ids_est, 10)
  testthat::expect_equal(eff$ppv_data$n_innocent_ids_est, 20)
  testthat::expect_equal(eff$ppv_data$effective_size, 3)
  testthat::expect_equal(non$ppv_data$n_innocent_ids_est, 60)
  # PPV = guilty IDs / (guilty IDs + estimated innocent-suspect IDs), 60 guilty IDs
  testthat::expect_equal(nom$ppv_data$ppv, 60 / 70)
  testthat::expect_equal(eff$ppv_data$ppv, 60 / 80)
  testthat::expect_equal(non$ppv_data$ppv, 60 / 120)

  biased <- mk(rep(3, 60))
  testthat::expect_equal(
    ppv_by_confidence(biased, correction = "effective", quiet = TRUE)$ppv_data$n_innocent_ids_est, 60)
  fair <- mk(rep(1:6, each = 10))
  testthat::expect_equal(
    ppv_by_confidence(fair, correction = "effective", quiet = TRUE)$ppv_data$n_innocent_ids_est, 10)
})
