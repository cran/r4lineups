# Primary-source numerical benchmarks. Source access and independent calculations:
# notes/validation-2.3.1-2026-11-07/roc-decision/SOURCES.md.
# Existing Smith (2019) DPP Table 4 benchmarks remain in test-formula-consistency.R.

published_rd_yaniv <- function() {
  # Yaniv, Yates & Smith (1991), Psychological Bulletin 110, p.613,
  # Figures 1-2: judgement, number of cases and relative outcome frequency.
  confidence <- c(10, 40, 60, 80, 100)
  n <- c(40, 20, 20, 10, 10)
  correct <- c(8, 10, 10, 6, 8)
  do.call(rbind, lapply(seq_along(n), function(j) data.frame(
    target_present = c(rep(TRUE, correct[j]), rep(FALSE, n[j]-correct[j])),
    identification = "suspect", confidence = confidence[j], suspect_pos = 1
  )))
}

testthat::test_that("NRI and ANRI reproduce Yaniv et al. Figures 1-2 and Equation 9", {
  d <- published_rd_yaniv()
  r <- suppressMessages(compute_anri(d,
    confidence_bins = c(0, 20, 50, 70, 90, 100), confidence_scale = "0-100"))
  # Printed p.615 values are .163 and .128, to three decimal places.
  # Absolute comparisons avoid testthat's relative-error scaling.
  testthat::expect_lt(abs(r$nri - .163), .0005)
  testthat::expect_lt(abs(r$anri - .128), .0005)
  testthat::expect_equal(r$n_total, 100)
  testthat::expect_equal(r$n_bins, 5L)
})

testthat::test_that("calibration matches independent C and O/U calculations for Yaniv's figure", {
  # Equation check, not an additional printed-number claim. C and O/U use
  # Brewer & Wells (2006), JEP Applied 12, p.12 definitions; counts from
  # Yaniv et al. (1991), p.613 Figures 1-2.
  r <- suppressMessages(make_calibration_data(published_rd_yaniv(),
    confidence_bins = c(0, 20, 50, 70, 90, 100), confidence_scale = "0-100"))
  n <- c(40, 20, 20, 10, 10)
  p <- c(.2, .5, .5, .6, .8)
  q <- c(.1, .4, .6, .8, 1)
  manual_C <- sum(n * (q-p)^2) / 100
  manual_OU <- sum(n * (q-p)) / 100
  testthat::expect_equal(r$C, manual_C, tolerance = 1e-12)
  testthat::expect_equal(r$OU, manual_OU, tolerance = 1e-12)
  testthat::expect_equal(r$C, .016, tolerance = 1e-12)
  testthat::expect_equal(r$overall_accuracy, .42, tolerance = 1e-12)
  testthat::expect_equal(r$overall_confidence, .42, tolerance = 1e-12)
})

testthat::test_that("CAC uses per-category correct and incorrect counts and explicit prior", {
  # Equation check of suspect-ID accuracy, as defined by Mickes (2015).
  # This reuses published category counts, not a claimed reproduction of
  # Mickes's empirical figures. Rates use separate 42/58 TP/TA denominators.
  d <- published_rd_yaniv()
  counts <- suppressMessages(make_cacdata(d, innocent_suspect = "designated"))
  prior <- make_cacdata(d, innocent_suspect = "designated", base_rate = .5)
  correct <- c(8,10,10,6,8)
  incorrect <- c(32,10,10,4,2)
  testthat::expect_equal(counts$cac_data$accuracy, correct/(correct+incorrect))
  expected <- (.5 * correct/42) / (.5 * correct/42 + .5 * incorrect/58)
  testthat::expect_equal(prior$cac_data$accuracy, expected, tolerance = 1e-12)
})

testthat::test_that("full ROC reproduces Smith et al. (2020) Figure 3", {
  # Perspectives on Psychological Science 15(3), p.596 Figure 3:
  # rows are affirmative high/medium/low, negative low/medium/high.
  g <- c(46,22,17,9,4,2)
  i <- c(9,13,19,21,16,22)
  decision <- c(rep("suspect",3),rep("reject",3))
  confidence <- c(3,2,1,1,2,3)
  rows <- function(tp,n) data.frame(target_present=tp,
    identification=rep(decision,n),confidence=rep(confidence,n),suspect_pos=1)
  d <- rbind(rows(TRUE,g),rows(FALSE,i))
  r <- make_fullroc_data(d, order="apriori", innocent_suspect="designated")
  testthat::expect_equal(r$roc_data$cumulative_hit_rate, c(0,.46,.68,.85,.94,.98,1))
  testthat::expect_equal(r$roc_data$cumulative_false_alarm_rate, c(0,.09,.22,.41,.62,.78,1))
  # Printed result immediately under the Figure 3 trapezoidal calculation: .80.
  testthat::expect_lt(abs(r$auc-.80), .005)
  # Exact trapezoids from those printed percentages give .7995.
  testthat::expect_equal(r$auc, .7995, tolerance=1e-12)
})

testthat::test_that("expected utility reproduces Lampinen et al. (2019) Table 2", {
  # Law and Human Behavior 43(1), p.29 Table 2: H=.60,F=.10,
  # prior=.65 and utilities (1,-1,-2,1) produce .375.
  d <- data.frame(target_present=rep(c(TRUE,FALSE),each=100),
    identification=c(rep("suspect",60),rep("reject",40),
                     rep("suspect",10),rep("reject",90)),
    confidence=1,suspect_pos=1)
  r <- make_utility_curves(d,base_rate=.65,
    utility_matrix=c(tp=1,fn=-1,fp=-2,tn=1),
    innocent_suspect="designated",criteria="all")
  testthat::expect_equal(unname(r$utility_all_ids), .375, tolerance=1e-12)
  testthat::expect_equal(unname(r$utility_data$expected_utility[r$utility_data$strategy=="reject_all"]),
                        -.3, tolerance=1e-12)
})

testthat::test_that("RAC wrapper preserves independently counted time-bin accuracies", {
  # Equation-level constructed benchmark, not a published empirical result.
  # Time bins contain 8 correct/2 incorrect and 2 correct/8 incorrect IDs.
  rows <- function(tp,yes,time) data.frame(target_present=tp,
    identification=c(rep("suspect",yes),rep("reject",10-yes)),
    response_time=time,confidence=50,suspect_pos=1)
  d <- rbind(rows(TRUE,8,2000),rows(FALSE,2,2000),
             rows(TRUE,2,8000),rows(FALSE,8,8000))
  r <- make_rac(d,time_bins=c(0,5000,10000),show_plot=FALSE,
                innocent_suspect="designated")
  testthat::expect_equal(r$rac_data$n_correct,c(8,2))
  testthat::expect_equal(r$rac_data$n_incorrect,c(2,8))
  testthat::expect_equal(r$rac_data$accuracy,c(.8,.2))
  testthat::expect_equal(r$rac_data$se,rep(sqrt(.8*.2/10),2))
  testthat::expect_equal(r$overall_accuracy,.5)
  testthat::expect_s3_class(r$plot,"ggplot")
})

# Input-contract regressions discovered by the bounded edge sweep. These are
# separate from the primary-source numerical benchmarks above.
testthat::test_that("calibration explains an empty selected population and retains nonchoosers", {
  d <- data.frame(target_present=rep(c(TRUE,FALSE),each=2),
                  identification="reject",confidence=50)
  for (population in c("suspect_ids","choosers")) {
    testthat::expect_error(
      make_calibration_data(d,population=population,confidence_scale="0-100"),
      "population|responses|observations|identifications"
    )
  }
  r <- make_calibration_data(d,population="nonchoosers",confidence_scale="0-100")
  testthat::expect_equal(r$n_total,4)
  testthat::expect_equal(r$overall_accuracy,.5)
  testthat::expect_equal(r$C,0)
})

testthat::test_that("utility rejects nonfinite and nonscalar priors informatively", {
  d <- published_rd_yaniv()
  for(prior in list(NA_real_,NaN,Inf,-Inf,c(.2,.8),numeric())) {
    testthat::expect_error(make_utility_curves(d,base_rate=prior),"base_rate")
  }
})

testthat::test_that("utility rejects nonfinite, nonnumeric and duplicate named values", {
  d <- published_rd_yaniv()
  invalid <- list(
    c(tp=NA_real_,fn=-1,fp=-2,tn=1),
    c(tp=Inf,fn=-1,fp=-2,tn=1),
    c(tp="x",fn="-1",fp="-2",tn="1"),
    c(tp=1,tp=2,fn=-1,fp=-2,tn=1),
    c(1,-1,-2,1)
  )
  for (u in invalid) {
    testthat::expect_error(make_utility_curves(d,utility_matrix=u),"utility_matrix")
  }
})
