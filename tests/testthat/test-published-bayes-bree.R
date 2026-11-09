# Wells, Yang & Smalarz (2015), Law and Human Behavior39:99-122,
# Table1 p102; worked posterior calculations pp103-104, Equations1-3.
# Counts encode the printed three-decimal proportions; the TA row sums.999,
# so it is normalized by its 999 encoded entries. No raw sample size or
# sampling-uncertainty claim is made from these rounded rates.
test_that("Bayes curves reproduce Wells et al.'s worked posterior probabilities", {
  make_arm <- function(tp, counts) data.frame(
    target_present = tp,
    identification = rep(c("suspect", "filler", "reject"), counts)
  )
  d <- rbind(make_arm(TRUE, c(461, 212, 327)),
             make_arm(FALSE, c(134, 345, 520)))
  x <- make_bayes_curves(d, prior_grid = c(.25, .75),
                         innocent_suspect = "designated")$curves
  low <- x[x$prior == .25, ]
  post <- setNames(low$posterior, low$response)
  expect_equal(unname(post[c("suspect", "filler", "reject")]),
               c(.534, .170, .173), tolerance = .00051, scale = 1)
  expect_equal(x$posterior[x$prior == .75 & x$response == "suspect"],
               .912, tolerance = .00051, scale = 1)
})

test_that("BREE follows Wells et al. Equation4 using independent odds arithmetic", {
  # Equation-level check on constructed rates, not a published scalar benchmark.
  arm <- function(tp, suspect, filler, reject) data.frame(
    target_present = tp,
    identification = rep(c("suspect", "filler", "reject"),
                         c(suspect, filler, reject))
  )
  a <- rbind(arm(TRUE,60,20,20),arm(FALSE,10,30,60))
  b <- rbind(arm(TRUE,40,20,40),arm(FALSE,20,30,50))
  priors <- c(.1,.25,.5,.8,.9)
  # Prior odds in B times LR_B must equal prior odds in A times LR_A.
  expected_odds <- priors/(1-priors) * (6/2)
  expected_prior <- expected_odds/(1+expected_odds)
  x <- make_bree_curve(a,b,prior_grid=priors)$bree_curve
  expect_equal(x$prior_b,expected_prior,tolerance=1e-12)
  expect_equal(x$delta,expected_prior-priors,tolerance=1e-12)
  expect_equal(x$posterior_a,
               expected_prior*.4/(expected_prior*.4+(1-expected_prior)*.2),
               tolerance=1e-12)
  # Reversing the procedures reverses the matched-prior transformation.
  back <- make_bree_curve(b,a,prior_grid=x$prior_b)$bree_curve
  expect_equal(back$prior_b,priors,tolerance=1e-12)
})

test_that("Bayes and BREE reject invalid priors before returning probabilities", {
  d <- data.frame(target_present=rep(c(TRUE,FALSE),each=10),
                  identification=rep(c(rep("suspect",5),rep("reject",5)),2))
  for (p in list(-.1,1.1,NA_real_,Inf,c(.5,NaN),numeric(),".5")) {
    expect_error(make_bayes_curves(d,prior_grid=p),"prior_grid")
    expect_error(make_bree_curve(d,d,prior_grid=p),"prior_grid")
  }
  x <- make_bayes_curves(d,prior_grid=c(0,.5,1))$curves
  expect_equal(x$posterior,x$prior) # The two arm distributions are identical.
})

test_that("probability, signed and entropy gains preserve their distinct meanings", {
  d <- data.frame(target_present=rep(c(TRUE,FALSE),each=10),
    identification=c(rep("suspect",6),rep("reject",4),
                     "suspect",rep("reject",9)))
  posterior <- .1*.6/(.1*.6+.9*.1)
  probability <- make_bayes_curves(d,prior_grid=.1)$curves
  signed <- make_bayes_curves(d,prior_grid=.1,gain="signed")$curves
  bits <- make_bayes_curves(d,prior_grid=.1,gain="entropy")$curves
  h <- function(p) -p*log2(p)-(1-p)*log2(1-p)
  expect_equal(probability$information_gain[probability$response=="suspect"],
               abs(posterior-.1))
  expect_equal(signed$information_gain[signed$response=="suspect"],posterior-.1)
  expect_equal(bits$information_gain[bits$response=="suspect"],h(.1)-h(posterior))
  expect_lt(bits$information_gain[bits$response=="suspect"],0)
})

test_that("impossible conditioning events and unattainable BREE matches are explicit", {
  arm <- function(tp, suspect, reject) data.frame(
    target_present=tp, identification=rep(c("suspect","reject"),c(suspect,reject)))
  a <- rbind(arm(TRUE,9,1),arm(FALSE,0,10))
  b <- rbind(arm(TRUE,0,10),arm(FALSE,9,1))
  warnings <- character()
  x <- withCallingHandlers(make_bayes_curves(a,prior_grid=c(0,.5,1)),
    warning=function(w) {warnings <<- c(warnings,conditionMessage(w));
                         invokeRestart("muffleWarning")})
  expect_true(any(grepl("zero probability",warnings)))
  impossible <- x$curves$response=="suspect" & x$curves$prior==0
  expect_true(is.na(x$curves$posterior[impossible]))
  expect_true(is.na(x$curves$information_gain[impossible]))
  expect_equal(x$curves$posterior[x$curves$response=="suspect" &
                                  x$curves$prior==.5],1)
  warnings <- character()
  z <- withCallingHandlers(make_bree_curve(a,b,prior_grid=.5),
    warning=function(w) {warnings <<- c(warnings,conditionMessage(w));
                         invokeRestart("muffleWarning")})
  expect_true(any(grepl("cannot be matched",warnings)))
  expect_true(is.na(z$bree_curve$prior_b))
  expect_true(is.na(z$bree_curve$delta))
})

test_that("Bayesian draw counts and member positions cannot be silently recoded", {
  functions <- list(
    function(S) esize_T_bayes(c(4,6),S=S),
    function(S) func_size_bayes(rep(1:2,5),1,S=S),
    function(S) diag_ratio_T_bayes(rep(1:2,5),rep(1:2,5),1,1,2,2,S=S),
    function(S) calibration_bayes(data.frame(target_present=rep(c(TRUE,FALSE),5),
                                            identification="suspect",confidence=80),S=S)
  )
  for (fun in functions) for (S in list(0,NA_real_,Inf,1.5,c(2,3)))
    expect_error(fun(S),"S must")
  expect_error(func_size_bayes(rep(1:2,5),1.5,S=10),"target_pos")
  expect_error(diag_ratio_T_bayes(rep(1:2,5),rep(1:2,5),3,1,2,2,S=10),
               "target_pos")
})
