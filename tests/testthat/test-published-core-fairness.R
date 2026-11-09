# Primary-source benchmarks: see notes/validation-2.3.1-2026-11-07/core/sources.md.
# Tolerances below refer to the printed decimal precision, not observed test error.

test_that("Malpass (1981) Table 1 original effective and functional sizes reproduce", {
  # Law and Human Behavior 5(4), p.300, Table 1, lineups A--K.
  # The original E removes null foils; esize_m() returns the nominal-size
  # revision but prints both versions when both=TRUE.
  counts <- rbind(c(20,20,20,20,20), c(10,22,23,22,23),
                  c(25,19,19,19,18), c(25,25,25,13,12),
                  c(25,35,14,13,13), c(25,30,25,10,10),
                  c(25,30,30,8,7), c(25,55,8,6,6),
                  c(25,75,0,0,0), c(60,10,10,10,10), c(80,10,10,0,0))
  original_e <- c(5,4.5,4.75,4.25,4,4,3.75,3,1.5,3,1.6)
  functional <- c(5,10,4,4,4,4,4,4,4,1.67,1.25)
  for (j in seq_len(nrow(counts))) {
    choices <- rep(1:5, counts[j,])
    output <- capture.output(esize_m(table(factor(choices,levels=1:5)),5,both=TRUE))
    actual_original <- as.numeric(sub(".*= *", "", output[1]))
    expect_equal(actual_original, original_e[j], tolerance=1e-7)
    # J's printed 1.67 is rounded to two decimals.
    expect_lte(abs(func_size(choices,1)-functional[j]),.005)
  }
})

test_that("Tredoux (1998) Table 3 nominal-size effective sizes reproduce", {
  # Law and Human Behavior 22(2), p.225, Table 3, E_b column, lineups g--l.
  # In particular g and i keep their observed zero-count positions.
  counts <- list(c(0,25,5,25,3,2),c(10,10,9,10,11,10),
                 c(1,0,12,12,0,11),c(7,7,7,24,8,7),
                 c(12,6,9,13,14,6),c(6,19,3,20,8,10))
  expected <- c(3,5.90,3.17,4.60,5.10,4.45)
  actual <- vapply(counts,function(x) esize_m(rep(1:6,x),6),numeric(1))
  # E_b is printed to two decimals.
  expect_true(all(abs(actual-expected)<=.005))
})

test_that("Tredoux (1998) Table 6 diversity and interval equations reproduce", {
  # p.230, Table 6: member counts 3,24,30,33,10, I=.7326 and E'=3.740.
  # Its diversity limits are .7015 and .7637. Equations 8 and 11, pp.226--227,
  # give the diversity variance and the reciprocal transformation.
  counts <- c(3,24,30,33,10)
  expect_equal(i_esize_T(counts),.7326,tolerance=1e-12)
  expect_lte(abs(esize_T(counts)-3.740),.0005)
  actual <- unlist(esize_T_ci_n(counts),use.names=FALSE)
  # Compare the printed diversity limits at their four-decimal precision.
  expect_true(all(abs((1-1/actual)-c(.7015,.7637))<=.00005))
  # Independent equation calculation from the printed counts (not another
  # package helper). Retain precision through the reciprocal transformation.
  p <- counts/100
  diversity <- 1-sum(p^2)
  sd_i <- sqrt(4/100*(sum(p^3)-sum(p^2)^2))
  expected <- 1/(1-(diversity+c(-1,1)*qnorm(.975)*sd_i))
  expect_equal(actual,expected,tolerance=1e-12)
})

test_that("Tredoux (1998) Table 7 diagnosticity ratios use all trials", {
  # p.231, Table 7: TP suspect31/60; TA suspect5/60 or25/60.
  # N/P denotes no choice. Use an arbitrary non-suspect member to represent
  # these trials in the vector interface: only suspect count and N enter DR.
  tp <- c(rep(4,31),rep(1,29))
  ta1 <- c(rep(4,5),rep(1,55))
  ta2 <- c(rep(4,25),rep(1,35))
  expect_equal(diag_ratio_W(tp,ta1,4,4,6,6),6.20,tolerance=1e-12)
  expect_equal(diag_ratio_W(tp,ta2,4,4,6,6),1.24,tolerance=1e-12)
  # Equation 12 p.233 adds .5 to the suspect count and total once.
  expect_equal(diag_ratio_T(tp,ta1,4,4,6,6),31.5/5.5,tolerance=1e-12)
  expect_equal(diag_ratio_T(tp,ta2,4,4,6,6),31.5/25.5,tolerance=1e-12)
  # Equation15 p.233 applies to uncorrected ratios, not the .5 correction.
  expected_variance <- 1/31-1/60+1/5-1/60
  expect_equal(var_diag_ratio(tp,ta1,4,4,6,6),expected_variance,tolerance=1e-12)
  cells <- data.frame(n11=31,n21=29,n12=5,n22=55)
  expect_equal(var_lnd(cells)$var,expected_variance,tolerance=1e-12)
  expect_equal(d_weights(cells)$wi,1/expected_variance,tolerance=1e-12)
})

test_that("pooling reconstructs the rounded inputs of Tredoux (1998) Table 9", {
  # p.235 Table 9 supplies d=(2.84,9.34,2.58), Var(log d)=(.16,.18,.04).
  # Reconstruct Eqs17--18 from those rounded inputs. This is not exact
  # reproduction of unavailable unrounded inputs or an independent-study test:
  # the source explicitly says its three ratios were dependent (p.234).
  d <- c(2.84,9.34,2.58)
  variance <- c(.16,.18,.04)
  weights <- 1/variance
  mean_log <- sum(weights*log(d))/sum(weights)
  pooled <- exp(mean_log)
  q <- sum(weights*(log(d)-mean_log)^2)
  input <- rbind(variance,log(d),weights)
  expect_equal(d_bar(input),pooled,tolerance=1e-12)
  expect_equal(chi_diag(input),q,tolerance=1e-12)
  # With the printed ln(d) row (1.04, 2.23, 0.95) the pooled ratio and the
  # homogeneity chi-square reproduce the Table 9 note: d = 3.19, X2 = 7.55.
  printed <- rbind(variance,c(1.04,2.23,0.95),weights)
  expect_lte(abs(d_bar(printed)-3.19),.005)
  expect_lte(abs(chi_diag(printed)-7.55),.005)
})

# Edge-case regressions found by the 7 November bounded validation sweep.
# These assert input contracts, independently of the source benchmarks above.
test_that("core wrappers reject impossible suspect positions and malformed resamples", {
  expect_error(lineup_prop_tab(table(c(1,1,2)),target_pos=-1),"target_pos")
  expect_error(lineup_prop_tab(table(c(1,1,2)),target_pos=1.5),"target_pos")
  expect_error(gen_lineup_prop(data.frame(a=c(1,2),b=c(1,99)),1,2),
               "between|position|member")
  expect_error(gen_lineup_prop(list(a=c(1,2),b=c(1,NA_real_)),1,2),
               "finite|missing|position|member")
  expect_error(func_size_report(rep(1:6,2),target_pos=7,k=6,R=20),"target_pos")
  expect_error(eff_size_per_foils(rep(1:2,5),c(1.5,2),2,R=20),"target_pos")
  expect_error(eff_size_per_foils(rep(1:2,5),c(1,1,2),2,R=20),"target_pos")
})


test_that("resample helpers preserve supported input forms and valid zero counts", {
  expected <- c(.5,0)
  expect_equal(unname(gen_lineup_prop(list(a=c(1,2),b=c(2,2)),1,2)),expected)
  expect_equal(unname(gen_lineup_prop(data.frame(a=factor(c(1,2)),
                                                b=factor(c(2,2))),1,2)),expected)
  expect_equal(unname(gen_lineup_prop(matrix(c(1,2,2,2),nrow=2),1,2)),expected)
  # A valid unchosen member has probability0 and infinite functional size.
  tab <- lineup_table(c(1,1),k=2,suspect_pos=2)
  expect_equal(lineup_prop_tab(tab,2),0)
  expect_error(lineup_prop_tab(tab,3),"target_pos")
  suppressWarnings(capture.output(report<-func_size_report(c(1,1),2,2,R=20)))
  expect_identical(report$estimate,Inf)
  expect_identical(report$exact[2],Inf)
  expect_equal(report$n_infinite,20)
})

test_that("constant effective-size bootstrap retains available estimates", {
  set.seed(1107)
  expect_warning(capture.output(result <- effsize_compare(
    data.frame(a=rep(1,10),b=rep(2,10)),R=20
  )),"constant|degenerate|BCa|bca")
  expect_equal(result$difference,0)
  expect_equal(unname(result$normal),c(0,0))
  expect_equal(unname(result$percentile),c(0,0))
  expect_true(all(is.na(result$bca)))
  # This is a statement about a constant empirical resampling distribution,
  # not proof that the population effective-size difference is zero.
})

test_that("bootstrap distribution wrappers preserve factor position labels", {
  set.seed(1108)
  # A factor labelled2 denotes member2, not its internal factor code1.
  values <- lineup_bias_boot_dist(factor(rep(2,3)),2,6,R=10)
  expect_equal(as.vector(values),rep(1,10))
  expect_equal(attr(values,"n"),3)
  expect_error(esize_boot_dist(factor(c(2,2,6)),k=2,R=10),"between|position|member")
  expect_error(lineup_bias_boot_dist(c(2,2,NA),2,6,R=10),"finite|missing|NA")
  expect_error(esize_boot_dist(c(2,2,NA),k=6,R=10),"finite|missing|NA")
})
