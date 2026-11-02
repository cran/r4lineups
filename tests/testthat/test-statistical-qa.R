# Small independent counterexamples retained from notes/statistical-qa.
test_that('one-column matrix and named-table representations preserve observations', {
 x<-c(1,2,2,2)
 expect_equal(lineup_prop_vec(matrix(x,ncol=1),1,6),.25)
 expect_equal(func_size(matrix(x,ncol=1),1),4)
 expect_equal(gen_linevec(table(c(2,2,5)),6),c(2,2,5))
 expect_error(datacheck3(c('1.5'=2,'2'=3),6),'integers')
 expect_error(classify_lineup_choices(data.frame(lineup_member=1.9),suspect_col=1),'whole')
 set.seed(3);expect_true(all(unlist(gen_boot_samples(6,50))==6))
})

test_that('independent effective-size inference is insensitive to arbitrary row pairing', {
 x<-rep(1:6,c(40,12,12,12,12,12));y<-rep(1:6,c(45,11,11,11,11,11))
 set.seed(99);yp<-sample(y)
 set.seed(1);capture.output(a<-effsize_compare(data.frame(x,y),R=999))
 set.seed(1);capture.output(b<-effsize_compare(data.frame(x,y=yp),R=999))
 expect_equal(a$difference,.5080634588,tolerance=1e-9)
 expect_lt(abs(sd(a$boot$t)/sd(b$boot$t)-1),.15)
 expect_lt(a$percentile[1],0);expect_gt(a$percentile[2],0)
})

test_that('every utility criterion implements its reported rule on custom grids', {
 d<-data.frame(target_present=rep(c(TRUE,FALSE),each=20),identification=c(rep('suspect',16),rep('reject',4),rep('suspect',2),rep('reject',18)),confidence=80)
 u<-make_utility_curves(d,base_rate=.01,thresholds=50)
 expect_gt(u$max_utility$criterion,80)
 expect_equal(unname(u$max_utility$expected_utility),.49)
 expect_equal(unname(u$utility_all_ids),.2545)
 for(i in seq_len(nrow(u$utility_data))) {
  a<-d$identification=='suspect'&d$confidence>=u$utility_data$criterion[i]
  expect_equal(mean(a[d$target_present]),u$utility_data$hit_rate[i])
  expect_equal(mean(a[!d$target_present]),u$utility_data$false_alarm_rate[i])
 }
})

test_that('unbounded corrected E and bootstrap mass are retained', {
 expect_warning(e<-esize_T(rep(1,6),unbiased=TRUE),'unbounded')
 expect_identical(e,Inf)
 set.seed(31);expect_warning(b<-esize_boot_dist(c(1,2),6,R=999,unbiased=TRUE),'retained')
 expect_length(b,999);expect_gt(sum(is.infinite(b)),400)
 expect_equal(sum(is.infinite(b)),attr(b,'n_infinite'))
})

test_that('maximum-filler log distributions preserve normal tail precision', {
 expect_equal(pmax_filler(-40,6,log.p=TRUE),5*pnorm(-40,log.p=TRUE))
 expect_equal(qmax_filler(-1000,6,lower.tail=FALSE,log.p=TRUE),qnorm(-1000-log(5),lower.tail=FALSE,log.p=TRUE),tolerance=1e-10)
 expect_identical(dmax_filler(-Inf,2,log=TRUE),-Inf)
})

test_that('contradictory design inputs and incomplete bins are rejected', {
 d<-lineup_example
 expect_error(make_rocdata(d,innocent_suspect='estimated'),'Recode')
 expect_error(make_eig_data(d,innocent_suspect='estimated'),'Recode')
 expect_error(make_cacdata(d,confidence_bins=c(60,100)),'cover')
 expect_error(make_calibration_data(d,confidence_bins=c(60,100)),'cover')
})

test_that('fixed zROC criteria survive rare-response bootstrap omission', {
 d<-data.frame(target_present=rep(c(TRUE,FALSE),each=100),identification=c(rep('suspect',61),rep('reject',39),rep('suspect',10),rep('reject',90)),confidence=c(100,rep(50,60),rep(0,39),rep(50,10),rep(0,90)))
 z<-fit_sdt_roc(d,lineup_size=6,n_bootstrap=50,seed=1,conf_level=.99)
 expect_equal(z$bootstrap_ci$n_successful,50)
 expect_output(print(z),'99% CI')
})

test_that("bootstrap distribution inputs do not truncate fractional design metadata", {
  expect_error(lineup_bias_boot_dist(c(1,2),1.9,6,R=10), "integer")
  expect_error(lineup_bias_boot_dist(c(1,2),1,6.9,R=10), "integer")
  expect_error(esize_boot_dist(c(1,2),6.9,R=10), "integer")
  expect_error(diag_ratio_W(c(1,2),c(1,2),1.9,1,6,6), "position")
  expect_error(var_diag_ratio(c(1,2),c(1,2),1.9,1,6,6), "position")
})
test_that("a single base-rate utility comparison has no interpolated crossover", {
  d <- data.frame(target_present = rep(c(TRUE, FALSE), each = 10),
                  identification = rep(c("suspect", "reject"), each = 5, times = 2),
                  confidence = 80, suspect_pos = 1)
  result <- make_utility_difference(d, d, base_rate_grid = .5)
  expect_equal(unname(result$difference_curve$difference), 0)
  expect_length(result$crossover_points, 0)
  expect_error(make_utility_difference(d, d, numeric()), "nonempty")
})
