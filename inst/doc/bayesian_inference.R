## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      warning = FALSE, message = FALSE,
                      fig.width = 7, fig.height = 5)

## -----------------------------------------------------------------------------
library(r4lineups)

## -----------------------------------------------------------------------------
data(nortje2012)

lineup_vec <- nortje2012$lineup_1
k <- max(lineup_vec, na.rm = TRUE)
lineup_tab <- table(factor(lineup_vec, levels = seq_len(k)))

# An illustrative tolerance. In a real analysis choose and justify it in
# advance; the software cannot supply one.
tolerance <- 0.75 * k

set.seed(1)
esize_post <- esize_T_bayes(
  lineup_tab,
  alpha = 0.5,
  S = 1000,
  threshold = tolerance
)

esize_post

## -----------------------------------------------------------------------------
plot(esize_post)

## ----esize_deficit------------------------------------------------------------
deficit <- k - esize_post$E_draws
knitr::kable(
  data.frame(
    Summary  = c(sprintf("95%% interval for E' (k = %d)", k),
                 "95% interval for the deficit k - E'",
                 sprintf("P(E' < %.1f | data)", tolerance)),
    Value    = c(sprintf("[%.2f, %.2f]", esize_post$credible_interval[["lower"]],
                         esize_post$credible_interval[["upper"]]),
                 sprintf("[%.2f, %.2f]", quantile(deficit, .025),
                         quantile(deficit, .975)),
                 sprintf("%.3f", esize_post$threshold_probs$P_below))
  ),
  row.names = FALSE,
  caption = "Posterior summaries of effective size, nortje2012 lineup 1."
)

## -----------------------------------------------------------------------------
set.seed(2)
func_post <- func_size_bayes(
  lineup_vec,
  target_pos = 3,
  alpha = 0.5,
  S = 1000,
  threshold = k
)

func_post

## -----------------------------------------------------------------------------
plot(func_post)

## -----------------------------------------------------------------------------
tp_vec <- nortje2012$lineup_1
ta_vec <- nortje2012$lineup_2

set.seed(3)
diag_post <- diag_ratio_T_bayes(
  lineup_pres = tp_vec,
  lineup_abs = ta_vec,
  pos_pres = 3,
  pos_abs = 6,
  k1 = max(tp_vec, na.rm = TRUE),
  k2 = max(ta_vec, na.rm = TRUE),
  alpha = 0.5,
  S = 1000,
  threshold = 1
)

diag_post

## -----------------------------------------------------------------------------
plot(diag_post)

## -----------------------------------------------------------------------------
data(lineup_example)

set.seed(4)
cal_post <- calibration_bayes(
  lineup_example,
  confidence_bins = c(0, 60, 80, 100),
  alpha = 0.5,
  S = 1000
)

cal_post

## -----------------------------------------------------------------------------
plot(cal_post, metric = "C")
plot(cal_post, metric = "NRI")

## -----------------------------------------------------------------------------
set.seed(5)
sdt_post <- sdt_compare(
  hits_A = 45, misses_A = 55, fas_A = 12, crs_A = 88,
  hits_B = 38, misses_B = 62, fas_B = 10, crs_B = 90,
  label_A = "Procedure A",
  label_B = "Procedure B",
  alpha = 0.5,
  S = 1000
)

sdt_post

## -----------------------------------------------------------------------------
plot(sdt_post)

## ----prior_sensitivity--------------------------------------------------------
sensitivity <- do.call(rbind, lapply(c(0.1, 0.5, 1.0), function(a) {
  set.seed(11)
  post <- esize_T_bayes(lineup_tab, alpha = a, S = 2000)
  data.frame(
    `Prior alpha`      = a,
    `Posterior median` = round(post$posterior_median, 3),
    `Lower`            = round(post$credible_interval[["lower"]], 3),
    `Upper`            = round(post$credible_interval[["upper"]], 3),
    check.names = FALSE
  )
}))

knitr::kable(
  sensitivity, row.names = FALSE,
  caption = "Sensitivity of the E' posterior to prior concentration, nortje2012 lineup 1 (nominal size 8)."
)

