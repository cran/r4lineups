## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      warning = FALSE, message = FALSE,
                      fig.width = 7, fig.height = 5)

## ----libraries----------------------------------------------------------------
library(r4lineups)

## ----max_filler---------------------------------------------------------------
dmax_filler(0, lineup_size = 6)   # density at 0
pmax_filler(0, lineup_size = 6)   # P(max filler < 0) = 0.5^5
qmax_filler(0.5, lineup_size = 6) # median of the max filler signal

## ----moments------------------------------------------------------------------
moments <- max_filler_moments(6)

knitr::kable(
  data.frame(
    Moment = c("Mean", "Variance", "Skewness"),
    Value  = round(c(moments$mean, moments$var, moments$skewness), 4),
    Reading = c("Centre: well above the 0 of any single filler",
                "Spread: narrower than a single filler's variance of 1",
                "Asymmetry: positive, so a longer right tail")
  ),
  row.names = FALSE,
  caption = "Moments of the maximum of five independent standard normal filler signals (lineup size 6). Analytic quantities, not computed from data."
)

## ----estimate-----------------------------------------------------------------
params <- estimate_msdt_params(pr_rej_I = 0.25, pr_rej_G = 0.15,
                               lineup_size = 6)
params

## ----estimate_check-----------------------------------------------------------
# The estimate reproduces the input rejection rate exactly, as it must
pnorm(params$gamma)^6

## ----joint_present------------------------------------------------------------
plot_msdt_joint(dprime = 1.2, gamma = 0.4, lineup_size = 6,
                lineup_type = "culprit_present")

## ----joint_absent-------------------------------------------------------------
plot_msdt_joint(dprime = 1.2, gamma = 0.4, lineup_size = 6,
                lineup_type = "culprit_absent")

