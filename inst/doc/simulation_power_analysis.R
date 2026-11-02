## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  message = FALSE
)

# The slowest results are computed with the code shown and stored in
# precomputed/, together with the random-number state, so that the rest of the
# vignette is unchanged. Set R4LINEUPS_PRECOMPUTE=true to recompute them
# (data-raw/vignette_precompute.R).
precompute <- identical(Sys.getenv("R4LINEUPS_PRECOMPUTE"), "true")
stored <- function(name, objects) {
  path <- file.path("precomputed", paste0(name, ".rds"))
  env <- knitr::knit_global()
  if (precompute) {
    saveRDS(list(objects = mget(objects, envir = env),
                 seed = globalenv()[[".Random.seed"]]),
            path, compress = "xz")
  } else {
    s <- readRDS(path)
    list2env(s$objects, envir = env)
    if (!is.null(s$seed)) assign(".Random.seed", s$seed, envir = globalenv())
  }
  invisible(NULL)
}

## ----library------------------------------------------------------------------
library(r4lineups)

## ----criterion-semantics------------------------------------------------------
criteria <- c(0, 0.5, 1.0, 1.27, 1.5, 2.0)

rejection_rates <- vapply(criteria, function(cc) {
  d <- simulate_lineup_data(n_tp = 800, n_ta = 800, d_prime = 1.5,
                            c_criterion = cc, lineup_size = 6,
                            conf_levels = 5, seed = 11)
  100 * mean(d$identification[!d$target_present] == "reject")
}, numeric(1))

knitr::kable(
  data.frame(`c_criterion` = criteria,
             `TA rejection rate (%)` = round(rejection_rates, 1),
             check.names = FALSE),
  row.names = FALSE,
  caption = "Rejection rate in target-absent lineups as the criterion varies, simulated data (800 lineups of size 6 per point, d' = 1.5)."
)

## ----criterion-by-size--------------------------------------------------------
sizes <- c(4, 6, 8, 10, 12)

knitr::kable(
  data.frame(`Lineup size` = sizes,
             `Neutral criterion` = round(qnorm(0.5^(1 / sizes)), 3),
             check.names = FALSE),
  row.names = FALSE,
  caption = "Criterion giving 50% target-absent rejections in a fair lineup: the median of the maximum of k standard normal draws."
)

## ----basic_simulation---------------------------------------------------------
sim_data <- simulate_lineup_data(
  n_tp        = 200,   # 200 target-present lineups
  n_ta        = 200,   # 200 target-absent lineups
  d_prime     = 1.5,   # moderate discriminability
  c_criterion = 1.0,   # moderately conservative for a 6-person lineup
  lineup_size = 6,
  conf_levels = 5,
  seed        = 2026
)

head(as.data.frame(sim_data))

## ----foil-bias----------------------------------------------------------------
uneven <- simulate_lineup_data(
  n_tp = 400, n_ta = 400, d_prime = 1.5, c_criterion = 1.0,
  lineup_size = 6, conf_levels = 5,
  # Foil 2 is a strong alternative; foil 6 is nearly a null foil
  foil_bias = c(0.5, 0.25, 0.0, -0.4, -1.2),
  seed = 2026
)

lineup_table(uneven$lineup_member[uneven$lineup_member > 0],
             k = 6, suspect_pos = 1)

## ----power_analysis, eval = precompute----------------------------------------
# # Stored with the vignette; set R4LINEUPS_PRECOMPUTE=true to recompute.
# power_result <- simulate_power_analysis(
#   sample_sizes  = c(25, 50, 100, 200),
#   d_prime       = 1.0,
#   d_prime_null  = 0,
#   n_simulations = 30,   # small, so this vignette builds quickly
#   n_bootstrap   = 60,   # likewise
#   conf_levels   = 5,
#   alpha         = 0.05,
#   seed          = 2026,
#   verbose       = FALSE
# )

## ----power_analysis_stored, include = FALSE-----------------------------------
stored("power_analysis", "power_result")

## ----power_table--------------------------------------------------------------
knitr::kable(
  setNames(as.data.frame(power_result)[, c("sample_size", "n_total", "power",
                                           "power_se", "mean_effect")],
           c("TP (= TA) per condition", "Total witnesses", "Power",
             "Power SE", "Mean effect")),
  digits = 3, row.names = FALSE,
  caption = "Power to detect d' = 1.0 against chance, 30 simulated studies per sample size."
)

## ----power_crossing, include = FALSE------------------------------------------
pr <- as.data.frame(power_result)
first80 <- which(pr$power >= 0.8)[1]
cross_lo <- pr$sample_size[first80 - 1]
cross_hi <- pr$sample_size[first80]

## ----power_plot---------------------------------------------------------------
p <- plot(power_result)

## ----power_plot_modified------------------------------------------------------
library(ggplot2)

p +
  geom_hline(yintercept = 0.95, linetype = "dotted") +
  labs(title = "Power curve with a 95% reference line") +
  theme_minimal(base_size = 12)

## ----parameter_recovery-------------------------------------------------------
true_dprimes <- c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0)

recovery <- do.call(rbind, lapply(true_dprimes, function(dp) {
  dat <- as.data.frame(simulate_lineup_data(
    n_tp = 500, n_ta = 500, d_prime = dp, c_criterion = 1.0,
    lineup_size = 6, conf_levels = 5, seed = 321
  ))
  fit <- fit_winter_2ht(dat, lineup_size = 6,
                        target_present = "target_present",
                        identification = "identification")
  data.frame(`True d'` = dp,
             dP = round(fit$parameters[["dP"]], 3),
             dA = round(fit$parameters[["dA"]], 3),
             check.names = FALSE)
}))

knitr::kable(
  recovery, row.names = FALSE,
  caption = "2-HT detection parameters fitted to data simulated from a signal detection model at known d', 500 lineups per condition."
)

## ----design_comparison--------------------------------------------------------
sizes <- c(4, 6, 8, 10, 12)
set.seed(77)

design <- do.call(rbind, lapply(sizes, function(k) {
  # Criterion matched to lineup size, so willingness to choose is held constant
  crit <- mean(replicate(5000, max(stats::rnorm(k))))
  d <- as.data.frame(simulate_lineup_data(
    n_tp = 500, n_ta = 500, d_prime = 1.5, c_criterion = crit,
    lineup_size = k, conf_levels = 5, seed = 77
  ))
  tp <- d[d$target_present, ]; ta <- d[!d$target_present, ]
  data.frame(
    `Lineup size`          = k,
    `Criterion used`       = round(crit, 2),
    `Correct ID rate (%)`  = round(100 * mean(tp$identification == "suspect"), 1),
    `False ID rate (%)`    = round(100 * mean(ta$identification == "suspect"), 1),
    check.names = FALSE
  )
}))

knitr::kable(
  design, row.names = FALSE,
  caption = "Identification rates by lineup size at a size-matched criterion, simulated data (500 target-present and 500 target-absent lineups per size, d' = 1.5)."
)

## ----roc_setup----------------------------------------------------------------
sim_roc <- simulate_lineup_data(
  n_tp = 300, n_ta = 300, d_prime = 1.8,
  c_criterion = 1.0, conf_levels = 5, seed = 5
)

## ----roc_analysis-------------------------------------------------------------
roc_result <- make_roc(sim_roc, lineup_size = 6, show_plot = FALSE)
print(roc_result)

## ----roc_plot-----------------------------------------------------------------
plot(roc_result) +
  labs(title = "ROC from simulated data (d' = 1.8, 300 lineups per condition)") +
  theme_minimal(base_size = 12)

## ----cac_analysis-------------------------------------------------------------
cac_result <- make_cac(sim_roc, show_plot = FALSE)
print(cac_result)

## ----cac_plot-----------------------------------------------------------------
plot(cac_result) +
  labs(title = "CAC from simulated data (d' = 1.8)") +
  theme_minimal(base_size = 12)

## ----rac_analysis-------------------------------------------------------------
sim_rac <- simulate_lineup_data(
  n_tp = 1000, n_ta = 1000, d_prime = 1.5, c_criterion = 1.0,
  conf_levels = 5, include_response_time = TRUE, seed = 6
)

# Bins must match the data. These simulated times run from roughly 2.5 to 9.5
# seconds, so bins extending to 20 seconds would leave the upper ones empty.
round(quantile(sim_rac$response_time, c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))

rac_result <- make_rac(sim_rac, time_bins = c(0, 4500, 5500, 6500, Inf),
                       show_plot = FALSE)
print(rac_result)

## ----fullroc_analysis---------------------------------------------------------
fullroc_result <- make_fullroc(sim_roc, lineup_size = 6, show_plot = FALSE)
print(fullroc_result)

