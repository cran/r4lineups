## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
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

## ----load-data----------------------------------------------------------------
library(r4lineups)
data(lineup_example)

head(lineup_example)

## ----classify-demo------------------------------------------------------------
head(classify_lineup_choices(
  lineup_example[, c("lineup_member", "suspect_pos")],
  k = 6
))

## ----foil-usage---------------------------------------------------------------
lineup_table(
  lineup_example$lineup_member[lineup_example$lineup_member > 0],
  k = 6, suspect_pos = 1
)

## ----calibration-basic--------------------------------------------------------
# Compute calibration with binned confidence
cal_result <- make_calibration(
  lineup_example,
  confidence_bins = c(0, 60, 80, 100),
  choosers_only = TRUE  # Analyze only suspect identifications
)

print(cal_result)

## ----calibration-plot---------------------------------------------------------
cal_result$plot

## ----calibration-save---------------------------------------------------------
outfile <- file.path(tempdir(), "calibration_plot.png")
ggplot2::ggsave(outfile, cal_result$plot, width = 8, height = 6, dpi = 300)

# Change only the extension to change the format:
#   ggsave("calibration_plot.pdf", cal_result$plot)   # vector, for print
#   ggsave("calibration_plot.svg", cal_result$plot)   # vector, for the web

## ----simulate-procedures------------------------------------------------------
# Procedure A: stronger evidence, and foils that are reasonable alternatives
data_proc_a <- as.data.frame(simulate_lineup_data(
  n_tp = 400, n_ta = 400, d_prime = 1.8, c_criterion = 0.9,
  lineup_size = 6, conf_levels = 9,
  foil_bias = c(0.4, 0.2, 0.0, -0.3, -0.8), seed = 101
))

# Procedure B: weaker evidence, and one near-useless foil
data_proc_b <- as.data.frame(simulate_lineup_data(
  n_tp = 400, n_ta = 400, d_prime = 1.1, c_criterion = 0.9,
  lineup_size = 6, conf_levels = 9,
  foil_bias = c(0.5, 0.1, -0.2, -0.9, -2.0), seed = 102
))

# Put confidence on the same 20-100 scale used by lineup_example
conf_scale <- round(seq(20, 100, length.out = 9))
data_proc_a$confidence <- conf_scale[data_proc_a$confidence]
data_proc_b$confidence <- conf_scale[data_proc_b$confidence]

# Combine into one frame with a condition variable
procedures <- rbind(
  transform(data_proc_a, procedure = "Procedure A"),
  transform(data_proc_b, procedure = "Procedure B")
)

## ----calibration-by-condition-------------------------------------------------
cal_by_cond <- make_calibration_by_condition(
  procedures,
  condition_vars = "procedure",
  confidence_bins = c(0, 60, 80, 100)
)

knitr::kable(
  cal_by_cond$condition_summary, digits = 3,
  caption = "Calibration statistics by procedure, simulated data (suspect identifications)."
)

## ----calibration-by-condition-plot--------------------------------------------
make_calibration_by_condition_gg(cal_by_cond)

## ----bayes-likelihoods--------------------------------------------------------
lik <- make_bayes_curves(
  lineup_example,
  response_categories = "confidence",
  confidence_bins = c(0, 60, 80, 100)
)$likelihoods

knitr::kable(
  transform(
    as.data.frame(lik)[, c("response", "p_x_given_guilty",
                           "p_x_given_innocent", "n_guilty", "n_innocent")],
    LR = round(lik$p_x_given_guilty / lik$p_x_given_innocent, 2)
  ),
  digits = 3, row.names = FALSE,
  caption = "Response likelihoods and likelihood ratios, lineup_example."
)

## ----bayes-curves-------------------------------------------------------------
# Compute Bayesian curves for simple responses
bayes_result <- make_bayes_curves(
  lineup_example,
  response_categories = "simple",  # "suspect", "filler", "reject"
  prior_grid = seq(0.01, 0.99, 0.01)
)

print(bayes_result)

# Plot prior-posterior relationships
plot_bayes_prior_posterior(bayes_result)

## ----bayes-information-gain---------------------------------------------------
plot_bayes_information_gain(bayes_result)

## ----bayes-information-gain-entropy-------------------------------------------
bayes_entropy <- make_bayes_curves(
  lineup_example,
  response_categories = "simple",
  prior_grid = seq(0.01, 0.99, 0.01),
  gain = "entropy"
)
plot_bayes_information_gain(bayes_entropy)

## ----bayes-confidence---------------------------------------------------------
bayes_conf <- make_bayes_curves(
  lineup_example,
  response_categories = "confidence",
  confidence_bins = c(0, 60, 80, 100)
)

# Get suspect responses only
suspect_responses <- grep("^suspect_", unique(bayes_conf$curves$response), value = TRUE)
plot_bayes_prior_posterior(bayes_conf, selected_responses = suspect_responses)

## ----bree---------------------------------------------------------------------
bree_result <- make_bree_curve(
  data_proc_a,
  data_proc_b,
  reference_response = "suspect"
)

plot_bree(bree_result)

## ----bree-summary, include = FALSE--------------------------------------------
bc <- bree_result$bree_curve
bc <- bc[is.finite(bc$delta), ]
at20 <- bc[which.min(abs(bc$prior_a - 0.2)), ]
peak <- bc[which.max(bc$delta), ]

## ----utility-matrix-----------------------------------------------------------
utility_matrix <- c(
  tp =  1.0,   # Correct identification of a guilty suspect: a benefit
  fn = -0.5,   # Failing to identify a guilty suspect: a cost
  fp = -2.0,   # Identifying an innocent suspect: the most serious cost
  tn =  0.5    # Correctly declining to identify an innocent suspect: a benefit
)

## ----utility-curves-----------------------------------------------------------
# Compute expected utility across confidence criteria
util_result <- make_utility_curves(
  lineup_example,
  base_rate = 0.5,  # Assume 50% guilty suspects
  utility_matrix = utility_matrix,
  lineup_size = 6
)

print(util_result)

# Plot utility curves
plot_utility_curves(util_result)

## ----utility-rule-------------------------------------------------------------
utility_rule <- function(m) {
  if (m$strategy == "reject_all") "Act on no identification"
  else paste0("Act on confidence >= ", m$criterion)
}

## ----utility-comparison-------------------------------------------------------
# Compare the two procedures simulated in the Calibration by Condition section
util_proc_a <- make_utility_curves(data_proc_a, base_rate = 0.5,
                                   utility_matrix = utility_matrix)
util_proc_b <- make_utility_curves(data_proc_b, base_rate = 0.5,
                                   utility_matrix = utility_matrix)

knitr::kable(
  data.frame(
    Procedure = c("A (d' = 1.8)", "B (d' = 1.1)"),
    `Max expected utility` = round(c(util_proc_a$max_utility$expected_utility,
                                     util_proc_b$max_utility$expected_utility), 3),
    `Optimal rule` = c(utility_rule(util_proc_a$max_utility),
                       utility_rule(util_proc_b$max_utility)),
    check.names = FALSE
  ),
  caption = "Expected utility at the optimal confidence criterion, simulated data (400 target-present and 400 target-absent lineups per procedure)."
)

## ----utility-base-rates-------------------------------------------------------
# Test multiple base rates
base_rates <- c(0.2, 0.5, 0.8)

sensitivity <- do.call(rbind, lapply(base_rates, function(br) {
  util <- make_utility_curves(lineup_example, base_rate = br,
                              utility_matrix = utility_matrix,
                              lineup_size = 6)
  data.frame(
    `Base rate` = br,
    `Max expected utility` = round(util$max_utility$expected_utility, 3),
    `Optimal rule` = utility_rule(util$max_utility),
    check.names = FALSE
  )
}))

knitr::kable(
  sensitivity,
  row.names = FALSE,
  caption = "Sensitivity of expected utility to the assumed base rate, lineup_example data."
)

## ----dpp----------------------------------------------------------------------
dpp_result <- make_dpp(
  lineup_example,
  lineup_size = 6
)

dpp_result$dpp
dpp_result$dpp_by_point

plot_dpp(dpp_result)

## ----dpp-weighted-------------------------------------------------------------
# An innocent-suspect identification three times as costly as a miss,
# with culprits present in 30% of lineups
make_dpp(lineup_example, alpha = 3, beta = 0.3)$dpp

## ----dpp-comparison, eval = precompute----------------------------------------
# # Stored with the vignette; set R4LINEUPS_PRECOMPUTE=true to recompute.
# dpp_comparison <- compare_dpp(
#   data_proc_a,
#   data_proc_b,
#   lineup_size = 6,
#   n_bootstrap = 500,
#   seed = 2026
# )

## ----dpp-comparison-stored, include = FALSE-----------------------------------
stored("dpp_comparison", "dpp_comparison")

## ----dpp-comparison-results---------------------------------------------------
dpp_comparison

plot_dpp_comparison(dpp_comparison)

## ----anri---------------------------------------------------------------------
# Compute ANRI
anri_result <- compute_anri(
  lineup_example,
  confidence_bins = c(0, 60, 80, 100)
)

print(anri_result)

## ----anri-bootstrap-----------------------------------------------------------
anri_boot <- bootstrap_anri(
  lineup_example,
  confidence_bins = c(0, 60, 80, 100),
  n_bootstrap = 100,
  conf_level = 0.95,
  seed = 123
)

knitr::kable(
  data.frame(
    Statistic = c("NRI (uncorrected)", "ANRI (bias-corrected)"),
    Estimate  = round(c(anri_boot$nri, anri_boot$anri), 3),
    `95% CI`  = c("—", sprintf("[%.3f, %.3f]",
                               anri_boot$ci_lower, anri_boot$ci_upper)),
    check.names = FALSE
  ),
  row.names = FALSE,
  caption = "Resolution with bootstrap interval, lineup_example, 3 confidence bins."
)

## ----anri-comparison----------------------------------------------------------
anri_comparison <- compare_anri(
  procedures,
  group_var = "procedure",
  confidence_bins = c(0, 60, 80, 100),
  n_bootstrap = 100,
  seed = 456
)

print(anri_comparison)

plot_anri_comparison(anri_comparison)

## ----comprehensive-analysis---------------------------------------------------
bins <- c(0, 60, 80, 100)

# 1. Calibration: does confidence mean what it says?
cal <- make_calibration(lineup_example, confidence_bins = bins,
                        show_plot = FALSE)

# 2. DPP: average utility across operating points
dpp <- make_dpp(lineup_example, lineup_size = 6)

# 3. ANRI: resolution, with inference
anri <- bootstrap_anri(lineup_example, confidence_bins = bins,
                       n_bootstrap = 100, seed = 789)

# 4. Utility: what should be done about it?
util <- make_utility_curves(lineup_example, base_rate = 0.5,
                            utility_matrix = utility_matrix,
                            lineup_size = 6)

# 5. Bayesian: what is an identification worth as evidence?
bayes <- make_bayes_curves(lineup_example, response_categories = "confidence",
                           confidence_bins = bins)

knitr::kable(
  data.frame(
    Method   = c("Calibration (C)", "Over/underconfidence (O/U)",
                 "DPP", "ANRI", "Max expected utility"),
    Estimate = round(c(cal$C, cal$OU, dpp$dpp, anri$anri,
                       util$max_utility$expected_utility), 3)
  ),
  row.names = FALSE,
  caption = "Summary across all five methods, lineup_example."
)

## ----writeup-values, include = FALSE------------------------------------------
hi <- lik[lik$response == "suspect_(80,100]", ]
hi_lr <- hi$p_x_given_guilty / hi$p_x_given_innocent
ou_dir <- if (cal$OU < 0) "underconfident" else "overconfident"

## ----session-info-------------------------------------------------------------
sessionInfo()

