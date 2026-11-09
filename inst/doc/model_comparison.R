## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  message = FALSE
)

## ----load_data----------------------------------------------------------------
library(r4lineups)

lineup_data <- as.data.frame(simulate_lineup_data(
  n_tp        = 300,
  n_ta        = 300,
  d_prime     = 1.5,
  c_criterion = 0.9,   # see the note below: 0 would be far too liberal
  lineup_size = 6,
  conf_levels = 5,
  foil_bias   = c(0.4, 0.2, 0.0, -0.4, -1.0),
  seed        = 2024
))

head(lineup_data)

## ----criterion_warning--------------------------------------------------------
too_liberal <- as.data.frame(simulate_lineup_data(
  n_tp = 300, n_ta = 300, d_prime = 1.5, c_criterion = 0,
  lineup_size = 6, conf_levels = 5, seed = 2024
))

knitr::kable(
  data.frame(
    `c_criterion` = c(0, 0.9),
    `Rejection rate (%)` = round(100 * c(
      mean(too_liberal$identification == "reject"),
      mean(lineup_data$identification == "reject")
    ), 1),
    check.names = FALSE
  ),
  row.names = FALSE,
  caption = "Rejection rate under two decision criteria, simulated data (300 target-present and 300 target-absent lineups)."
)

## ----compare_all_models-------------------------------------------------------
# Fit all three models
comparison <- compare_models(
  lineup_data,
  models = c("2ht", "eig", "fullroc"),
  lineup_size = 6,
  prior_guilt = 0.5
)

# View comparison table
print(comparison)

## ----detailed_summary---------------------------------------------------------
# More detailed output
summary(comparison)

## ----visualize_models, fig.width=10, fig.height=4.5---------------------------
# Side-by-side plots
plot(comparison, ncol = 3)

## ----model_2ht----------------------------------------------------------------
# Access 2-HT model
model_2ht <- comparison$fitted_models$`2ht`

knitr::kable(
  data.frame(Parameter = names(model_2ht$parameters),
             Estimate  = round(unname(model_2ht$parameters), 3),
             SE        = round(unname(model_2ht$se), 3)),
  row.names = FALSE,
  caption = "2-HT parameter estimates, simulated data. Standard errors are NA because b is estimated on its boundary."
)

cat("AIC:", round(model_2ht$aic, 2),
    " BIC:", round(model_2ht$bic, 2),
    " log-likelihood:", round(model_2ht$loglik, 2), "\n")

## ----model_eig----------------------------------------------------------------
# Access EIG model
model_eig <- comparison$fitted_models$eig

# View EIG value
cat("Expected Information Gain:", round(model_eig$eig, 4), "bits\n")
cat("Information Efficiency:",
    round(model_eig$eig / model_eig$prior_entropy * 100, 1), "%\n")

# Most informative responses
cat("\nTop 5 most informative response categories:\n")
print(head(model_eig$response_data[, c("response", "information_gain",
                                        "posterior_guilty")], 5))

## ----model_fullroc------------------------------------------------------------
# Access Full ROC model
model_fullroc <- comparison$fitted_models$fullroc

# View AUC
cat("Full ROC AUC:", round(model_fullroc$auc, 4), "\n")
cat("Operating Points:", model_fullroc$summary$n_operating_points, "\n")

# View diagnosticity table (top 10 points)
cat("\nTop 10 most diagnostic evidence categories:\n")
print(head(model_fullroc$diagnosticity_table[, c("evidence_label",
                                                  "diagnosticity_ratio")], 10))

## ----model_selection----------------------------------------------------------
cat("2-HT Model Fit:\n")
cat("  AIC:", round(comparison$fitted_models$`2ht`$aic, 2), "\n")
cat("  BIC:", round(comparison$fitted_models$`2ht`$bic, 2), "\n")
cat("\nAIC and BIC compare likelihood-based models fitted to the same data.\n")
cat("EIG and full-ROC AUC are not likelihoods, so they cannot be ranked\n")
cat("against the 2-HT model by information criteria.\n")

## ----specific_models----------------------------------------------------------
# Fit only 2-HT and EIG (skip Full ROC)
comparison_subset <- compare_models(
  lineup_data,
  models = c("2ht", "eig"),
  lineup_size = 6,
  prior_guilt = 0.5
)

print(comparison_subset)

## ----confidence_bins----------------------------------------------------------
# Bin confidence for EIG and Full ROC
comparison_binned <- compare_models(
  lineup_data,
  models = c("eig", "fullroc"),
  lineup_size = 6,
  confidence_bins = c(0, 40, 70, 100)  # Low, medium, high
)

cat("Binned confidence results:\n")
print(comparison_binned)

## ----custom_prior-------------------------------------------------------------
# Use different prior probability for EIG
comparison_prior <- compare_models(
  lineup_data,
  models = "eig",
  prior_guilt = 0.3  # Assume 30% base rate
)

cat("EIG with prior = 0.3:\n")
print(comparison_prior$fitted_models$eig$eig)

## ----simulation_example-------------------------------------------------------
sim_data <- as.data.frame(simulate_lineup_data(
  n_tp = 300, n_ta = 300, d_prime = 1.5, c_criterion = 0.9,
  lineup_size = 6, conf_levels = 5,
  foil_bias = c(0.4, 0.2, 0.0, -0.4, -1.0), seed = 2026
))

sim_comparison <- compare_models(
  sim_data,
  models = c("2ht", "eig", "fullroc"),
  lineup_size = 6
)

print(sim_comparison)

## ----parameter_recovery-------------------------------------------------------
recovery <- do.call(rbind, lapply(c(0.5, 1.0, 1.5, 2.0, 2.5), function(dp) {
  d <- as.data.frame(simulate_lineup_data(
    n_tp = 400, n_ta = 400, d_prime = dp, c_criterion = 0.9,
    lineup_size = 6, conf_levels = 5,
    foil_bias = c(0.4, 0.2, 0.0, -0.4, -1.0), seed = 500 + round(10 * dp)
  ))
  fit <- fit_winter_2ht(d, lineup_size = 6,
                        target_present = "target_present",
                        identification = "identification")
  data.frame(`True d'` = dp,
             `Fitted dP` = round(fit$parameters[["dP"]], 3),
             `Fitted dA` = round(fit$parameters[["dA"]], 3),
             check.names = FALSE)
}))

knitr::kable(
  recovery, row.names = FALSE,
  caption = "2-HT detection parameters fitted to data simulated from a signal detection model at known d', 400 lineups per arm."
)

## ----example_procedure--------------------------------------------------------
# Fit all models to assess procedure
procedure_eval <- compare_models(
  lineup_data,
  models = c("2ht", "eig", "fullroc")
)

fm <- procedure_eval$fitted_models
knitr::kable(
  data.frame(
    Measure  = c("2-HT dP (SE)", "EIG, bits (share of prior entropy)",
                 "Full ROC AUC (diagnosticity ordering)"),
    Estimate = c(sprintf("%.3f (%.3f)", fm$`2ht`$parameters[["dP"]],
                         fm$`2ht`$se[["dP"]]),
                 sprintf("%.3f (%.0f%%)", fm$eig$eig,
                         100 * fm$eig$eig / fm$eig$prior_entropy),
                 sprintf("%.3f", fm$fullroc$auc))
  ),
  row.names = FALSE,
  caption = "Three summaries of the same simulated procedure, prior guilt .5."
)

## ----example_bias-------------------------------------------------------------
params <- comparison$fitted_models$`2ht`$parameters

model_2ht <- comparison$fitted_models$`2ht`
knitr::kable(
  data.frame(Parameter = "b (biased suspect selection)",
             Estimate = round(params[["b"]], 3),
             `95% Wald interval` = sprintf("[%.3f, %.3f]",
               max(0, params[["b"]] - 1.96 * model_2ht$se[["b"]]),
               min(1, params[["b"]] + 1.96 * model_2ht$se[["b"]])),
             check.names = FALSE),
  row.names = FALSE,
  caption = "2-HT bias parameter, simulated data (300 target-present and 300 target-absent lineups)."
)

## ----example_detection--------------------------------------------------------
knitr::kable(
  data.frame(
    Parameter = c("dP (detect culprit present)",
                  "dA (detect culprit absent)",
                  "g (guessing-based selection)"),
    Estimate  = round(c(params[["dP"]], params[["dA"]], params[["g"]]), 3)
  ),
  row.names = FALSE,
  caption = "2-HT detection and guessing parameters, simulated data."
)

## ----table_console------------------------------------------------------------
# Console-friendly table
table_console <- format_comparison_table(comparison, format = "console")
print(table_console)

## ----table_markdown, results='asis'-------------------------------------------
table_md <- format_comparison_table(comparison, format = "markdown")

# One element per line, so sep = "\n" is required. Plain cat(table_md) joins
# the lines with spaces and the table collapses into a single paragraph.
cat(table_md, sep = "\n")

## ----pitfall_sample_size------------------------------------------------------
# Small sample may lead to unreliable estimates
small_data <- simulate_lineup_data(n_tp = 30, n_ta = 30, d_prime = 1.5, conf_levels = 5)

small_comparison <- compare_models(
  small_data,
  models = c("2ht", "eig"),
  show_warnings = FALSE
)

cat("Warning: Small samples (n=60) may produce unstable parameter estimates\n")
cat("Recommend: n ≥ 100 per condition for reliable model fitting\n")

## ----pitfall_comparison-------------------------------------------------------
cat("Important: Don't directly compare across models!\n\n")
cat("✗ Wrong: 'dP = 0.6 is better than EIG = 0.4'\n")
cat("  → These are different scales and constructs\n\n")
cat("✓ Right: 'dP rose from 0.45 to 0.60 between conditions, and EIG\n")
cat("          (prior .5) rose from 0.30 to 0.40 bits'\n")
cat("  → Compare each measure with itself across conditions\n")

## ----best_practice_multiple---------------------------------------------------
# Always fit at least 2-3 models
comprehensive <- compare_models(
  lineup_data,
  models = c("2ht", "eig", "fullroc")
)

cat("Fitting multiple models provides:\n")
cat("  • Process-level understanding (2-HT)\n")
cat("  • Overall diagnosticity (EIG)\n")
cat("  • Threshold-free performance (Full ROC)\n")

## ----best_practice_reporting--------------------------------------------------
cat("In your paper, report:\n")
cat("  • 2-HT: Parameters with SE, AIC/BIC, convergence\n")
cat("  • EIG: Value in bits, efficiency percentage\n")
cat("  • Full ROC: AUC, number of operating points\n")
cat("  • What each model's estimates say about the procedure\n")

## ----best_practice_assumptions------------------------------------------------
summary(comparison$fitted_models$`2ht`)

