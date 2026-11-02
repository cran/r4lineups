## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  message = FALSE
)

## ----libraries----------------------------------------------------------------
library(r4lineups)
library(ggplot2)

## ----range_comparison---------------------------------------------------------
data(lineup_example)

partial <- make_roc(lineup_example, lineup_size = 6, show_plot = FALSE)
full    <- make_fullroc(lineup_example, lineup_size = 6, show_plot = FALSE)

knitr::kable(
  data.frame(
    Method = c("Partial ROC (suspect IDs only)", "Full ROC (all responses)"),
    `False-ID axis spans` = c(
      sprintf("0 to %.3f", max(partial$roc_data$false_id_rate)),
      sprintf("0 to %.3f", max(full$roc_data$cumulative_false_alarm_rate))
    ),
    `Area reported` = c(
      sprintf("pAUC = %.4f", partial$pauc),
      sprintf("AUC = %.4f", full$auc)
    ),
    check.names = FALSE
  ),
  row.names = FALSE,
  caption = "Range covered by the two methods, lineup_example (700 target-present and 700 target-absent lineups of size 6)."
)

## ----data_structure-----------------------------------------------------------
str(lineup_example)

## ----compute_fullroc----------------------------------------------------------
result <- make_fullroc(
  lineup_example,
  lineup_size = 6,
  order       = "apriori",
  show_plot   = FALSE
)

print(result)

## ----fullroc_plot-------------------------------------------------------------
result$plot + theme_minimal(base_size = 12)

## ----bias_demo----------------------------------------------------------------
null_auc <- function(n_per_arm, n_reps = 15, seed = 7) {
  set.seed(seed)
  out <- t(vapply(seq_len(n_reps), function(i) {
    d <- data.frame(
      target_present = rep(c(TRUE, FALSE), each = n_per_arm),
      identification = sample(c("suspect", "filler", "reject"),
                              2 * n_per_arm, replace = TRUE,
                              prob = c(0.2, 0.3, 0.5)),
      confidence     = sample(seq(20, 100, 20), 2 * n_per_arm, replace = TRUE)
    )
    c(diagnosticity = make_fullroc(d, lineup_size = 6,
                                   show_plot = FALSE)$auc,
      apriori       = make_fullroc(d, lineup_size = 6, order = "apriori",
                                   show_plot = FALSE)$auc)
  }, numeric(2)))
  colMeans(out)
}

bias_table <- do.call(rbind, lapply(c(100, 200, 500, 1000), function(n) {
  a <- null_auc(n)
  data.frame(`Lineups per arm` = n,
             `Diagnosticity ordering` = round(a[["diagnosticity"]], 3),
             `A-priori ordering` = round(a[["apriori"]], 3),
             check.names = FALSE)
}))

knitr::kable(
  bias_table, row.names = FALSE,
  caption = "Mean AUC from data containing NO signal at all, so the true AUC is 0.500. Simulated, 15 replications per row, 15 decision-by-confidence categories."
)

## ----binning------------------------------------------------------------------
binned <- make_fullroc(
  lineup_example,
  conf_bins   = c(0, 60, 80, 100),
  order       = "apriori",
  lineup_size = 6,
  show_plot   = FALSE
)

knitr::kable(
  data.frame(
    Version = c("All confidence levels", "Three bins (0-60, 60-80, 80-100)"),
    `Operating points` = c(nrow(result$roc_data), nrow(binned$roc_data)),
    AUC = round(c(result$auc, binned$auc), 4),
    check.names = FALSE
  ),
  row.names = FALSE,
  caption = "Effect of binning confidence, lineup_example, a-priori ordering."
)

## ----diagnosticity_table------------------------------------------------------
knitr::kable(
  head(as.data.frame(result$diagnosticity_table)[
    , c("decision", "conf_bin", "hit_rate", "false_alarm_rate",
        "diagnosticity_ratio")], 8),
  digits = 4, row.names = FALSE,
  caption = "Non-cumulative response rates and diagnosticity ratios, lineup_example (first 8 categories under a-priori ordering)."
)

## ----roc_data-----------------------------------------------------------------
knitr::kable(
  head(as.data.frame(result$roc_data), 6),
  digits = 4, row.names = FALSE,
  caption = "Cumulative rates defining the ROC operating points, lineup_example."
)

## ----comparison---------------------------------------------------------------
strong <- as.data.frame(simulate_lineup_data(
  n_tp = 500, n_ta = 500, d_prime = 2.0, c_criterion = 0.9,
  lineup_size = 6, conf_levels = 5,
  foil_bias = c(0.4, 0.2, 0.0, -0.4, -1.0), seed = 11
))

weak <- as.data.frame(simulate_lineup_data(
  n_tp = 500, n_ta = 500, d_prime = 1.2, c_criterion = 0.9,
  lineup_size = 6, conf_levels = 5,
  foil_bias = c(0.4, 0.2, 0.0, -0.4, -1.0), seed = 12
))

strong_roc <- make_fullroc(strong, lineup_size = 6, order = "apriori",
                           show_plot = FALSE)
weak_roc   <- make_fullroc(weak,   lineup_size = 6, order = "apriori",
                           show_plot = FALSE)

knitr::kable(
  data.frame(
    Condition = c("Strong memory (d' = 2.0)", "Weak memory (d' = 1.2)"),
    AUC = round(c(strong_roc$auc, weak_roc$auc), 4)
  ),
  row.names = FALSE,
  caption = "Full ROC AUC by viewing condition, simulated data (500 target-present and 500 target-absent lineups per condition, a-priori ordering)."
)

## ----comparison_plot----------------------------------------------------------
curves <- rbind(
  transform(as.data.frame(strong_roc$roc_data), Condition = "Strong (d' = 2.0)"),
  transform(as.data.frame(weak_roc$roc_data),   Condition = "Weak (d' = 1.2)")
)

ggplot(curves, aes(x = cumulative_false_alarm_rate, y = cumulative_hit_rate,
                   colour = Condition)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", colour = "grey60") +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  coord_equal() +
  labs(
    x = "Cumulative false alarm rate",
    y = "Cumulative hit rate",
    title = "Full ROC curves for two viewing conditions",
    subtitle = "Simulated data, 500 target-present and 500 target-absent lineups per condition"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

