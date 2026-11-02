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

## ----libraries----------------------------------------------------------------
library(r4lineups)
library(ggplot2)

## ----roc_intro_data-----------------------------------------------------------
set.seed(2026)

strong <- as.data.frame(simulate_lineup_data(
  n_tp = 400, n_ta = 400, d_prime = 2.0, c_criterion = 0.9,
  lineup_size = 6, conf_levels = 5,
  foil_bias = c(0.4, 0.2, 0.0, -0.4, -1.0), seed = 1
))

roc_points <- make_rocdata(strong, lineup_size = 6)$roc_data

knitr::kable(
  roc_points[, c("confidence", "n_correct_ids", "n_false_ids",
                 "correct_id_rate", "false_id_rate")],
  digits = 4, row.names = FALSE,
  caption = "ROC points for one simulated condition (400 target-present and 400 target-absent lineups, d' = 2.0). Each row is a confidence threshold: rows lower down admit less confident identifications."
)

## ----roc_truncation_figure, fig.height = 5.5----------------------------------
roc_df <- roc_points[order(roc_points$false_id_rate), ]

ggplot(roc_df, aes(x = false_id_rate, y = correct_id_rate)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", colour = "grey60") +
  geom_line(linewidth = 1, colour = "steelblue") +
  geom_point(size = 2.5, colour = "steelblue") +
  annotate("segment",
           x = max(roc_df$false_id_rate), xend = 1,
           y = max(roc_df$correct_id_rate), yend = 1,
           linetype = "dashed", colour = "firebrick") +
  annotate("text", x = 0.55, y = 0.72, colour = "firebrick", hjust = 0,
           label = "no data here:\nwitnesses never choose\nthis freely") +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    x = "False identification rate",
    y = "Correct identification rate",
    title = "A lineup ROC curve occupies only a corner of the space",
    subtitle = "Simulated data, 400 target-present and 400 target-absent lineups, d' = 2.0"
  ) +
  theme_minimal(base_size = 12)

## ----pauc_region_figure, fig.height = 5---------------------------------------
cutoff <- max(roc_df$false_id_rate)
shade <- rbind(
  data.frame(false_id_rate = 0, correct_id_rate = 0),
  roc_df[, c("false_id_rate", "correct_id_rate")],
  data.frame(false_id_rate = cutoff, correct_id_rate = 0)
)

ggplot(roc_df, aes(x = false_id_rate, y = correct_id_rate)) +
  geom_polygon(data = shade, fill = "steelblue", alpha = 0.25) +
  geom_line(linewidth = 1, colour = "steelblue") +
  geom_point(size = 2.5, colour = "steelblue") +
  labs(
    x = "False identification rate",
    y = "Correct identification rate",
    title = "Partial AUC is the shaded area, over the observed region only",
    subtitle = "Simulated data, d' = 2.0. Note the x-axis stops well short of 1"
  ) +
  theme_minimal(base_size = 12)

## ----region_problem-----------------------------------------------------------
weak <- as.data.frame(simulate_lineup_data(
  n_tp = 400, n_ta = 400, d_prime = 1.4, c_criterion = 0.9,
  lineup_size = 6, conf_levels = 5,
  foil_bias = c(0.4, 0.2, 0.0, -0.4, -1.0), seed = 2
))

reach_strong <- max(make_rocdata(strong, lineup_size = 6)$roc_data$false_id_rate)
reach_weak   <- max(make_rocdata(weak,   lineup_size = 6)$roc_data$false_id_rate)

knitr::kable(
  data.frame(
    Condition = c("Strong (d' = 2.0)", "Weak (d' = 1.4)"),
    `Maximum false-ID rate reached` = round(c(reach_strong, reach_weak), 4),
    check.names = FALSE
  ),
  row.names = FALSE,
  caption = "The two simulated conditions stop at different points, so there is no single region both fully cover."
)

## ----bootstrap_normality, eval = precompute-----------------------------------
# # Stored with the vignette; set R4LINEUPS_PRECOMPUTE=true to recompute.
# comparison <- compare_pauc(
#   strong, weak,
#   lineup_size = 6,
#   label1 = "Strong (d' = 2.0)",
#   label2 = "Weak (d' = 1.4)",
#   n_bootstrap = 600,
#   seed = 123
# )

## ----bootstrap_normality_stored, include = FALSE------------------------------
stored("bootstrap_normality", "comparison")

## ----bootstrap_normality_shape------------------------------------------------
boot_diff <- comparison$bootstrap_results$diff_boot

knitr::kable(
  data.frame(
    Quantity = c("Mean", "SD", "Skewness", "Excess kurtosis",
                 "Shapiro-Wilk p"),
    Value = c(
      round(mean(boot_diff), 5),
      round(sd(boot_diff), 5),
      round(mean((boot_diff - mean(boot_diff))^3) / sd(boot_diff)^3, 3),
      round(mean((boot_diff - mean(boot_diff))^4) / sd(boot_diff)^4 - 3, 3),
      signif(shapiro.test(boot_diff)$p.value, 3)
    )
  ),
  row.names = FALSE,
  caption = "Shape of the bootstrap distribution of the pAUC difference, simulated data (600 bootstrap resamples)."
)

## ----bootstrap_figure---------------------------------------------------------
ggplot(data.frame(diff = boot_diff), aes(x = diff)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = "steelblue", colour = "white", alpha = 0.8) +
  stat_function(fun = dnorm,
                args = list(mean = mean(boot_diff), sd = sd(boot_diff)),
                colour = "firebrick", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(
    x = "pAUC difference (strong - weak)",
    y = "Density",
    title = "Bootstrap distribution of the pAUC difference",
    subtitle = "Simulated data. Red curve = normal with the same mean and SD; dotted line = no difference"
  ) +
  theme_minimal(base_size = 12)

## ----interval_comparison------------------------------------------------------
knitr::kable(
  data.frame(
    Method = c("Wald (normal approximation)", "Bootstrap percentile"),
    Lower = round(c(comparison$ci_diff[["lower"]],
                    unname(quantile(boot_diff, 0.025))), 5),
    Upper = round(c(comparison$ci_diff[["upper"]],
                    unname(quantile(boot_diff, 0.975))), 5)
  ),
  row.names = FALSE,
  caption = "95% confidence intervals for the pAUC difference by two methods, simulated data."
)

## ----basic_comparison---------------------------------------------------------
print(comparison)

## ----comparison_plot, fig.width = 8, fig.height = 6---------------------------
plot(comparison, show_cutoff = TRUE, show_test_results = TRUE)

## ----detailed_summary---------------------------------------------------------
summary(comparison)

## ----custom_cutoff------------------------------------------------------------
comparison_10 <- compare_pauc(
  strong, weak,
  lineup_size = 6,
  max_false_id_rate = 0.10,
  label1 = "Strong", label2 = "Weak",
  n_bootstrap = 300,
  seed = 456
)

knitr::kable(
  data.frame(
    Region = c(sprintf("Default (to %.3f)", comparison$max_false_id_rate),
               "Policy cutoff (to 0.100)"),
    `pAUC difference` = round(c(comparison$pauc_diff,
                                comparison_10$pauc_diff), 5),
    `p-value` = signif(c(comparison$p_value, comparison_10$p_value), 3),
    check.names = FALSE
  ),
  row.names = FALSE,
  caption = "The same two conditions compared over two different regions, simulated data."
)

## ----multiple_comparisons-----------------------------------------------------
medium <- as.data.frame(simulate_lineup_data(
  n_tp = 400, n_ta = 400, d_prime = 1.7, c_criterion = 0.9,
  lineup_size = 6, conf_levels = 5,
  foil_bias = c(0.4, 0.2, 0.0, -0.4, -1.0), seed = 3
))

pairs <- list(
  `Strong vs Medium` = list(strong, medium),
  `Strong vs Weak`   = list(strong, weak),
  `Medium vs Weak`   = list(medium, weak)
)

## ----multiple_comparisons_run, eval = precompute------------------------------
# # Stored with the vignette; set R4LINEUPS_PRECOMPUTE=true to recompute.
# results <- do.call(rbind, lapply(names(pairs), function(nm) {
#   cmp <- compare_pauc(pairs[[nm]][[1]], pairs[[nm]][[2]],
#                       lineup_size = 6, n_bootstrap = 200, seed = 41)
#   data.frame(Comparison = nm,
#              `pAUC difference` = round(cmp$pauc_diff, 5),
#              Z = round(cmp$z_score, 3),
#              p = signif(cmp$p_value, 4),
#              check.names = FALSE)
# }))

## ----multiple_comparisons_stored, include = FALSE-----------------------------
stored("multiple_comparisons", "results")

## ----multiple_comparisons_adjust----------------------------------------------
results$`p (Bonferroni)` <- signif(p.adjust(results$p, method = "bonferroni"), 4)

knitr::kable(
  results, row.names = FALSE,
  caption = "Pairwise pAUC comparisons with Bonferroni adjustment, simulated data (400 target-present and 400 target-absent lineups per condition)."
)

