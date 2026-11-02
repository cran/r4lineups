## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  message = FALSE
)

## ----load_package-------------------------------------------------------------
library(r4lineups)
library(ggplot2)

## ----simulate_data------------------------------------------------------------
set.seed(123)

lineup_data <- as.data.frame(simulate_lineup_data(
  n_tp = 600, n_ta = 600,
  d_prime     = 1.6,
  c_criterion = 0.9,
  lineup_size = 6,
  conf_levels = 9,
  foil_bias   = c(0.4, 0.2, 0.0, -0.5, -1.1),
  seed        = 123
))

# Response time falls as evidence strength (here, ordinal confidence) rises.
# The noise term is deliberately large: response time is a noisy indicator.
lineup_data$response_time <- round(pmax(
  800,
  14000 - 900 * lineup_data$confidence + rnorm(nrow(lineup_data), 0, 3500)
))

# Put confidence on a 20-100 scale, as the other vignettes do
lineup_data$confidence <- round(seq(20, 100, length.out = 9))[lineup_data$confidence]

table(lineup_data$target_present, lineup_data$identification)

## ----check_correlation--------------------------------------------------------
cor(lineup_data$response_time, lineup_data$confidence)

## ----point_biserial-----------------------------------------------------------
# All trials: "correct" means a suspect ID when the target was present, or a
# rejection when it was absent
all_trials <- lineup_data
all_trials$correct <- as.integer(
  (all_trials$target_present  & all_trials$identification == "suspect") |
  (!all_trials$target_present & all_trials$identification == "reject")
)

# Suspect identifications only. Among these, "correct" is simply target-present
choosers <- lineup_data[lineup_data$identification == "suspect", ]
choosers$correct <- as.integer(choosers$target_present)

knitr::kable(
  data.frame(
    Sample = c("All trials", "Suspect identifications only"),
    n = c(nrow(all_trials), nrow(choosers)),
    `Point-biserial r` = round(c(
      cor(all_trials$response_time, all_trials$correct),
      cor(choosers$response_time,   choosers$correct)
    ), 3),
    check.names = FALSE
  ),
  row.names = FALSE,
  caption = "Correlation between response time and accuracy, simulated data (1200 lineups of size 6)."
)

## ----time_bins----------------------------------------------------------------
# What is the actual range?
round(quantile(choosers$response_time, c(0, 0.25, 0.5, 0.75, 1)))

time_bins <- c(0, 5000, 7500, 10000, Inf)

## ----compute_rac--------------------------------------------------------------
rac_result <- make_rac(
  data        = lineup_data,
  lineup_size = 6,
  time_bins   = time_bins,
  show_plot   = FALSE
)

print(rac_result)

## ----plot_rac-----------------------------------------------------------------
rac_result$plot + theme_minimal(base_size = 12)

## ----rac_vs_cac---------------------------------------------------------------
rac <- make_rac(lineup_data, time_bins = time_bins, show_plot = FALSE)
cac <- make_cac(lineup_data, confidence_bins = c(0, 60, 80, 100),
                show_plot = FALSE)

rac$plot + ggtitle("RAC: response time and accuracy") +
  theme_minimal(base_size = 12)

## ----cac_plot-----------------------------------------------------------------
cac$plot + ggtitle("CAC: confidence and accuracy") +
  theme_minimal(base_size = 12)

## ----fine_bins----------------------------------------------------------------
fine_bins <- c(seq(0, 20000, by = 2500), Inf)

rac_fine <- make_rac(lineup_data, time_bins = fine_bins, show_plot = FALSE,
                     show_errorbars = TRUE, show_n = TRUE)

knitr::kable(
  as.data.frame(rac_fine$rac_data)[, c("response_time", "n_total", "accuracy")],
  digits = 3, row.names = FALSE,
  caption = "RAC with 2.5-second bins plus an overflow bin, simulated data. Note the small n in the extreme bins."
)

## ----continuous_rac-----------------------------------------------------------
fit <- glm(correct ~ splines::ns(response_time, df = 3),
           family = binomial, data = choosers)

grid <- data.frame(
  response_time = seq(min(choosers$response_time),
                      max(choosers$response_time), length.out = 200)
)
pred <- predict(fit, newdata = grid, type = "link", se.fit = TRUE)
grid$accuracy <- plogis(pred$fit)
grid$lower    <- plogis(pred$fit - 1.96 * pred$se.fit)
grid$upper    <- plogis(pred$fit + 1.96 * pred$se.fit)

## ----continuous_plot----------------------------------------------------------
ggplot(grid, aes(x = response_time / 1000, y = accuracy)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "steelblue") +
  geom_line(linewidth = 1, colour = "steelblue") +
  geom_hline(yintercept = 0.9, linetype = "dashed", colour = "red") +
  geom_rug(data = choosers,
           aes(x = response_time / 1000, y = NULL), alpha = 0.15) +
  scale_y_continuous(limits = c(0.5, 1)) +
  labs(
    x = "Response time (seconds)",
    y = "Estimated accuracy",
    title = "Continuous RAC: accuracy as a smooth function of response time",
    subtitle = "Simulated data, 600 target-present and 600 target-absent lineups. Band = 95% CI; dashed line = 90% accuracy"
  ) +
  theme_minimal(base_size = 12)

## ----time_boundary------------------------------------------------------------
above <- grid$response_time[grid$accuracy >= 0.90]
boundary <- if (length(above) > 0) max(above) else NA

# The fitted curve could cross 90% more than once. Count the crossings, so a
# single "boundary" is not reported when accuracy dips and recovers.
n_crossings <- sum(diff(grid$accuracy >= 0.90) != 0)

cat("Estimated accuracy at  3 s:", round(approx(grid$response_time, grid$accuracy, 3000)$y, 3), "\n")
cat("Estimated accuracy at  8 s:", round(approx(grid$response_time, grid$accuracy, 8000)$y, 3), "\n")
cat("Estimated accuracy at 15 s:", round(approx(grid$response_time, grid$accuracy, 15000)$y, 3), "\n\n")
cat("Accuracy is at least 90% up to about", round(boundary / 1000, 1), "seconds\n")
cat("Crossings of the 90% line:", n_crossings, "\n")

## ----compare_conditions-------------------------------------------------------
make_condition <- function(d_prime, seed) {
  d <- as.data.frame(simulate_lineup_data(
    n_tp = 400, n_ta = 400, d_prime = d_prime, c_criterion = 0.9,
    lineup_size = 6, conf_levels = 9,
    foil_bias = c(0.4, 0.2, 0.0, -0.5, -1.1), seed = seed
  ))
  d$response_time <- round(pmax(
    800, 14000 - 900 * d$confidence + rnorm(nrow(d), 0, 3500)
  ))
  d$confidence <- round(seq(20, 100, length.out = 9))[d$confidence]
  d
}

data_condition1 <- make_condition(d_prime = 2.0, seed = 201)  # good viewing
data_condition2 <- make_condition(d_prime = 1.0, seed = 202)  # poor viewing

condition1_rac <- make_rac(data_condition1, time_bins = time_bins,
                           show_plot = FALSE)
condition2_rac <- make_rac(data_condition2, time_bins = time_bins,
                           show_plot = FALSE)

comparison <- data.frame(
  `Time bin` = condition1_rac$rac_data$response_time,
  `Condition 1 (d' = 2.0)` = round(condition1_rac$rac_data$accuracy, 3),
  `Condition 2 (d' = 1.0)` = round(condition2_rac$rac_data$accuracy, 3),
  check.names = FALSE
)

knitr::kable(
  comparison, row.names = FALSE,
  caption = "RAC accuracy by condition, simulated data (400 target-present and 400 target-absent lineups per condition)."
)

## ----custom_analysis----------------------------------------------------------
rac_data <- rac_result$rac_data

ggplot(rac_data, aes(x = mean_time / 1000, y = accuracy)) +
  geom_point(aes(size = n_total), colour = "darkblue", alpha = 0.6) +
  geom_line(colour = "darkblue", alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(
    x = "Mean response time (seconds)",
    y = "Accuracy",
    title = "Binned RAC, simulated data",
    size = "N suspect IDs"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 0.5, linetype = "dashed")

## ----real_data_structure------------------------------------------------------
my_data <- data.frame(
  target_present = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE),
  identification = c("suspect", "filler", "suspect", "suspect",
                     "reject", "suspect"),
  response_time  = c(8234, 12456, 6789, 15020, 9310, 4870)
)

str(my_data)

## ----export-------------------------------------------------------------------
# As CSV. Written to a temporary directory here; use your own path
outfile <- file.path(tempdir(), "rac_analysis.csv")
write.csv(rac_result$rac_data, outfile, row.names = FALSE)

## ----export_table-------------------------------------------------------------
knitr::kable(rac_result$rac_data, digits = 3,
             caption = "RAC analysis results, simulated data.")

