## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  message = FALSE
)

## -----------------------------------------------------------------------------
library(r4lineups)
data(lineup_example)
head(lineup_example)

## -----------------------------------------------------------------------------
counts <- c(
  n_tp_suspect = sum(lineup_example$target_present & lineup_example$identification == "suspect"),
  n_tp_filler = sum(lineup_example$target_present & lineup_example$identification == "filler"),
  n_tp_reject = sum(lineup_example$target_present & lineup_example$identification == "reject"),
  n_ta_suspect = sum(!lineup_example$target_present & lineup_example$identification == "suspect"),
  n_ta_filler = sum(!lineup_example$target_present & lineup_example$identification == "filler"),
  n_ta_reject = sum(!lineup_example$target_present & lineup_example$identification == "reject")
)

fit <- fit_winter_2ht(counts, lineup_size = 6)
fit$parameters

## ----plot_params--------------------------------------------------------------
plot_2ht_parameters(fit)

## ----plot_fit-----------------------------------------------------------------
plot_2ht_fit(fit)

## -----------------------------------------------------------------------------
set.seed(123)
boot_fit <- boot_winter_2ht(
  counts,
  lineup_size = 6,
  nboot = 200,
  conf_level = 0.95,
  method = "percentile"
)

boot_fit$ci

## ----plot_boot----------------------------------------------------------------
plot_2ht_parameters(boot_fit)

## ----fairness_demo------------------------------------------------------------
scenarios <- list(
  "Fair: foils match suspect"      = rep( 0.0, 5),
  "Uniformly weak foils"           = rep(-1.6, 5),
  "Uneven foils, suspect typical"  = c( 1.6,  0.9, -1.5, -2.5, -3.5),
  "Uneven foils, one dominant"     = c( 2.5, -3.0, -3.0, -3.0, -3.0)
)

fairness <- do.call(rbind, lapply(names(scenarios), function(nm) {
  d <- as.data.frame(simulate_lineup_data(
    n_tp = 1500, n_ta = 1500, d_prime = 1.4, c_criterion = 0.9,
    lineup_size = 6, conf_levels = 5,
    foil_bias = scenarios[[nm]], seed = 101
  ))
  fit_s <- fit_winter_2ht(d, lineup_size = 6,
                          target_present = "target_present",
                          identification = "identification")

  ta     <- d[!d$target_present, ]
  chosen <- ta$lineup_member[ta$lineup_member > 0]
  E      <- esize_T(table(factor(chosen, levels = 1:6)))

  data.frame(
    `Lineup`              = nm,
    `b (2-HT)`            = round(fit_s$parameters[["b"]], 3),
    `E' (Tredoux)`        = round(E, 2),
    `dP (2-HT)`           = round(fit_s$parameters[["dP"]], 3),
    check.names = FALSE
  )
}))

knitr::kable(
  fairness, row.names = FALSE,
  caption = "The 2-HT bias parameter against effective size (Tredoux, 1998), on the same simulated data (1500 target-present and 1500 target-absent lineups, nominal size 6)."
)

