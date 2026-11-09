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

## ----eig_compute--------------------------------------------------------------
eig_obj <- make_eig(
  lineup_example,
  confidence_bins = c(0, 60, 80, 100),
  show_plot = FALSE
)

eig_obj$eig

## ----eig_responses------------------------------------------------------------
knitr::kable(
  head(as.data.frame(eig_obj$response_data), 8),
  digits = 4, row.names = FALSE,
  caption = "Per-response information gain and posterior probability of guilt, lineup_example."
)

## ----plot_eig-----------------------------------------------------------------
plot_eig(eig_obj)

## ----plot_eig_posteriors------------------------------------------------------
plot_eig_posteriors(eig_obj)

## ----ppv_designated-----------------------------------------------------------
ppv_designated <- make_ppv_range(
  lineup_example,
  lineup_size = 6,
  show_plots = FALSE
)

knitr::kable(
  data.frame(
    Correction = c("Nominal size", "Effective size", "None"),
    `Overall PPV` = round(c(ppv_designated$ppv_nominal$overall_ppv,
                            ppv_designated$ppv_effective$overall_ppv,
                            ppv_designated$ppv_none$overall_ppv), 4),
    check.names = FALSE
  ),
  row.names = FALSE,
  caption = "PPV under three corrections, lineup_example -- which designates an innocent suspect, so all three coincide."
)

## ----ppv_estimated------------------------------------------------------------
no_designation <- lineup_example
ta_suspect <- !no_designation$target_present &
               no_designation$identification == "suspect"
no_designation$identification[ta_suspect] <- "filler"
no_designation$suspect_pos <- NULL

ppv_estimated <- make_ppv_range(
  no_designation,
  lineup_size = 6,
  innocent_suspect = "estimated",  # state the design rather than rely on "auto"
  show_plots = FALSE
)

knitr::kable(
  data.frame(
    Correction = c("Nominal size (fair lineup)",
                   "Effective size",
                   "None (worst case)"),
    `Overall PPV` = round(c(ppv_estimated$ppv_nominal$overall_ppv,
                            ppv_estimated$ppv_effective$overall_ppv,
                            ppv_estimated$ppv_none$overall_ppv), 4),
    check.names = FALSE
  ),
  row.names = FALSE,
  caption = "PPV under three corrections, same data with the innocent-suspect designation removed. This is the range the method exists to report."
)

## ----effective_sizes----------------------------------------------------------
knitr::kable(
  data.frame(
    `Confidence bin` = ppv_estimated$ppv_effective$ppv_data$confidence,
    `Effective size` = round(ppv_estimated$ppv_effective$ppv_data$effective_size, 2),
    `Error rate`     = round(ppv_estimated$ppv_effective$ppv_data$error_rate, 3),
    check.names = FALSE
  ),
  row.names = FALSE,
  caption = "Effective size estimated within each confidence bin, against a nominal size of 6."
)

## ----plot_ppv_range-----------------------------------------------------------
plot_ppv_range(ppv_estimated)

## ----plot_effective_size------------------------------------------------------
plot_effective_size_conf(ppv_estimated)

