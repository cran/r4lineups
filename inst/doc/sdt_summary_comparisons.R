## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      warning = FALSE, message = FALSE)

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

## -----------------------------------------------------------------------------
library(r4lineups)

# Condition A counts
hits_a <- 34
fas_a <- 6
misses_a <- 16
cr_a <- 44

summ_a <- sdt_summary_from_counts(hits_a, fas_a, misses_a, cr_a)
summ_a

## -----------------------------------------------------------------------------
var_a_miller <- sdt_summary_variance(hits_a, fas_a, misses_a, cr_a, method = "miller")
var_a_miller

## -----------------------------------------------------------------------------
var_a_gouv <- sdt_summary_variance(hits_a, fas_a, misses_a, cr_a, method = "gourevitch")
var_a_gouv

## -----------------------------------------------------------------------------
set.seed(1)
var_a_boot <- sdt_summary_variance(hits_a, fas_a, misses_a, cr_a,
                                   method = "bootstrap", nboot = 1000)
var_a_boot

## -----------------------------------------------------------------------------
# Condition B counts
hits_b <- 28
fas_b <- 10
misses_b <- 22
cr_b <- 40

compare_sdt_summary(hits_a, fas_a, misses_a, cr_a,
                    hits_b, fas_b, misses_b, cr_b,
                    metric = "dprime",
                    method = "miller")

## -----------------------------------------------------------------------------
compare_sdt_summary(hits_a, fas_a, misses_a, cr_a,
                    hits_b, fas_b, misses_b, cr_b,
                    metric = "c",
                    method = "bootstrap",
                    nboot = 2000)

## ----compound_fit, eval = precompute------------------------------------------
# # Stored with the vignette; set R4LINEUPS_PRECOMPUTE=true to recompute.
# fit_free <- fit_max_sdt(
#   n_hit = 69, n_tp_choose = 82, n_fa = 64, N_tp = 96, N_ta = 106,
#   n_hit_2 = 68, n_tp_choose_2 = 78, n_fa_2 = 42, N_tp_2 = 96, N_ta_2 = 96,
#   n = 6, nboot = 300, seed = 11
# )

## ----compound_fit_stored, include = FALSE-------------------------------------
stored("compound_fit", "fit_free")

## ----compound_table-----------------------------------------------------------
knitr::kable(
  data.frame(
    Condition = c("Condition 1", "Condition 2"),
    `d'`      = round(c(fit_free$dprime_1, fit_free$dprime_2), 3),
    `95% CI`  = c(sprintf("[%.2f, %.2f]", fit_free$boot_ci["lower", "d1"],
                                          fit_free$boot_ci["upper", "d1"]),
                  sprintf("[%.2f, %.2f]", fit_free$boot_ci["lower", "d2"],
                                          fit_free$boot_ci["upper", "d2"])),
    `Criterion` = round(c(fit_free$lambda_1, fit_free$lambda_2), 3),
    check.names = FALSE
  ),
  row.names = FALSE,
  caption = "Compound (MAX SDT) model fitted to the Reisberg & Pezdek Profile-lineup counts, with parametric bootstrap intervals from 300 replicates."
)

## ----compound_diagnostics-----------------------------------------------------
cat("Goodness of fit: chi-square =", round(fit_free$chisq, 3),
    "on", fit_free$df, "df, p =", round(fit_free$p_value, 4), "
")
cat("Bootstrap replicates used:", fit_free$boot_n_used,
    " failed:", fit_free$boot_n_failed, "
")

## ----compound_compare---------------------------------------------------------
fit_equal_d <- fit_max_sdt(
  n_hit = 69, n_tp_choose = 82, n_fa = 64, N_tp = 96, N_ta = 106,
  n_hit_2 = 68, n_tp_choose_2 = 78, n_fa_2 = 42, N_tp_2 = 96, N_ta_2 = 96,
  n = 6, constrain_d = TRUE
)

compare_max_sdt(fit_free, fit_equal_d)

