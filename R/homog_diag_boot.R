#'Homogeneity of diagnosticity ratio with bootstrapped CIs
#'
#'Function for computing bootstrapped estimates of homogeneity of diagnosticity ratio
#'
#'@param lineup_pres_list A list containing k vectors of lineup choices for k lineups, in which the
#'                        target was present
#'@param lineup_abs_list A list containing k vectors of lineup choices for k lineups, in which the
#'                       target was absent
#'@param k Number of members in lineup. Must be specified by user (scalar).
#'@param R Number of bootstrap replications. Defaults to R = 100.
#'@param pos_list Suspect positions for each lineup pair, in the same format as
#'  \code{diag_param()}. This is required; earlier releases attempted to infer
#'  suspect positions from observed choices, which cannot be done validly.
#'@param seed Optional integer seed for reproducible resampling.
#'@param conf.level Confidence level for the percentile intervals and the
#'                  per-pair intervals, and the complement of the level at which
#'                  heterogeneity is declared. Defaults to 0.95.
#'@details Computes bootstrapped diagnosticity ratio with chi-squared estimate,
#'         significance level and confidence intervals for k lineup pairs.
#'
#'         The interval on the pooled ratio and the chi-square answer different
#'         questions, and can disagree. See \code{\link{homog_diag}} for the
#'         distinction; as there, a note is printed whenever the k ratios are
#'         heterogeneous and the pooled estimate should not be reported on its
#'         own.
#'@return Invisibly returns a list containing the observed mean diagnosticity,
#'  chi-square statistic with degrees of freedom and asymptotic p-value,
#'  percentile intervals, a \code{heterogeneous} flag, a \code{parameters} data
#'  frame of per-pair ratios and intervals, all attempted bootstrap draws
#'  (failed rows retained as NA), \code{n_successful}, \code{n_failed}, and
#'  \code{R}. Intervals are conditional on successful draws when failures occur.
#'@references Malpass, R. S. (1981). Effective size and defendant bias in
#'            eyewitness identification lineups. \emph{Law and Human Behavior, 5}(4), 299-309.
#'
#'            Malpass, R. S., Tredoux, C., & McQuiston-Surrett, D. (2007). Lineup
#'            construction and lineup fairness. In R. Lindsay, D. F. Ross, J. D. Read,
#'            & M. P. Toglia (Eds.), \emph{Handbook of Eyewitness Psychology, Vol. 2: Memory for
#'            people} (pp. 155-178). Mahwah, NJ: Lawrence Erlbaum Associates.
#'
#'            Tredoux, C. G. (1998). Statistical inference on measures of lineup fairness.
#'            \emph{Law and Human Behavior, 22}(2), 217-237.
#'
#'            Tredoux, C. (1999). Statistical considerations when determining measures of
#'            lineup size and lineup bias. \emph{Applied Cognitive Psychology}, 13, S9-S26.
#'
#'            Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'            empirically assessing the fairness of a lineup. \emph{Law and Human Behavior,
#'            3}(4), 285-293.
#'@examples
#'#Target present data:
#'A <- rep(1:6, length.out = 100)
#'B <- rep(1:5, length.out = 70)
#'C <- rep(1:4, length.out = 20)
#'lineup_pres_list <- list(A, B, C)
#'rm(A, B, C)
#'
#'#Target absent data:
#'A <- rep(6:1, length.out = 100)
#'B <- rep(5:1, length.out = 70)
#'C <- rep(4:1, length.out = 20)
#'lineup_abs_list <- list(A, B, C)
#'rm(A, B, C)
#'
#'pos_list <- c(3, 2, 1)
#'k <- c(6, 5, 4)
#'homog_diag_boot(lineup_pres_list, lineup_abs_list, k, R = 20,
#'                pos_list = pos_list, seed = 1)
#'
#'@export
#'@importFrom boot boot boot.ci
#'@importFrom stats setNames

homog_diag_boot <- function(lineup_pres_list, lineup_abs_list, k, R = 100,
                            pos_list = NULL, seed = NULL, conf.level = 0.95){
  if (is.null(pos_list)) {
    stop("pos_list is required because suspect positions cannot be inferred from choices.",
         call. = FALSE)
  }
  if (!is.numeric(R) || length(R) != 1L || R < 2 || R != as.integer(R)) {
    stop("R must be an integer of at least 2.", call. = FALSE)
  }
  restore_rng <- .local_seed(seed)
  on.exit(restore_rng(), add = TRUE)

  observed <- diag_param(lineup_pres_list, lineup_abs_list, pos_list, k)
  observed_parts <- t(cbind(var_lnd(observed),
                            ln_diag_ratio(observed, correction = FALSE),
                            d_weights(observed)))
  observed_mean <- d_bar(observed_parts)
  observed_chi <- chi_diag(observed_parts)

  draws <- replicate(R, {
    tp_boot <- lapply(lineup_pres_list, .resample_values)
    ta_boot <- lapply(lineup_abs_list, .resample_values)
    params <- diag_param(tp_boot, ta_boot, pos_list, k)
    tryCatch({
      parts <- t(cbind(var_lnd(params),
                       ln_diag_ratio(params, correction = FALSE),
                       d_weights(params)))
      c(mean = d_bar(parts), chi_square = chi_diag(parts))
    }, error = function(e) c(mean = NA_real_, chi_square = NA_real_))
  })
  valid_draws <- is.finite(draws["mean", ]) & is.finite(draws["chi_square", ])
  if (sum(valid_draws) < max(2L, ceiling(0.5 * R))) {
    stop("Fewer than half of bootstrap samples had positive suspect-ID cells; intervals are not reliable.",
         call. = FALSE)
  }
  n_successful <- sum(valid_draws)
  if (any(!valid_draws)) {
    warning(sum(!valid_draws), " of ", R, " bootstrap samples failed; intervals are conditional on successful samples.", call. = FALSE)
  }
  successful_draws <- draws[, valid_draws, drop = FALSE]
  alpha <- (1 - conf.level) / 2
  mean_ci <- stats::quantile(successful_draws["mean", ], c(alpha, 1 - alpha))
  chi_ci <- stats::quantile(successful_draws["chi_square", ], c(alpha, 1 - alpha))

  observed_df <- nrow(observed) - 1L
  observed_p <- stats::pchisq(observed_chi, df = observed_df, lower.tail = FALSE)
  heterogeneous <- isTRUE(observed_p < 1 - conf.level)
  pairs <- diag_pair_summary(observed, conf.level = conf.level)

  print_diag_pairs(pairs, conf.level = conf.level)
  cat("\n")
  cat("Mean diagnosticity ratio is", round(observed_mean, 3), "\n")
  cat("Confidence intervals (percentile)", round(mean_ci, 3), "\n")
  cat("Chi-squared estimate is", round(observed_chi, 3), "\n")
  cat("Confidence intervals (percentile):", round(chi_ci, 3), "\n")
  cat(paste0("Sig (asymptotic, df = ", observed_df, "): "),
      signif(observed_p, 4), "\n")
  print_pooling_note(heterogeneous, conf.level = conf.level)
  cat("\n")

  invisible(list(
    mean_diagnosticity = observed_mean,
    mean_ci = unname(mean_ci),
    chi_square = observed_chi,
    chi_square_ci = unname(chi_ci),
    df = observed_df,
    p_value = observed_p,
    heterogeneous = heterogeneous,
    parameters = pairs,
    draws = t(draws),
    R = as.integer(R),
    n_successful = n_successful,
    n_failed = as.integer(R) - n_successful
  ))
}
