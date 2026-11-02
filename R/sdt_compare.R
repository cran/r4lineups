#' Bayesian Comparison of SDT Sensitivity (d') Between Two Conditions
#'
#' Computes a Bayesian posterior distribution for the difference
#' \eqn{\Delta d' = d'_A - d'_B} using independent Jeffreys Beta posteriors
#' for the hit and false-alarm rates in each condition.
#'
#' @param hits_A,misses_A,fas_A,crs_A Integer counts for condition A.
#' @param hits_B,misses_B,fas_B,crs_B Integer counts for condition B.
#' @param label_A,label_B Character labels for conditions (default "A", "B").
#' @param alpha Prior concentration for the Beta prior on each rate.
#'   Default 0.5 (Jeffreys). Named shortcuts: \code{"jeffreys"}, \code{"uniform"},
#'   \code{"weak"}.
#' @param S Number of posterior draws. Default 10000.
#' @param credible_mass Width of the equal-tailed credible interval. Default 0.95.
#'
#' @return An object of class \code{"sdt_compare"} containing:
#' \describe{
#'   \item{delta_dprime_draws}{Posterior draws of \eqn{\Delta d'}.}
#'   \item{dprime_A_draws, dprime_B_draws}{Per-condition d' draws.}
#'   \item{posterior_mean, posterior_median}{Posterior summaries of \eqn{\Delta d'}.}
#'   \item{credible_interval}{Named lower/upper CI on \eqn{\Delta d'}.}
#'   \item{P_A_greater}{Posterior probability that \eqn{d'_A > d'_B}.}
#'   \item{P_B_greater}{Posterior probability that \eqn{d'_B > d'_A}.}
#'   \item{dprime_A_mean, dprime_B_mean}{Posterior means of each d'.}
#'   \item{frequentist}{Output of \code{compare_sdt_summary()} for comparison.}
#'   \item{prior_alpha, S, credible_mass}{Metadata.}
#' }
#'
#' @details
#' Independent Jeffreys Beta posteriors are placed on the hit and false-alarm
#' rates in each condition:
#' \deqn{HR_A \mid \mathbf{n} \sim \mathrm{Beta}(\text{hits}_A + \alpha,\; \text{misses}_A + \alpha)}
#' \deqn{FAR_A \mid \mathbf{n} \sim \mathrm{Beta}(\text{fas}_A + \alpha,\; \text{crs}_A + \alpha)}
#' The posterior d' draws are:
#' \deqn{d'^{(s)}_A = \Phi^{-1}(HR_A^{(s)}) - \Phi^{-1}(FAR_A^{(s)})}
#' and analogously for B. The posterior of the difference is
#' \eqn{\Delta d'^{(s)} = d'^{(s)}_A - d'^{(s)}_B}, from which
#' \eqn{P(d'_A > d'_B \mid \mathbf{n})} is directly read off.
#'
#' The Jeffreys prior (\eqn{\alpha = 0.5}) handles zero cell counts gracefully
#' and provides direct probability statements - unlike the asymptotic z-test in
#' \code{\link{compare_sdt_summary}}.
#'
#' @references
#' Macmillan, N. A., & Creelman, C. D. (2005). \emph{Detection Theory: A User's
#' Guide} (2nd ed.). Lawrence Erlbaum.
#'
#' Wixted, J. T., & Mickes, L. (2014). A signal-detection-based diagnostic-feature
#' detection model of eyewitness identification. \emph{Psychological Review, 121}(2),
#' 262-276.
#'
#' @seealso \code{\link{compare_sdt_summary}}, \code{\link{sdt_summary_from_counts}},
#'   \code{\link{diag_ratio_T_bayes}}
#'
#' @examples
#' # Simultaneous vs. sequential lineup comparison
#' res <- sdt_compare(
#'   hits_A = 45, misses_A = 55, fas_A = 12, crs_A = 88,
#'   hits_B = 38, misses_B = 62, fas_B = 10, crs_B = 90,
#'   label_A = "Simultaneous", label_B = "Sequential"
#' )
#' print(res)
#' plot(res)
#'
#' @importFrom stats rbeta qnorm quantile median
#' @export
sdt_compare <- function(hits_A, misses_A, fas_A, crs_A,
                         hits_B, misses_B, fas_B, crs_B,
                         label_A = "A", label_B = "B",
                         alpha = 0.5, S = 10000, credible_mass = 0.95) {
  alpha_val <- .resolve_alpha_scalar(alpha)
  if (!is.numeric(S) || S < 1)
    stop("S must be a positive integer.", call. = FALSE)
  if (!is.numeric(credible_mass) || credible_mass <= 0 || credible_mass >= 1)
    stop("credible_mass must be in (0, 1).", call. = FALSE)

  for (nm in c("hits_A", "misses_A", "fas_A", "crs_A",
               "hits_B", "misses_B", "fas_B", "crs_B")) {
    val <- get(nm)
    if (!is.numeric(val) || length(val) != 1 || val < 0)
      stop(sprintf("%s must be a non-negative numeric scalar.", nm), call. = FALSE)
  }

  # Posterior draws for condition A
  HR_A  <- rbeta(S, hits_A + alpha_val, misses_A + alpha_val)
  FAR_A <- rbeta(S, fas_A  + alpha_val, crs_A   + alpha_val)
  # Clamp to avoid +/-Inf from qnorm
  eps   <- .Machine$double.eps
  HR_A  <- pmin(pmax(HR_A,  eps), 1 - eps)
  FAR_A <- pmin(pmax(FAR_A, eps), 1 - eps)
  dprime_A_draws <- qnorm(HR_A) - qnorm(FAR_A)

  # Posterior draws for condition B
  HR_B  <- rbeta(S, hits_B + alpha_val, misses_B + alpha_val)
  FAR_B <- rbeta(S, fas_B  + alpha_val, crs_B   + alpha_val)
  HR_B  <- pmin(pmax(HR_B,  eps), 1 - eps)
  FAR_B <- pmin(pmax(FAR_B, eps), 1 - eps)
  dprime_B_draws <- qnorm(HR_B) - qnorm(FAR_B)

  delta <- dprime_A_draws - dprime_B_draws

  tail_p <- (1 - credible_mass) / 2
  ci     <- quantile(delta, probs = c(tail_p, 1 - tail_p))
  names(ci) <- c("lower", "upper")

  freq <- tryCatch(
    compare_sdt_summary(hits_A, fas_A, misses_A, crs_A,
                        hits_B, fas_B, misses_B, crs_B,
                        metric = "dprime"),
    error = function(e) NULL
  )

  structure(
    list(
      delta_dprime_draws = delta,
      dprime_A_draws     = dprime_A_draws,
      dprime_B_draws     = dprime_B_draws,
      posterior_mean     = mean(delta),
      posterior_median   = median(delta),
      credible_interval  = ci,
      P_A_greater        = mean(delta > 0),
      P_B_greater        = mean(delta < 0),
      dprime_A_mean      = mean(dprime_A_draws),
      dprime_B_mean      = mean(dprime_B_draws),
      frequentist        = freq,
      label_A            = label_A,
      label_B            = label_B,
      prior_alpha        = alpha_val,
      S                  = S,
      credible_mass      = credible_mass
    ),
    class = "sdt_compare"
  )
}

#' @export
print.sdt_compare <- function(x, digits = 3, ...) {
  pct <- round(x$credible_mass * 100)
  cat(sprintf("Bayesian SDT Comparison: %s vs %s\n", x$label_A, x$label_B))
  cat(sprintf("  Prior: Jeffreys Beta(%.2f, %.2f) on HR and FAR\n",
              x$prior_alpha, x$prior_alpha))
  cat(sprintf("  Posterior mean d': %s = %.3f, %s = %.3f\n",
              x$label_A, x$dprime_A_mean, x$label_B, x$dprime_B_mean))
  cat(sprintf("  Delta d' (%s - %s):\n", x$label_A, x$label_B))
  cat(sprintf("    Posterior mean:   %.3f\n", x$posterior_mean))
  cat(sprintf("    Posterior median: %.3f\n", x$posterior_median))
  cat(sprintf("    %d%% CI: [%.3f, %.3f]\n", pct,
              x$credible_interval["lower"], x$credible_interval["upper"]))
  cat(sprintf("  P(d'_%s > d'_%s | data): %.4f\n",
              x$label_A, x$label_B, x$P_A_greater))
  cat(sprintf("  P(d'_%s > d'_%s | data): %.4f\n",
              x$label_B, x$label_A, x$P_B_greater))
  if (!is.null(x$frequentist)) {
    cat(sprintf("  (Frequentist z-test: z = %.3f, p = %.4f)\n",
                x$frequentist$z, x$frequentist$p_value))
  }
  invisible(x)
}

#' @export
plot.sdt_compare <- function(x, ...) {
  pct   <- round(x$credible_mass * 100)
  delta <- x$delta_dprime_draws
  lbl   <- sprintf("Delta d' (%s - %s)", x$label_A, x$label_B)
  ggplot2::ggplot(data.frame(delta_d = delta), ggplot2::aes(x = delta_d)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            bins = 40, fill = "#72b7b2", alpha = 0.6) +
    ggplot2::geom_density(colour = "#1f2d3d", linewidth = 1) +
    ggplot2::geom_vline(xintercept = x$credible_interval["lower"],
                        linetype = "dashed", colour = "#e15759") +
    ggplot2::geom_vline(xintercept = x$credible_interval["upper"],
                        linetype = "dashed", colour = "#e15759") +
    ggplot2::geom_vline(xintercept = x$posterior_median,
                        linetype = "solid", colour = "#1f2d3d") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted", colour = "grey50") +
    ggplot2::labs(
      x     = lbl, y = "Density",
      title = sprintf("Posterior distribution of %s", lbl),
      subtitle = sprintf("P(%s > %s) = %.3f; %d%% CI [%.3f, %.3f]",
                         x$label_A, x$label_B, x$P_A_greater, pct,
                         x$credible_interval["lower"], x$credible_interval["upper"])
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

utils::globalVariables("delta_d")
