#' Bayesian Diagnosticity Ratio (Tredoux, Beta-Binomial Model)
#'
#' Computes a Bayesian posterior distribution for the Tredoux diagnosticity
#' ratio using independent Beta-Binomial models for the target-present and
#' target-absent suspect-selection rates.
#'
#' @param lineup_pres A numeric vector of lineup choices for a target-present lineup.
#' @param lineup_abs A numeric vector of lineup choices for a target-absent lineup.
#' @param pos_pres Scalar; suspect position in the target-present lineup.
#' @param pos_abs Scalar; suspect position in the target-absent lineup.
#' @param k1 Number of members in the target-present lineup (for input validation).
#' @param k2 Number of members in the target-absent lineup (for input validation).
#' @param alpha Dirichlet concentration (prior strength). A scalar \eqn{\alpha > 0}
#'   applied to both cells. Named shortcuts: \code{"jeffreys"} (0.5, default),
#'   \code{"uniform"} (1), \code{"weak"} (0.1).
#' @param S Number of posterior draws. Default 10000.
#' @param credible_mass Width of the equal-tailed credible interval. Default 0.95.
#' @param threshold Optional numeric. If supplied, reports \eqn{P(DR < t)} and
#'   \eqn{P(DR > t | \mathbf{n})}.
#'
#' @return An object of class \code{"diag_ratio_T_bayes"} containing:
#' \describe{
#'   \item{DR_draws}{Numeric vector of length \code{S}: posterior draws of DR.}
#'   \item{lnDR_draws}{Log-transformed posterior draws.}
#'   \item{posterior_mean, posterior_median}{Posterior point estimates. The
#'     mean is exact, and is \code{Inf} when it does not exist (see Details).}
#'   \item{credible_interval}{Named two-element vector (lower, upper).}
#'   \item{prior_alpha}{Alpha value used.}
#'   \item{n_tp, n_ta, S, credible_mass}{Input metadata.}
#'   \item{threshold, threshold_probs}{Threshold and associated probabilities (or NULL).}
#' }
#'
#' @details
#' The target-present suspect-selection rate \eqn{p_{TP}} and the target-absent rate
#' \eqn{p_{TA}} are modelled independently with Beta posteriors:
#' \deqn{p_{TP} \mid \mathbf{n} \sim \mathrm{Beta}(n_{TP,s} + \alpha,\; N_{TP} - n_{TP,s} + \alpha)}
#' \deqn{p_{TA} \mid \mathbf{n} \sim \mathrm{Beta}(n_{TA,s} + \alpha,\; N_{TA} - n_{TA,s} + \alpha)}
#' The posterior of the diagnosticity ratio is obtained by drawing
#' \eqn{DR^{(s)} = p_{TP}^{(s)} / p_{TA}^{(s)}} for each sample \eqn{s}.
#' The default Jeffreys prior (\eqn{\alpha = 0.5}) is recommended for small samples.
#' Unlike the continuity-corrected point estimate, the posterior handles zero cell
#' counts naturally and provides direct probability statements such as
#' \eqn{P(DR > 1 \mid \mathbf{n})}.
#'
#' The posterior mean is computed exactly, not from the draws. Because the two
#' rates are independent, \eqn{E[DR] = E[p_{TP}] \, E[1/p_{TA}]}, with
#' \eqn{E[1/p_{TA}] = (a + b - 1)/(a - 1)} for \eqn{p_{TA} \sim
#' \mathrm{Beta}(a, b)} when \eqn{a > 1}. When \eqn{a \le 1} (for example,
#' no target-absent suspect choices under the Jeffreys prior) the mean is
#' infinite, and an average of the draws would not settle on any value as
#' \code{S} grew. Report the median and interval instead. In
#' \code{diag_ratio_T_bayes_compare()}, the mean difference is \code{NA} when
#' both means are infinite.
#'
#' @references
#' Tredoux, C. G. (1998). Statistical inference on measures of lineup fairness.
#' \emph{Law and Human Behavior, 22}(2), 217-237.
#'
#' Wells, G. L., & Turtle, J. W. (1986). Eyewitness identification: The importance
#' of lineup models. \emph{Psychological Bulletin, 99}(3), 320-329.
#'
#' @seealso \code{\link{diag_ratio_T}}, \code{\link{diag_ratio_T_bayes_compare}},
#'   \code{\link{esize_T_bayes}}
#'
#' @examples
#' set.seed(1)
#' lineup_pres <- round(runif(100, 1, 6))
#' lineup_abs  <- round(runif(80, 1, 6))
#' res <- diag_ratio_T_bayes(lineup_pres, lineup_abs, pos_pres = 3, pos_abs = 3,
#'                            k1 = 6, k2 = 6)
#' print(res)
#' plot(res)
#'
#' @importFrom stats rbeta qnorm quantile
#' @export
diag_ratio_T_bayes <- function(lineup_pres, lineup_abs, pos_pres, pos_abs,
                                k1, k2,
                                alpha = 0.5, S = 10000,
                                credible_mass = 0.95, threshold = NULL) {
  lineup_pres <- typecheck(lineup_pres)
  lineup_abs  <- typecheck(lineup_abs)
  datacheck2(lineup_pres, lineup_abs, k1, k2)

  alpha_val <- .resolve_alpha_scalar(alpha)
  if (!is.numeric(S) || S < 1)
    stop("S must be a positive integer.", call. = FALSE)
  if (!is.numeric(credible_mass) || credible_mass <= 0 || credible_mass >= 1)
    stop("credible_mass must be in (0, 1).", call. = FALSE)
  if (!is.null(threshold) && (!is.numeric(threshold) || length(threshold) != 1))
    stop("threshold must be a single numeric value.", call. = FALSE)

  n_tp_s <- sum(lineup_pres == pos_pres)
  N_tp   <- length(lineup_pres)
  n_ta_s <- sum(lineup_abs  == pos_abs)
  N_ta   <- length(lineup_abs)

  p_tp_draws <- rbeta(S, n_tp_s + alpha_val, N_tp - n_tp_s + alpha_val)
  p_ta_draws <- rbeta(S, n_ta_s + alpha_val, N_ta - n_ta_s + alpha_val)
  DR_draws   <- p_tp_draws / p_ta_draws
  lnDR_draws <- log(DR_draws)

  tail_prob <- (1 - credible_mass) / 2
  ci <- quantile(DR_draws, probs = c(tail_prob, 1 - tail_prob))
  names(ci) <- c("lower", "upper")

  threshold_probs <- NULL
  if (!is.null(threshold)) {
    threshold_probs <- list(
      P_below = mean(DR_draws < threshold),
      P_above = mean(DR_draws > threshold)
    )
  }

  structure(
    list(
      DR_draws        = DR_draws,
      lnDR_draws      = lnDR_draws,
      p_tp_draws      = p_tp_draws,
      p_ta_draws      = p_ta_draws,
      posterior_mean   = (n_tp_s + alpha_val) / (N_tp + 2 * alpha_val) *
        .beta_reciprocal_mean(n_ta_s + alpha_val, N_ta - n_ta_s + alpha_val),
      posterior_median = median(DR_draws),
      credible_interval = ci,
      prior_alpha      = alpha_val,
      n_tp             = N_tp,
      n_ta             = N_ta,
      n_tp_suspect     = n_tp_s,
      n_ta_suspect     = n_ta_s,
      S                = S,
      credible_mass    = credible_mass,
      threshold        = threshold,
      threshold_probs  = threshold_probs
    ),
    class = "diag_ratio_T_bayes"
  )
}

#' @export
print.diag_ratio_T_bayes <- function(x, digits = 3, ...) {
  pct <- round(x$credible_mass * 100)
  cat("Bayesian Diagnosticity Ratio (Tredoux) - Beta-Binomial model\n")
  cat(sprintf("  Prior: Jeffreys-type Beta(%.2f, %.2f)\n", x$prior_alpha, x$prior_alpha))
  cat(sprintf("  TP lineup: n = %d, suspect IDs = %d\n", x$n_tp, x$n_tp_suspect))
  cat(sprintf("  TA lineup: n = %d, suspect IDs = %d\n", x$n_ta, x$n_ta_suspect))
  cat(.format_posterior_mean("  Posterior mean DR:   ", x$posterior_mean))
  cat(sprintf("  Posterior median DR: %.3f\n", x$posterior_median))
  cat(sprintf("  %d%% credible interval: [%.3f, %.3f]\n",
              pct, x$credible_interval["lower"], x$credible_interval["upper"]))
  if (!is.null(x$threshold_probs)) {
    cat(sprintf("  P(DR < %.3f | data): %.4f\n", x$threshold, x$threshold_probs$P_below))
    cat(sprintf("  P(DR > %.3f | data): %.4f\n", x$threshold, x$threshold_probs$P_above))
  }
  invisible(x)
}

#' @importFrom graphics hist lines abline legend
#' @export
plot.diag_ratio_T_bayes <- function(x, ...) {
  pct   <- round(x$credible_mass * 100)
  DR    <- x$DR_draws
  ggplot2::ggplot(data.frame(DR = DR), ggplot2::aes(x = DR)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            bins = 40, fill = "#72b7b2", alpha = 0.6) +
    ggplot2::geom_density(colour = "#1f2d3d", linewidth = 1) +
    ggplot2::geom_vline(xintercept = x$credible_interval["lower"],
                        linetype = "dashed", colour = "#e15759") +
    ggplot2::geom_vline(xintercept = x$credible_interval["upper"],
                        linetype = "dashed", colour = "#e15759") +
    ggplot2::geom_vline(xintercept = x$posterior_median,
                        linetype = "solid", colour = "#1f2d3d") +
    ggplot2::geom_vline(xintercept = 1, linetype = "dotted", colour = "grey50") +
    ggplot2::labs(
      x     = "Diagnosticity ratio (DR)",
      y     = "Density",
      title = "Posterior distribution of diagnosticity ratio",
      subtitle = sprintf("Median = %.3f; %d%% CI [%.3f, %.3f]",
                         x$posterior_median, pct,
                         x$credible_interval["lower"], x$credible_interval["upper"])
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

#' Bayesian Comparison of Two Diagnosticity Ratios
#'
#' Fits independent Beta-Binomial models to two lineup pairs and returns the
#' posterior distribution of the difference \eqn{\Delta = DR_A - DR_B}, together
#' with \eqn{P(DR_A > DR_B \mid \mathbf{n})}.
#'
#' @param pres_A,abs_A Numeric vectors of lineup choices for lineup pair A
#'   (target-present and target-absent, respectively).
#' @param pos_pres_A,pos_abs_A Suspect positions for pair A.
#' @param k1_A,k2_A Lineup sizes for pair A (for validation).
#' @param pres_B,abs_B Numeric vectors of lineup choices for lineup pair B.
#' @param pos_pres_B,pos_abs_B Suspect positions for pair B.
#' @param k1_B,k2_B Lineup sizes for pair B.
#' @param alpha Prior concentration (shared). Default 0.5 (Jeffreys).
#' @param S Number of posterior draws. Default 10000.
#' @param credible_mass Width of credible interval. Default 0.95.
#'
#' @return An object of class \code{"diag_ratio_T_bayes_compare"}.
#'
#' @seealso \code{\link{diag_ratio_T_bayes}}
#' @examples
#' set.seed(1)
#' pres_A <- round(runif(100, 1, 6)); abs_A <- round(runif(80, 1, 6))
#' pres_B <- round(runif(100, 1, 6)); abs_B <- round(runif(80, 1, 6))
#' res <- diag_ratio_T_bayes_compare(pres_A, abs_A, 3, 3, 6, 6,
#'                                    pres_B, abs_B, 3, 3, 6, 6)
#' print(res)
#' @export
diag_ratio_T_bayes_compare <- function(pres_A, abs_A, pos_pres_A, pos_abs_A, k1_A, k2_A,
                                        pres_B, abs_B, pos_pres_B, pos_abs_B, k1_B, k2_B,
                                        alpha = 0.5, S = 10000, credible_mass = 0.95) {
  res_A <- diag_ratio_T_bayes(pres_A, abs_A, pos_pres_A, pos_abs_A, k1_A, k2_A,
                               alpha = alpha, S = S, credible_mass = credible_mass)
  res_B <- diag_ratio_T_bayes(pres_B, abs_B, pos_pres_B, pos_abs_B, k1_B, k2_B,
                               alpha = alpha, S = S, credible_mass = credible_mass)
  delta <- res_A$DR_draws - res_B$DR_draws

  tail_prob <- (1 - credible_mass) / 2
  ci_delta  <- quantile(delta, probs = c(tail_prob, 1 - tail_prob))
  names(ci_delta) <- c("lower", "upper")

  structure(
    list(
      delta            = delta,
      posterior_mean   = .difference_of_means(res_A$posterior_mean,
                                              res_B$posterior_mean),
      posterior_median = median(delta),
      credible_interval = ci_delta,
      P_A_greater      = mean(delta > 0),
      P_B_greater      = mean(delta < 0),
      res_A            = res_A,
      res_B            = res_B,
      S                = S,
      credible_mass    = credible_mass
    ),
    class = "diag_ratio_T_bayes_compare"
  )
}

#' @export
print.diag_ratio_T_bayes_compare <- function(x, digits = 3, ...) {
  pct <- round(x$credible_mass * 100)
  cat("Bayesian comparison of diagnosticity ratios (DR_A - DR_B)\n")
  cat(.format_posterior_mean("  DR_A posterior mean: ", x$res_A$posterior_mean))
  cat(.format_posterior_mean("  DR_B posterior mean: ", x$res_B$posterior_mean))
  cat(.format_posterior_mean("  Delta posterior mean:   ", x$posterior_mean))
  cat(sprintf("  Delta posterior median: %.3f\n", x$posterior_median))
  cat(sprintf("  %d%% CI on delta: [%.3f, %.3f]\n",
              pct, x$credible_interval["lower"], x$credible_interval["upper"]))
  cat(sprintf("  P(DR_A > DR_B | data): %.4f\n", x$P_A_greater))
  cat(sprintf("  P(DR_B > DR_A | data): %.4f\n", x$P_B_greater))
  invisible(x)
}

#' @export
plot.diag_ratio_T_bayes_compare <- function(x, ...) {
  pct   <- round(x$credible_mass * 100)
  delta <- x$delta
  ggplot2::ggplot(data.frame(delta = delta), ggplot2::aes(x = delta)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            bins = 40, fill = "#72b7b2", alpha = 0.6) +
    ggplot2::geom_density(colour = "#1f2d3d", linewidth = 1) +
    ggplot2::geom_vline(xintercept = x$credible_interval["lower"],
                        linetype = "dashed", colour = "#e15759") +
    ggplot2::geom_vline(xintercept = x$credible_interval["upper"],
                        linetype = "dashed", colour = "#e15759") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted", colour = "grey50") +
    ggplot2::labs(
      x     = expression(paste(Delta, " = DR"[A], " - DR"[B])),
      y     = "Density",
      title = "Posterior distribution of DR difference",
      subtitle = sprintf("P(DR_A > DR_B) = %.3f; %d%% CI [%.3f, %.3f]",
                         x$P_A_greater, pct,
                         x$credible_interval["lower"], x$credible_interval["upper"])
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

.resolve_alpha_scalar <- function(alpha) {
  if (is.character(alpha)) {
    alpha <- switch(alpha,
      jeffreys = 0.5,
      uniform  = 1.0,
      weak     = 0.1,
      stop("Unknown alpha name. Use 'jeffreys', 'uniform', or 'weak'.", call. = FALSE)
    )
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0)
    stop("alpha must be a positive scalar or one of 'jeffreys', 'uniform', 'weak'.",
         call. = FALSE)
  alpha
}

utils::globalVariables(c("DR", "delta"))
