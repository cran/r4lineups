#' Bayesian Functional Size (Beta-Binomial Model)
#'
#' Computes a Bayesian posterior distribution for the functional size of a lineup
#' using a Beta-Binomial conjugate model for the suspect-selection rate.
#'
#' @param lineup_vec A numeric vector of lineup choices.
#' @param target_pos A scalar; the position of the suspect in the lineup.
#' @param alpha Dirichlet concentration (prior strength). A positive scalar.
#'   Named shortcuts: \code{"jeffreys"} (0.5, default), \code{"uniform"} (1),
#'   \code{"weak"} (0.1).
#' @param S Number of posterior draws. Default 10000.
#' @param credible_mass Width of the equal-tailed credible interval. Default 0.95.
#' @param threshold Optional numeric. If supplied, reports \eqn{P(F < t)} and
#'   \eqn{P(F > t \mid \mathbf{n})}.
#'
#' @return An object of class \code{"func_size_bayes"} containing:
#' \describe{
#'   \item{F_draws}{Numeric vector of length \code{S}: posterior draws of functional size.}
#'   \item{posterior_mean, posterior_median}{Posterior point estimates. The
#'     mean is exact, and is \code{Inf} when it does not exist (see Details).}
#'   \item{credible_interval}{Named two-element vector (lower, upper).}
#'   \item{prior_alpha, n, n_suspect, S, credible_mass}{Input metadata.}
#'   \item{threshold, threshold_probs}{Threshold and probabilities (or NULL).}
#' }
#'
#' @details
#' Functional size \eqn{F = N / n_s} is the reciprocal of the suspect-selection rate
#' \eqn{p = n_s / N}. The Beta-Binomial conjugate model gives:
#' \deqn{p \mid \mathbf{n} \sim \mathrm{Beta}(n_s + \alpha,\; N - n_s + \alpha)}
#' Posterior draws of \eqn{p^{(s)}} are transformed via \eqn{F^{(s)} = 1/p^{(s)}}.
#' The default Jeffreys prior (\eqn{\alpha = 0.5}) is recommended. The posterior
#' gives direct probability statements such as \eqn{P(F < k \mid \mathbf{n})},
#' where \eqn{k} is the nominal lineup size.
#'
#' The posterior mean of a reciprocal is computed exactly, not from the draws:
#' with \eqn{a = n_s + \alpha} and \eqn{b = N - n_s + \alpha},
#' \eqn{E[F \mid \mathbf{n}] = (a + b - 1)/(a - 1)} when \eqn{a > 1}. When
#' \eqn{a \le 1} (for example, no suspect choices under the Jeffreys prior) the
#' mean is infinite, and the Monte Carlo average of the draws would not settle
#' on any value as \code{S} grew. Report the median and interval instead.
#'
#' @references
#' Tredoux, C. G. (1998). Statistical inference on measures of lineup fairness.
#' \emph{Law and Human Behavior, 22}(2), 217-237.
#'
#' @seealso \code{\link{func_size}}, \code{\link{esize_T_bayes}}
#'
#' @examples
#' set.seed(1)
#' lineup_vec <- round(runif(100, 1, 6))
#' res <- func_size_bayes(lineup_vec, target_pos = 3)
#' print(res)
#' plot(res)
#'
#' # Threshold: P(F < 6 | data)
#' res_t <- func_size_bayes(lineup_vec, target_pos = 3, threshold = 6)
#' print(res_t)
#'
#' @importFrom stats rbeta quantile
#' @export
func_size_bayes <- function(lineup_vec, target_pos,
                             alpha = 0.5, S = 10000,
                             credible_mass = 0.95, threshold = NULL) {
  lineup_vec <- typecheck(lineup_vec)
  if (!is.numeric(target_pos) || length(target_pos) != 1)
    stop("target_pos must be a single numeric value.", call. = FALSE)
  alpha_val <- .resolve_alpha_scalar(alpha)
  if (!is.numeric(S) || S < 1)
    stop("S must be a positive integer.", call. = FALSE)
  if (!is.numeric(credible_mass) || credible_mass <= 0 || credible_mass >= 1)
    stop("credible_mass must be in (0, 1).", call. = FALSE)
  if (!is.null(threshold) && (!is.numeric(threshold) || length(threshold) != 1))
    stop("threshold must be a single numeric value.", call. = FALSE)

  N     <- length(lineup_vec)
  n_s   <- sum(lineup_vec == target_pos)
  if (n_s == 0)
    warning("No mock witnesses chose the target position. Posterior will be highly right-skewed.",
            call. = FALSE)

  p_draws <- rbeta(S, n_s + alpha_val, N - n_s + alpha_val)
  F_draws <- 1 / p_draws

  tail_prob <- (1 - credible_mass) / 2
  ci <- quantile(F_draws, probs = c(tail_prob, 1 - tail_prob))
  names(ci) <- c("lower", "upper")

  threshold_probs <- NULL
  if (!is.null(threshold)) {
    threshold_probs <- list(
      P_below = mean(F_draws < threshold),
      P_above = mean(F_draws > threshold)
    )
  }

  structure(
    list(
      F_draws          = F_draws,
      posterior_mean   = .beta_reciprocal_mean(n_s + alpha_val, N - n_s + alpha_val),
      posterior_median = median(F_draws),
      credible_interval = ci,
      prior_alpha      = alpha_val,
      n                = N,
      n_suspect        = n_s,
      S                = S,
      credible_mass    = credible_mass,
      threshold        = threshold,
      threshold_probs  = threshold_probs
    ),
    class = "func_size_bayes"
  )
}

#' @export
print.func_size_bayes <- function(x, digits = 3, ...) {
  pct <- round(x$credible_mass * 100)
  cat("Bayesian Functional Size - Beta-Binomial model\n")
  cat(sprintf("  Prior: Jeffreys-type Beta(%.2f, %.2f)\n", x$prior_alpha, x$prior_alpha))
  cat(sprintf("  n = %d, suspect IDs = %d (rate = %.3f)\n",
              x$n, x$n_suspect, x$n_suspect / x$n))
  cat(.format_posterior_mean("  Posterior mean F:   ", x$posterior_mean))
  cat(sprintf("  Posterior median F: %.3f\n", x$posterior_median))
  cat(sprintf("  %d%% credible interval: [%.3f, %.3f]\n",
              pct, x$credible_interval["lower"], x$credible_interval["upper"]))
  if (!is.null(x$threshold_probs)) {
    cat(sprintf("  P(F < %.3f | data): %.4f\n", x$threshold, x$threshold_probs$P_below))
    cat(sprintf("  P(F > %.3f | data): %.4f\n", x$threshold, x$threshold_probs$P_above))
  }
  invisible(x)
}

#' @export
plot.func_size_bayes <- function(x, ...) {
  pct <- round(x$credible_mass * 100)
  F_v <- x$F_draws
  ggplot2::ggplot(data.frame(F_size = F_v), ggplot2::aes(x = F_size)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            bins = 40, fill = "#72b7b2", alpha = 0.6) +
    ggplot2::geom_density(colour = "#1f2d3d", linewidth = 1) +
    ggplot2::geom_vline(xintercept = x$credible_interval["lower"],
                        linetype = "dashed", colour = "#e15759") +
    ggplot2::geom_vline(xintercept = x$credible_interval["upper"],
                        linetype = "dashed", colour = "#e15759") +
    ggplot2::geom_vline(xintercept = x$posterior_median,
                        linetype = "solid", colour = "#1f2d3d") +
    ggplot2::labs(
      x     = "Functional size (F)",
      y     = "Density",
      title = "Posterior distribution of functional size",
      subtitle = sprintf("Median = %.3f; %d%% CI [%.3f, %.3f]",
                         x$posterior_median, pct,
                         x$credible_interval["lower"], x$credible_interval["upper"])
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

utils::globalVariables("F_size")

# E[1/p] for p ~ Beta(a, b): finite only when a > 1
.beta_reciprocal_mean <- function(a, b) {
  if (a > 1) (a + b - 1) / (a - 1) else Inf
}

# Difference of two posterior means; undefined when both are infinite
.difference_of_means <- function(m_a, m_b) {
  if (is.infinite(m_a) && is.infinite(m_b)) NA_real_ else m_a - m_b
}

.format_posterior_mean <- function(label, value) {
  if (is.finite(value)) {
    sprintf("%s%.3f\n", label, value)
  } else {
    sprintf("%sdoes not exist (%s); report the median and interval\n", label,
            if (is.na(value)) "both means are infinite" else "infinite")
  }
}
