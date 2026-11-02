#' Bayesian Calibration Analysis (Beta-Binomial Model)
#'
#' Computes Bayesian posterior distributions for the calibration statistic C,
#' over/underconfidence (O/U), and the Normalized Resolution Index (NRI) by
#' placing independent Beta priors on the accuracy rate in each confidence bin.
#'
#' @param data A dataframe with columns \code{target_present} (logical),
#'   \code{identification} (character: "suspect", "filler", "reject"), and
#'   \code{confidence} (numeric).
#' @param confidence_bins Numeric vector of bin edges for confidence (e.g.,
#'   \code{c(0, 60, 80, 100)}). If \code{NULL} (default), treats each unique
#'   confidence level as its own bin.
#' @param choosers_only Logical. If \code{TRUE} (default), only suspect
#'   identifications are included (standard eyewitness calibration practice).
#' @param lineup_size Integer. Number of lineup members (default 6).
#' @param confidence_scale How the confidence scale is interpreted: "auto" (default),
#'   "0-1", or "0-100". See \code{\link{make_calibration_data}}.
#' @param alpha Prior concentration for the Beta prior on per-bin accuracy.
#'   Default 0.5 (Jeffreys). Named shortcuts: \code{"jeffreys"}, \code{"uniform"},
#'   \code{"weak"}.
#' @param S Number of posterior draws per bin. Default 10000.
#' @param credible_mass Width of equal-tailed credible intervals. Default 0.95.
#'
#' @return An object of class \code{"calibration_bayes"} containing:
#' \describe{
#'   \item{C_draws, OU_draws, NRI_draws}{Posterior draws of each statistic.}
#'   \item{C_mean, C_median, C_ci}{Posterior mean, median, and credible interval for C.}
#'   \item{OU_mean, OU_median, OU_ci}{Same for O/U.}
#'   \item{NRI_mean, NRI_median, NRI_ci}{Same for NRI.}
#'   \item{point_estimates}{Frequentist C, OU, NRI from \code{make_calibration_data()}.}
#'   \item{bin_data}{Per-bin summary (n, n_correct, mean_confidence, accuracy).}
#'   \item{prior_alpha, S, credible_mass, n_total}{Metadata.}
#' }
#'
#' @details
#' For each confidence bin \eqn{j} with \eqn{n_j} responses and \eqn{k_j} correct
#' identifications, the posterior accuracy is:
#' \deqn{a_j \mid \mathbf{n} \sim \mathrm{Beta}(k_j + \alpha,\; n_j - k_j + \alpha).}
#' For each posterior draw \eqn{s}, the calibration statistics are computed:
#' \deqn{C^{(s)} = \sum_j \frac{n_j}{N}(c_j - a_j^{(s)})^2}
#' \deqn{OU^{(s)} = \bar{c} - \sum_j \frac{n_j}{N} a_j^{(s)}}
#' \deqn{NRI^{(s)} = \frac{\sum_j \frac{n_j}{N}(a_j^{(s)} - \bar{a}^{(s)})^2}{\bar{a}^{(s)}(1-\bar{a}^{(s)})}}
#' where \eqn{c_j} is the mean confidence (proportion scale) in bin \eqn{j} and
#' \eqn{\bar{c}} is the overall mean confidence.
#' This model assumes exchangeable Bernoulli correctness within each bin.
#' In experiments with fixed TP and TA totals, conditioning on the number of
#' identifications in a bin does not generally give a binomial correct count.
#' These intervals are therefore conditional working-model summaries, rather
#' than automatically exact posterior inference for that fixed-arm design.
#' If innocent IDs are estimated from fillers, the fractional effective counts
#' produce a working Beta posterior, not the exact likelihood for observed
#' designated-suspect counts. Its intervals do not propagate all uncertainty
#' in that estimation. Bin frequencies and mean confidence are conditioned on.
#'
#' @references
#' Juslin, P., Olsson, N., & Winman, A. (1996). Calibration and diagnosticity of
#' confidence in eyewitness identification. \emph{Journal of Experimental Psychology:
#' Learning, Memory, and Cognition, 22}(5), 1304-1316.
#'
#' Brewer, N., & Wells, G. L. (2006). The confidence-accuracy relationship in
#' eyewitness identification. \emph{Journal of Experimental Psychology: Applied,
#' 12}(1), 11-30.
#'
#' @seealso \code{\link{make_calibration_data}}, \code{\link{esize_T_bayes}}
#'
#' @examples
#' set.seed(42)
#' n <- 200
#' conf  <- sample(c(50, 70, 90), n, replace = TRUE)
#' tp    <- sample(c(TRUE, FALSE), n, replace = TRUE)
#' ident <- ifelse(tp, "suspect", sample(c("suspect","filler","reject"), n, replace=TRUE))
#' df    <- data.frame(target_present = tp, identification = ident, confidence = conf)
#' res   <- calibration_bayes(df, confidence_bins = c(0, 60, 80, 100))
#' print(res)
#' plot(res)
#'
#' @importFrom stats rbeta quantile median
#' @inheritParams make_rocdata
#' @export
calibration_bayes <- function(data,
                               confidence_bins = NULL,
                               choosers_only   = TRUE,
                               lineup_size     = 6,
                               alpha           = 0.5,
                               S               = 10000,
                               credible_mass   = 0.95,
                               confidence_scale = c("auto", "0-1", "0-100"),
                               innocent_suspect = "auto") {
  confidence_scale <- match.arg(confidence_scale)
  alpha_val <- .resolve_alpha_scalar(alpha)
  if (!is.numeric(S) || S < 1)
    stop("S must be a positive integer.", call. = FALSE)
  if (!is.numeric(credible_mass) || credible_mass <= 0 || credible_mass >= 1)
    stop("credible_mass must be in (0, 1).", call. = FALSE)

  freq <- make_calibration_data(data,
                                confidence_bins = confidence_bins,
                                choosers_only   = choosers_only,
                                lineup_size     = lineup_size,
                                confidence_scale = confidence_scale,
                                innocent_suspect = innocent_suspect)
  bin_df <- freq$calibration_data
  N      <- freq$n_total
  conf_scale <- freq$confidence_scale

  if (nrow(bin_df) == 0)
    stop("No bins with data - check confidence_bins and data.", call. = FALSE)

  n_j   <- bin_df$n
  k_j   <- round(bin_df$n_correct)
  c_j   <- bin_df$mean_confidence / conf_scale  # proportion scale
  w_j   <- n_j / N
  c_bar <- sum(w_j * c_j)  # weighted mean confidence (proportion)

  # Draw S posterior accuracy samples per bin: matrix [S x n_bins]
  a_mat <- mapply(
    function(k, n) rbeta(S, k + alpha_val, n - k + alpha_val),
    k = k_j, n = n_j
  )
  if (is.vector(a_mat)) a_mat <- matrix(a_mat, nrow = S, ncol = 1)

  # Posterior C (vectorised over draws)
  C_draws <- as.numeric(a_mat %*% (w_j * c_j^2) -
                         2 * (a_mat %*% (w_j * c_j)) * c_j[1] +
                         sum(w_j * c_j^2))
  # More directly:
  C_draws <- rowSums(sweep(a_mat, 2, w_j * c_j^2, "*")) * 0  # reset
  C_draws <- vapply(seq_len(S), function(s) {
    sum(w_j * (c_j - a_mat[s, ])^2)
  }, numeric(1))

  # Posterior OU
  OU_draws <- vapply(seq_len(S), function(s) {
    c_bar - sum(w_j * a_mat[s, ])
  }, numeric(1))

  # Posterior NRI
  NRI_draws <- vapply(seq_len(S), function(s) {
    a_bar_s <- sum(w_j * a_mat[s, ])
    if (a_bar_s <= 0 || a_bar_s >= 1) return(NA_real_)
    var_comp <- sum(w_j * (a_mat[s, ] - a_bar_s)^2)
    var_comp / (a_bar_s * (1 - a_bar_s))
  }, numeric(1))

  tail_p <- (1 - credible_mass) / 2
  ci_C   <- quantile(C_draws,   probs = c(tail_p, 1 - tail_p), na.rm = TRUE)
  ci_OU  <- quantile(OU_draws,  probs = c(tail_p, 1 - tail_p), na.rm = TRUE)
  ci_NRI <- quantile(NRI_draws, probs = c(tail_p, 1 - tail_p), na.rm = TRUE)
  names(ci_C) <- names(ci_OU) <- names(ci_NRI) <- c("lower", "upper")

  structure(
    list(
      C_draws   = C_draws,
      OU_draws  = OU_draws,
      NRI_draws = NRI_draws,
      C_mean    = mean(C_draws),   C_median  = median(C_draws),   C_ci  = ci_C,
      OU_mean   = mean(OU_draws),  OU_median = median(OU_draws),  OU_ci = ci_OU,
      NRI_mean  = mean(NRI_draws, na.rm=TRUE),
      NRI_median= median(NRI_draws, na.rm=TRUE),
      NRI_ci    = ci_NRI,
      point_estimates = list(C = freq$C, OU = freq$OU, NRI = freq$NRI),
      bin_data      = bin_df,
      prior_alpha   = alpha_val,
      S             = S,
      credible_mass = credible_mass,
      n_total       = N
    ),
    class = "calibration_bayes"
  )
}

#' @export
print.calibration_bayes <- function(x, digits = 3, ...) {
  pct <- round(x$credible_mass * 100)
  cat("Bayesian Calibration Analysis - Beta-Binomial model\n")
  cat(sprintf("  n = %d; %d confidence bins; prior alpha = %.2f\n",
              x$n_total, nrow(x$bin_data), x$prior_alpha))
  cat(sprintf("  C (calibration):      mean = %.4f, median = %.4f, %d%% CI [%.4f, %.4f]\n",
              x$C_mean, x$C_median, pct, x$C_ci["lower"], x$C_ci["upper"]))
  cat(sprintf("  O/U (over/underconf): mean = %.4f, median = %.4f, %d%% CI [%.4f, %.4f]\n",
              x$OU_mean, x$OU_median, pct, x$OU_ci["lower"], x$OU_ci["upper"]))
  cat(sprintf("  NRI (resolution):     mean = %.4f, median = %.4f, %d%% CI [%.4f, %.4f]\n",
              x$NRI_mean, x$NRI_median, pct, x$NRI_ci["lower"], x$NRI_ci["upper"]))
  cat(sprintf("  (Frequentist: C = %.4f, O/U = %.4f, NRI = %.4f)\n",
              x$point_estimates$C, x$point_estimates$OU,
              ifelse(is.na(x$point_estimates$NRI), NA, x$point_estimates$NRI)))
  invisible(x)
}

#' @export
plot.calibration_bayes <- function(x, metric = c("C", "OU", "NRI"), ...) {
  metric <- match.arg(metric)
  draws  <- switch(metric, C = x$C_draws, OU = x$OU_draws, NRI = x$NRI_draws)
  ci     <- switch(metric, C = x$C_ci,   OU = x$OU_ci,    NRI = x$NRI_ci)
  med    <- switch(metric, C = x$C_median, OU = x$OU_median, NRI = x$NRI_median)
  pct    <- round(x$credible_mass * 100)
  xlab   <- switch(metric,
    C   = "Calibration (C)",
    OU  = "Over/underconfidence (O/U)",
    NRI = "Normalized Resolution Index (NRI)"
  )
  ggplot2::ggplot(data.frame(val = draws[!is.na(draws)]),
                  ggplot2::aes(x = val)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            bins = 40, fill = "#72b7b2", alpha = 0.6) +
    ggplot2::geom_density(colour = "#1f2d3d", linewidth = 1) +
    ggplot2::geom_vline(xintercept = ci["lower"], linetype="dashed", colour="#e15759") +
    ggplot2::geom_vline(xintercept = ci["upper"], linetype="dashed", colour="#e15759") +
    ggplot2::geom_vline(xintercept = med, linetype="solid", colour="#1f2d3d") +
    ggplot2::labs(
      x     = xlab, y = "Density",
      title = sprintf("Posterior distribution of %s", metric),
      subtitle = sprintf("Median = %.4f; %d%% CI [%.4f, %.4f]", med, pct,
                         ci["lower"], ci["upper"])
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

utils::globalVariables("val")
