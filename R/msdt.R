#' mSDT: Max Filler Distribution (IID Normal Fillers)
#'
#' Distribution functions for the maximum of (m-1) independent standard normal
#' filler signals, where m is the lineup size (suspect + fillers).
#'
#' @param x,q Numeric vector of quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Number of random draws.
#' @param lineup_size Lineup size (suspect + fillers), must be >= 2.
#' @param log,log.p Logical. Return log-density / log-probability.
#' @param lower.tail Logical. If TRUE (default), returns P[X <= q].
#'
#' @return Density, distribution, quantile, or random draws.
#' @export
#'
#' @examples
#' dmax_filler(0, lineup_size = 6)
#' pmax_filler(0, lineup_size = 6)
#' qmax_filler(0.5, lineup_size = 6)
#' rmax_filler(5, lineup_size = 6)
dmax_filler <- function(x, lineup_size, log = FALSE) {
  m <- msdt_check_lineup_size(lineup_size)
  n <- m - 1
  if (log) {
    if (n == 1) return(stats::dnorm(x, log = TRUE))
    return(log(n) + stats::dnorm(x, log = TRUE) +
             (n - 1) * stats::pnorm(x, log.p = TRUE))
  }
  n * stats::dnorm(x) * stats::pnorm(x)^(n - 1)
}

#' @rdname dmax_filler
#' @export
pmax_filler <- function(q, lineup_size, lower.tail = TRUE, log.p = FALSE) {
  m <- msdt_check_lineup_size(lineup_size)
  n <- m - 1
  log_cdf <- n * stats::pnorm(q, log.p = TRUE)
  if (lower.tail) return(if (log.p) log_cdf else exp(log_cdf))
  log_single_tail <- stats::pnorm(q, lower.tail = FALSE, log.p = TRUE)
  # At very small tails log(1 - Phi(q)^n) = log(n) + log(1-Phi(q))
  # to machine precision; taking 1 - exp(log_cdf) would round to zero.
  log_tail <- ifelse(log_single_tail < -36,
                     log(n) + log_single_tail, log(-expm1(log_cdf)))
  if (log.p) log_tail else exp(log_tail)
}

#' @rdname dmax_filler
#' @export
qmax_filler <- function(p, lineup_size, lower.tail = TRUE, log.p = FALSE) {
  m <- msdt_check_lineup_size(lineup_size)
  n <- m - 1
  lp <- if (log.p) p else log(p)
  if (lower.tail) return(stats::qnorm(lp / n, log.p = TRUE))
  log_single_tail <- ifelse(lp < -36, lp - log(n),
    log(-expm1(log1p(-exp(lp)) / n)))
  stats::qnorm(log_single_tail, lower.tail = FALSE, log.p = TRUE)
}

#' @rdname dmax_filler
#' @export
rmax_filler <- function(n, lineup_size) {
  if (!is.numeric(n) || length(n) != 1 || !is.finite(n) ||
      n < 0 || n != floor(n)) {
    stop("n must be a non-negative integer.", call. = FALSE)
  }
  u <- stats::runif(n)
  qmax_filler(u, lineup_size = lineup_size)
}

#' Max Filler Distribution Moments
#'
#' Numeric moments of the max filler distribution using integration.
#'
#' @param lineup_size Lineup size (suspect + fillers), must be >= 2.
#' @param moments Character vector specifying moments to return.
#'   Supported: "mean", "var", "skewness".
#' @param rel.tol Relative tolerance passed to integrate().
#' @param subdivisions Maximum subdivisions for integrate().
#'
#' @return Named list of moments.
#' @export
#'
#' @examples
#' max_filler_moments(6)
max_filler_moments <- function(lineup_size,
                               moments = c("mean", "var", "skewness"),
                               rel.tol = 1e-8,
                               subdivisions = 2000) {
  m <- msdt_check_lineup_size(lineup_size)
  moments <- match.arg(moments, several.ok = TRUE)
  n <- m - 1

  fmax <- function(x) n * stats::dnorm(x) * stats::pnorm(x)^(n - 1)
  raw_moment <- function(k) {
    stats::integrate(function(x) (x^k) * fmax(x),
      lower = -Inf, upper = Inf, rel.tol = rel.tol,
      subdivisions = subdivisions
    )$value
  }

  out <- list()
  if ("mean" %in% moments || "var" %in% moments || "skewness" %in% moments) {
    m1 <- raw_moment(1)
  }
  if ("var" %in% moments || "skewness" %in% moments) {
    m2 <- raw_moment(2)
  }
  if ("skewness" %in% moments) {
    m3 <- raw_moment(3)
  }

  if ("mean" %in% moments) {
    out$mean <- m1
  }
  if ("var" %in% moments) {
    out$var <- m2 - m1^2
  }
  if ("skewness" %in% moments) {
    varx <- m2 - m1^2
    mu3 <- m3 - 3 * m1 * m2 + 2 * m1^3
    out$skewness <- mu3 / (varx^(3 / 2))
  }
  out
}

#' mSDT Parameter Estimation from Rejection Rates
#'
#' Estimate decision criterion (gamma) and discriminability (d') from
#' rejection rates in culprit-absent and culprit-present lineups.
#'
#' @param pr_rej_I Rejection rate in culprit-absent lineups.
#' @param pr_rej_G Rejection rate in culprit-present lineups.
#' @param gamma Decision criterion estimated from culprit-absent rejection
#'   rates, typically returned by \code{msdt_gamma_from_rej()}.
#' @param lineup_size Lineup size (suspect + fillers), must be >= 2.
#' @param eps Small value used to avoid probabilities of 0 or 1.
#'
#' @details
#' This is a \strong{method-of-moments estimator} that is exactly identified
#' from the two rejection rates alone: Pr(REJ|I) fixes gamma, and the gap
#' between culprit-absent and culprit-present rejection rates fixes d'. It is
#' \strong{information-lossy}: it ignores the composition of choices
#' (suspect-ID vs filler-ID) and the spread of filler choices, which are the
#' most diagnostic outcomes. If you have the full response breakdown
#' (suspect/filler/reject counts for both lineup types), prefer the
#' minimum-Pearson fitter \code{\link{fit_max_sdt}} or the 2-HT model
#' (\code{\link{fit_winter_2ht}}), which use all outcome counts.
#'
#' @return A list with gamma and dprime.
#' @seealso \code{\link{fit_max_sdt}} for a full-outcome, minimum-Pearson fit;
#'   \code{\link{fit_winter_2ht}} for the 2-HT MPT model.
#' @export
#'
#' @examples
#' estimate_msdt_params(pr_rej_I = 0.25, pr_rej_G = 0.15, lineup_size = 6)
estimate_msdt_params <- function(pr_rej_I,
                                 pr_rej_G,
                                 lineup_size,
                                 eps = 1e-6) {
  gamma <- msdt_gamma_from_rej(pr_rej_I, lineup_size = lineup_size, eps = eps)
  dprime <- msdt_dprime_from_rej(pr_rej_G, gamma, lineup_size = lineup_size, eps = eps)
  list(gamma = gamma, dprime = dprime)
}

#' @rdname estimate_msdt_params
#' @export
msdt_gamma_from_rej <- function(pr_rej_I, lineup_size, eps = 1e-6) {
  m <- msdt_check_lineup_size(lineup_size)
  pr_rej_I <- msdt_clamp_prob(pr_rej_I, eps = eps, name = "pr_rej_I")
  stats::qnorm(pr_rej_I^(1 / m))
}

#' @rdname estimate_msdt_params
#' @export
msdt_dprime_from_rej <- function(pr_rej_G, gamma, lineup_size, eps = 1e-6) {
  m <- msdt_check_lineup_size(lineup_size)
  pr_rej_G <- msdt_clamp_prob(pr_rej_G, eps = eps, name = "pr_rej_G")
  denom <- stats::pnorm(gamma)^(m - 1)
  ratio <- pr_rej_G / denom
  ratio <- msdt_clamp_prob(ratio, eps = eps, name = "pr_rej_G / Phi(gamma)^(m-1)")
  gamma - stats::qnorm(ratio)
}

#' Plot mSDT Joint Distributions
#'
#' Contour plot of the joint distribution of suspect signal and max filler signal.
#'
#' @param dprime Discriminability between guilty and innocent suspects.
#' @param gamma Decision criterion.
#' @param lineup_size Lineup size (suspect + fillers), must be >= 2.
#' @param lineup_type Either "culprit_present" or "culprit_absent".
#' @param n Grid resolution per axis.
#' @param xlim,ylim Optional numeric ranges for axes.
#' @param show_decision Logical. If TRUE, overlays MAX rule boundaries.
#'
#' @return A ggplot object.
#' @examples
#' # Joint distribution for a culprit-present 6-person lineup
#' plot_msdt_joint(dprime = 1.5, gamma = 0.8, lineup_size = 6)
#' @export
plot_msdt_joint <- function(dprime,
                            gamma,
                            lineup_size,
                            lineup_type = c("culprit_present", "culprit_absent"),
                            n = 120,
                            xlim = NULL,
                            ylim = NULL,
                            show_decision = TRUE) {
  lineup_type <- match.arg(lineup_type)
  m <- msdt_check_lineup_size(lineup_size)

  mu <- if (lineup_type == "culprit_present") dprime else 0
  if (is.null(xlim)) {
    xlim <- stats::qnorm(c(0.001, 0.999), mean = mu, sd = 1)
  }
  if (is.null(ylim)) {
    ylim <- qmax_filler(c(0.001, 0.999), lineup_size = m)
  }

  xseq <- seq(xlim[1], xlim[2], length.out = n)
  yseq <- seq(ylim[1], ylim[2], length.out = n)
  grid <- expand.grid(ss = xseq, fsmax = yseq)
  grid$z <- stats::dnorm(grid$ss, mean = mu, sd = 1) * dmax_filler(grid$fsmax, lineup_size = m)

  p <- ggplot2::ggplot(grid, ggplot2::aes(x = ss, y = fsmax, z = z)) +
    ggplot2::geom_contour(color = "steelblue") +
    ggplot2::labs(
      x = "Suspect signal",
      y = "Max filler signal",
      title = paste("mSDT joint density (", gsub("_", " ", lineup_type), ")", sep = "")
    ) +
    ggplot2::theme_minimal()

  if (isTRUE(show_decision)) {
    p <- p +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
      ggplot2::geom_vline(xintercept = gamma, linetype = "dotted", color = "grey50") +
      ggplot2::geom_hline(yintercept = gamma, linetype = "dotted", color = "grey50")
  }
  p
}

msdt_check_lineup_size <- function(lineup_size) {
  if (length(lineup_size) != 1 || !is.finite(lineup_size)) {
    stop("lineup_size must be a finite scalar.", call. = FALSE)
  }
  if (abs(lineup_size - round(lineup_size)) > 0) {
    stop("lineup_size must be an integer.", call. = FALSE)
  }
  if (lineup_size < 2) {
    stop("lineup_size must be >= 2.", call. = FALSE)
  }
  as.integer(lineup_size)
}

msdt_clamp_prob <- function(p, eps = 1e-6, name = "p") {
  if (any(!is.finite(p))) {
    stop(paste0(name, " must be finite."), call. = FALSE)
  }
  if (any(p < 0 | p > 1)) {
    stop(paste0(name, " must be in [0, 1]."), call. = FALSE)
  }
  if (eps <= 0) {
    return(p)
  }
  p_adj <- pmin(pmax(p, eps), 1 - eps)
  if (any(p_adj != p)) {
    warning(paste0(name, " adjusted to avoid 0/1 probabilities."), call. = FALSE)
  }
  p_adj
}
