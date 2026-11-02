#' Compute Deviation from Perfect Performance (DPP)
#'
#' Computes the average deviation from perfect performance of Smith, Lampinen,
#' Wells, Smalarz and Mackovichova (2019). DPP is calculated at every operating
#' point (cumulative confidence threshold) of the ROC and averaged over them,
#' so that, unlike pAUC, it is not tied to a region of false-ID rates that two
#' differently truncated ROC curves may not share.
#'
#' @param data A dataframe with the following columns:
#'   \itemize{
#'     \item target_present: Logical. TRUE if guilty suspect in lineup
#'     \item identification: Character. "suspect", "filler", or "reject"
#'     \item confidence: Numeric. Confidence rating
#'   }
#'   Alternatively, with \code{use_roc_obj = TRUE}, the output of
#'   \code{\link{make_rocdata}}.
#' @param lineup_size Integer. Number of people in lineup (default = 6)
#' @param use_roc_obj Logical. If FALSE (default), computes ROC from data.
#'   If TRUE, data should be the output from make_rocdata(), whose first
#'   \code{roc_data} row is the origin and is not an operating point.
#' @param alpha,beta Optional weights for Smith et al.'s Equation 3.
#'   \code{alpha} is the cost of an innocent-suspect identification relative to
#'   a missed culprit identification, and \code{beta} is the base rate of
#'   culprit presence. Supply both or neither. When both are \code{NULL}
#'   (default), the unweighted Equation 2 is used.
#' @inheritParams make_rocdata
#'
#' @return A list containing:
#'   \itemize{
#'     \item dpp: Average DPP over the operating points
#'     \item dpp_by_point: Tibble with confidence threshold, culprit
#'       (correct-ID) rate, innocent-suspect ID rate and DPP at each operating
#'       point
#'     \item best_point: The operating point with the lowest DPP, which is the
#'       point of maximum utility
#'     \item roc_data: ROC curve data from \code{make_rocdata()}
#'     \item alpha, beta: The weights used (\code{NULL} for Equation 2)
#'     \item n_target_present, n_target_absent: Sample sizes
#'     \item innocent_suspect_method: How innocent-suspect IDs were obtained
#'     \item thresholds: The confidence criteria of the operating points
#'   }
#'
#' @details
#' At an operating point \eqn{c}, Smith et al.'s (2019) Equation 2 is
#'
#' \deqn{DPP(c) = 1 - Culprit(c) + Innocent(c)}
#'
#' where \eqn{Culprit(c)} is the culprit (correct) identification rate and
#' \eqn{Innocent(c)} the innocent-suspect identification rate at that point.
#' DPP(c) ranges from 0 (perfect performance: every culprit identified, no
#' innocent suspect identified) to 2 (the reverse). Any point on the chance
#' diagonal has DPP(c) = 1. When the base rate of culprit presence is .5 and
#' the costs of the two errors are equal, DPP(c) = 1 - U(c), where
#' \eqn{U(c) = Culprit(c) - Innocent(c)} is the expected utility.
#'
#' The reported \code{dpp} is the mean of DPP(c) over the operating points,
#' one for each confidence criterion of the ROC, from the highest to the
#' lowest (which includes every identification). Lower values are better. The
#' criteria are the confidence levels of the responses that enter the ROC
#' (see \code{\link{make_rocdata}}), so the confidence given to rejections
#' does not add points or change the average.
#'
#' Equation 3 weights the two errors:
#'
#' \deqn{DPP_{\alpha\beta}(c) = \beta[1 - Culprit(c)] + \alpha(1 - \beta)Innocent(c)}
#'
#' With \code{alpha = 1} and \code{beta = 0.5} this is half of Equation 2.
#' Weighted values no longer range from 0 to 2.
#'
#' @references
#' Smith, A. M., Lampinen, J. M., Wells, G. L., Smalarz, L., & Mackovichova,
#' S. (2019). Deviation from perfect performance measures the diagnostic
#' utility of eyewitness lineups but partial area under the ROC curve does
#' not. \emph{Journal of Applied Research in Memory and Cognition, 8}(1),
#' 50-59. \doi{10.1016/j.jarmac.2018.09.003}
#'
#' @examples
#' data(lineup_example)
#' dpp <- make_dpp(lineup_example)
#' dpp$dpp
#' dpp$dpp_by_point
#'
#' # Innocent-suspect IDs twice as costly as misses, culprit base rate .5
#' make_dpp(lineup_example, alpha = 2, beta = 0.5)$dpp
#'
#' @export
#' @import tibble
make_dpp <- function(data, lineup_size = 6, use_roc_obj = FALSE,
                     alpha = NULL, beta = NULL, innocent_suspect = "auto",
                     thresholds = NULL) {
  weights <- .dpp_weights(alpha, beta)

  if (use_roc_obj) {
    roc_obj <- data
    if (!is.list(roc_obj) || is.null(roc_obj$roc_data) ||
        !all(c("confidence", "correct_id_rate", "false_id_rate") %in%
             names(roc_obj$roc_data))) {
      stop("With use_roc_obj = TRUE, data must be output from make_rocdata().",
           call. = FALSE)
    }
  } else {
    required_cols <- c("target_present", "identification", "confidence")
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }
    roc_obj <- make_rocdata(data, lineup_size = lineup_size,
                            innocent_suspect = innocent_suspect,
                            thresholds = thresholds)
  }
  roc_data <- roc_obj$roc_data

  # Row 1 of make_rocdata() output is the (0, 0) origin, which is not an
  # operating point (no confidence level produces it).
  points <- roc_data[-1, , drop = FALSE]
  if (nrow(points) == 0) {
    stop("The ROC has no operating points.", call. = FALSE)
  }
  dpp_by_point <- tibble::tibble(
    confidence = points$confidence,
    culprit_rate = points$correct_id_rate,
    innocent_rate = points$false_id_rate,
    dpp = .dpp_at(points$correct_id_rate, points$false_id_rate, weights)
  )

  list(
    dpp = mean(dpp_by_point$dpp),
    dpp_by_point = dpp_by_point,
    best_point = dpp_by_point[which.min(dpp_by_point$dpp), ],
    roc_data = roc_data,
    alpha = alpha,
    beta = beta,
    n_target_present = roc_obj$n_target_present,
    n_target_absent = roc_obj$n_target_absent,
    innocent_suspect_method = roc_obj$innocent_suspect_method,
    thresholds = roc_obj$thresholds
  )
}

# Validate the Equation 3 weights; NULL means the unweighted Equation 2.
.dpp_weights <- function(alpha, beta) {
  if (is.null(alpha) && is.null(beta)) return(NULL)
  if (is.null(alpha) || is.null(beta)) {
    stop("Supply both alpha and beta, or neither.", call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) ||
      alpha < 0) {
    stop("alpha must be a single non-negative number.", call. = FALSE)
  }
  if (!is.numeric(beta) || length(beta) != 1L || !is.finite(beta) ||
      beta < 0 || beta > 1) {
    stop("beta must be a single number between 0 and 1.", call. = FALSE)
  }
  c(alpha = alpha, beta = beta)
}

# DPP at each operating point (Smith et al., 2019, Equations 2 and 3)
.dpp_at <- function(culprit, innocent, weights) {
  if (is.null(weights)) {
    1 - culprit + innocent
  } else {
    weights[["beta"]] * (1 - culprit) +
      weights[["alpha"]] * (1 - weights[["beta"]]) * innocent
  }
}


#' Compare DPP Between Two Procedures
#'
#' Computes average DPP for two lineup procedures and tests their difference
#' with a nonparametric bootstrap, as Smith et al. (2019) did.
#'
#' @param data_proc_a Dataframe for procedure A (standard lineup format)
#' @param data_proc_b Dataframe for procedure B (standard lineup format)
#' @param lineup_size Integer. Lineup size (default = 6)
#' @param alpha,beta Optional Equation 3 weights; see \code{\link{make_dpp}}.
#' @param n_bootstrap Integer. Number of bootstrap samples (default = 1000).
#'   Use 0 to skip the bootstrap.
#' @param conf_level Numeric. Confidence level for the interval (default = 0.95)
#' @param seed Integer. Random seed for reproducibility (default = NULL)
#' @inheritParams make_rocdata
#'
#' @return An object of class \code{"lineup_dpp_comparison"}, a list containing:
#'   \itemize{
#'     \item dpp_a: Average DPP for procedure A
#'     \item dpp_b: Average DPP for procedure B
#'     \item dpp_difference: DPP_A - DPP_B (negative = A is better)
#'     \item se_difference: Bootstrap standard error of the difference
#'     \item ci_difference: Percentile bootstrap interval for the difference
#'     \item boot_differences: Bootstrap distribution of the difference
#'     \item dpp_obj_a, dpp_obj_b: Full \code{make_dpp()} objects
#'     \item n_bootstrap, conf_level: Settings used
#'   }
#'
#' @details
#' Lower DPP indicates better performance, so a negative difference means
#' procedure A is better. Each bootstrap replicate resamples target-present
#' and target-absent trials separately within each procedure, so every
#' replicate keeps the original design, and the two procedures are treated as
#' independent samples. The innocent-suspect design and the confidence
#' criteria are determined once on each full dataset and reused in every
#' replicate, so every replicate averages over the same criteria.
#'
#' @references
#' Smith, A. M., Lampinen, J. M., Wells, G. L., Smalarz, L., & Mackovichova,
#' S. (2019). Deviation from perfect performance measures the diagnostic
#' utility of eyewitness lineups but partial area under the ROC curve does
#' not. \emph{Journal of Applied Research in Memory and Cognition, 8}(1),
#' 50-59. \doi{10.1016/j.jarmac.2018.09.003}
#'
#' @examples
#' data(lineup_example)
#' # Compare two halves of the example data (odd vs even rows)
#' odd <- seq(1, nrow(lineup_example), by = 2)
#' # 50 resamples keep the example quick; use 1000 or more in an analysis
#' cmp <- compare_dpp(lineup_example[odd, ], lineup_example[-odd, ],
#'                    n_bootstrap = 50, seed = 1)
#' cmp
#'
#' @export
compare_dpp <- function(data_proc_a, data_proc_b, lineup_size = 6,
                        alpha = NULL, beta = NULL, n_bootstrap = 1000,
                        conf_level = 0.95, seed = NULL,
                        innocent_suspect = "auto") {
  if (!is.numeric(n_bootstrap) || length(n_bootstrap) != 1L ||
      n_bootstrap < 0 || n_bootstrap != as.integer(n_bootstrap) ||
      n_bootstrap == 1) {
    stop("n_bootstrap must be 0 or an integer of at least 2.", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("conf_level must be strictly between 0 and 1.", call. = FALSE)
  }

  dpp_a <- make_dpp(data_proc_a, lineup_size = lineup_size, alpha = alpha,
                    beta = beta, innocent_suspect = innocent_suspect)
  dpp_b <- make_dpp(data_proc_b, lineup_size = lineup_size, alpha = alpha,
                    beta = beta, innocent_suspect = innocent_suspect)
  difference <- dpp_a$dpp - dpp_b$dpp

  boot_diff <- numeric()
  se <- NA_real_
  ci <- c(lower = NA_real_, upper = NA_real_)
  if (n_bootstrap > 0) {
    restore_rng <- .local_seed(seed)
    on.exit(restore_rng(), add = TRUE)
    resample <- function(dat) {
      tp <- which(as.logical(dat$target_present))
      ta <- which(!as.logical(dat$target_present))
      dat[c(tp[sample.int(length(tp), length(tp), replace = TRUE)],
            ta[sample.int(length(ta), length(ta), replace = TRUE)]), ,
          drop = FALSE]
    }
    boot_dpp <- function(dat, fit) {
      make_dpp(resample(dat), lineup_size = lineup_size, alpha = alpha,
               beta = beta, innocent_suspect = fit$innocent_suspect_method,
               thresholds = fit$thresholds)$dpp
    }
    boot_diff <- vapply(seq_len(n_bootstrap), function(b) {
      boot_dpp(data_proc_a, dpp_a) - boot_dpp(data_proc_b, dpp_b)
    }, numeric(1))
    se <- stats::sd(boot_diff)
    tail <- (1 - conf_level) / 2
    ci <- stats::quantile(boot_diff, c(tail, 1 - tail), names = FALSE)
    names(ci) <- c("lower", "upper")
  }

  structure(
    list(
      dpp_a = dpp_a$dpp,
      dpp_b = dpp_b$dpp,
      dpp_difference = difference,
      se_difference = se,
      ci_difference = ci,
      boot_differences = boot_diff,
      dpp_obj_a = dpp_a,
      dpp_obj_b = dpp_b,
      n_bootstrap = n_bootstrap,
      conf_level = conf_level
    ),
    class = c("lineup_dpp_comparison", "list")
  )
}


#' Print Method for lineup_dpp_comparison Objects
#' @param x A lineup_dpp_comparison object from \code{compare_dpp()}
#' @param ... Additional arguments (ignored)
#' @export
print.lineup_dpp_comparison <- function(x, ...) {
  cat("\n=== DPP Comparison ===\n\n")
  cat("Average DPP, procedure A: ", sprintf("%.4f", x$dpp_a), "\n")
  cat("Average DPP, procedure B: ", sprintf("%.4f", x$dpp_b), "\n")
  cat("Difference (A - B):       ", sprintf("%.4f", x$dpp_difference),
      " (negative = A better)\n")
  if (x$n_bootstrap > 0) {
    cat(sprintf("Bootstrap SE:              %.4f\n", x$se_difference))
    cat(sprintf("%g%% percentile interval:  [%.4f, %.4f]  (%d resamples)\n",
                100 * x$conf_level, x$ci_difference[["lower"]],
                x$ci_difference[["upper"]], x$n_bootstrap))
  }
  invisible(x)
}


#' Plot DPP Across Operating Points
#'
#' Plots DPP at each operating point of the ROC against the confidence
#' threshold that defines it, with the average marked.
#'
#' @param dpp_obj List output from make_dpp() or compute_dpp()
#' @param show_mean Logical. Draw the average DPP as a dashed line
#'   (default = TRUE)
#' @param show_chance Logical. Draw the chance reference at DPP = 1, which
#'   applies to the unweighted measure only (default = TRUE)
#' @param show_dpp Logical. Display the average DPP value on the plot
#'   (default = TRUE)
#'
#' @return A ggplot2 object
#'
#' @details
#' Points are ordered from the strictest threshold (highest confidence, left)
#' to the most lenient (right), as in Smith et al.'s (2019) Table 4. The
#' operating point with the lowest DPP, the point of maximum utility, is
#' highlighted.
#'
#' @examples
#' data(lineup_example)
#' dpp <- make_dpp(lineup_example)
#' plot_dpp(dpp)
#'
#' @export
#' @import ggplot2
plot_dpp <- function(dpp_obj, show_mean = TRUE, show_chance = TRUE,
                     show_dpp = TRUE) {
  pts <- dpp_obj$dpp_by_point
  best <- dpp_obj$best_point
  weighted <- !is.null(dpp_obj$alpha)

  p <- ggplot(pts, aes(x = confidence, y = dpp)) +
    geom_line(linewidth = 1, color = "steelblue") +
    geom_point(size = 2.5, color = "steelblue") +
    geom_point(data = best, size = 4, shape = 21, fill = "white",
               color = "firebrick", stroke = 1.2) +
    scale_x_reverse() +
    theme_bw(base_size = 14) +
    labs(
      x = "Confidence threshold (identifications at or above)",
      y = if (weighted) "Weighted DPP(c)" else "DPP(c)",
      title = "Deviation from Perfect Performance by Operating Point",
      caption = "0 = perfect; lower is better. Circled: point of maximum utility."
    )

  if (show_chance && !weighted) {
    p <- p + geom_hline(yintercept = 1, linetype = "dotted", color = "gray40")
  }
  if (show_mean) {
    p <- p + geom_hline(yintercept = dpp_obj$dpp, linetype = "dashed",
                        color = "steelblue")
  }
  if (show_dpp) {
    p <- p + labs(subtitle = sprintf("Average DPP = %.3f", dpp_obj$dpp))
  }
  if (!weighted) {
    p <- p + coord_cartesian(ylim = c(0, max(1.05, max(pts$dpp))))
  }
  p
}


#' Plot DPP Comparison Between Two Procedures
#'
#' Overlays DPP at each operating point for two procedures.
#'
#' @param compare_obj List output from compare_dpp()
#' @param proc_a_label Character. Label for procedure A (default = "Procedure A")
#' @param proc_b_label Character. Label for procedure B (default = "Procedure B")
#'
#' @return A ggplot2 object
#'
#' @examples
#' data(lineup_example)
#' odd <- seq(1, nrow(lineup_example), by = 2)
#' cmp <- compare_dpp(lineup_example[odd, ], lineup_example[-odd, ],
#'                    n_bootstrap = 0)
#' plot_dpp_comparison(cmp)
#'
#' @export
#' @import ggplot2
plot_dpp_comparison <- function(compare_obj,
                                proc_a_label = "Procedure A",
                                proc_b_label = "Procedure B") {
  a <- compare_obj$dpp_obj_a$dpp_by_point
  b <- compare_obj$dpp_obj_b$dpp_by_point
  a$procedure <- proc_a_label
  b$procedure <- proc_b_label
  pts <- rbind(a, b)
  pts$procedure <- factor(pts$procedure, levels = c(proc_a_label, proc_b_label))
  means <- data.frame(
    procedure = factor(c(proc_a_label, proc_b_label),
                       levels = c(proc_a_label, proc_b_label)),
    dpp = c(compare_obj$dpp_a, compare_obj$dpp_b)
  )
  weighted <- !is.null(compare_obj$dpp_obj_a$alpha)

  subtitle <- sprintf("%s: %.3f  |  %s: %.3f\nDifference = %.3f",
                      proc_a_label, compare_obj$dpp_a,
                      proc_b_label, compare_obj$dpp_b,
                      compare_obj$dpp_difference)
  if (isTRUE(compare_obj$n_bootstrap > 0)) {
    subtitle <- sprintf("%s [%.3f, %.3f]", subtitle,
                        compare_obj$ci_difference[["lower"]],
                        compare_obj$ci_difference[["upper"]])
  }

  p <- ggplot(pts, aes(x = confidence, y = dpp, color = procedure,
                       group = procedure)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    geom_hline(data = means, aes(yintercept = dpp, color = procedure),
               linetype = "dashed") +
    scale_x_reverse() +
    theme_bw(base_size = 14) +
    labs(
      x = "Confidence threshold (identifications at or above)",
      y = if (weighted) "Weighted DPP(c)" else "DPP(c)",
      title = "DPP Comparison",
      subtitle = subtitle,
      color = "Procedure",
      caption = "Dashed lines: average DPP. Lower is better."
    )
  if (!weighted) {
    p <- p + geom_hline(yintercept = 1, linetype = "dotted", color = "gray40") +
      coord_cartesian(ylim = c(0, max(1.05, max(pts$dpp))))
  }
  p
}


#' Compute and Plot DPP
#'
#' Main wrapper function to compute and plot DPP analysis.
#'
#' @param data Dataframe with standard lineup format
#' @param lineup_size Integer. Lineup size (default = 6)
#' @param show_plot Logical. Whether to display plot (default = TRUE)
#' @param alpha,beta Optional Equation 3 weights; see \code{\link{make_dpp}}.
#' @param ... Additional arguments passed to plot_dpp()
#' @inheritParams make_rocdata
#'
#' @return An object of class \code{"lineup_dpp"}: the \code{make_dpp()}
#'   result plus the plot.
#'
#' @examples
#' data(lineup_example)
#' dpp_result <- compute_dpp(lineup_example, show_plot = FALSE)
#' print(dpp_result)
#'
#' @export
compute_dpp <- function(data, lineup_size = 6, show_plot = TRUE,
                        alpha = NULL, beta = NULL, innocent_suspect = "auto",
                        ...) {
  dpp_obj <- make_dpp(data, lineup_size = lineup_size, alpha = alpha,
                      beta = beta, innocent_suspect = innocent_suspect)

  # The plot is always built, so that it is available as $plot and to the
  # plot() method. show_plot controls only whether it is displayed now:
  # suppressing display should not destroy the object.
  dpp_plot <- plot_dpp(dpp_obj, ...)
  if (show_plot) {
    print(dpp_plot)
  }

  result <- c(list(plot = dpp_plot), dpp_obj)
  class(result) <- c("lineup_dpp", "list")
  result
}


#' Print Method for lineup_dpp Objects
#' @param x A lineup_dpp object
#' @param ... Additional arguments (ignored)
#' @export
print.lineup_dpp <- function(x, ...) {
  cat("\n=== Deviation from Perfect Performance (DPP) ===\n\n")

  cat("Average DPP:", sprintf("%.4f", x$dpp), "\n")
  if (is.null(x$alpha)) {
    cat("  (0 = perfect, 1 = chance, 2 = worst; Smith et al., 2019, Eq. 2)\n\n")
  } else {
    cat(sprintf("  (weighted, alpha = %g, beta = %g; Smith et al., 2019, Eq. 3)\n\n",
                x$alpha, x$beta))
  }
  cat("Point of maximum utility: confidence >=", format(x$best_point$confidence),
      sprintf("(DPP = %.4f)\n\n", x$best_point$dpp))

  cat("DPP by operating point:\n")
  print(x$dpp_by_point, n = Inf)

  if (!is.null(x$plot)) {
    cat("\nPlot: save this object first, then extract it with x$plot\n")
  }

  invisible(x)
}
