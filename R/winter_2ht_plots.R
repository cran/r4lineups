#' Plot Parameter Estimates from 2-HT Model
#'
#' @description
#' Creates a visual display of the parameter estimates from a fitted 2-HT model,
#' with error bars showing the supplied bootstrap intervals, or regular 95\%
#' normal intervals where available.
#'
#' @param x A \code{winter_2ht} object, or a \code{winter_2ht_boot} object
#'   from \code{\link{boot_winter_2ht}}. Given the latter, the plotted intervals
#'   are the bootstrap percentile intervals; given the former they are normal
#'   approximations clamped to [0, 1]. from fit_winter_2ht()
#' @param ... Additional arguments passed to plotting functions
#' @param which Character vector specifying which parameters to plot.
#'   Default is c("dP", "dA", "b", "g") (all parameters).
#' @param show_ci Logical indicating whether to show 95\% confidence intervals. Default is TRUE.
#'
#' @return A ggplot object
#'
#' @examples
#' counts <- c(
#'   n_tp_suspect = 147, n_tp_filler = 94, n_tp_reject = 141,
#'   n_ta_suspect = 38, n_ta_filler = 138, n_ta_reject = 206
#' )
#' fit <- fit_winter_2ht(counts, lineup_size = 6)
#' plot_2ht_parameters(fit)
#'
#' @export
plot_2ht_parameters <- function(x, ..., which = c("dP", "dA", "b", "g"), show_ci = TRUE) {

  # A bootstrap object carries percentile intervals, which are preferable here:
  # all four parameters are probabilities bounded in [0, 1], and a normal
  # interval around an estimate near a boundary runs outside the parameter
  # space. Use them when they are available.
  boot_ci <- NULL
  if (inherits(x, "winter_2ht_boot")) {
    boot_ci <- x$ci
    x <- x$original_fit
  }
  if (!inherits(x, "winter_2ht")) {
    stop("x must be a winter_2ht or winter_2ht_boot object", call. = FALSE)
  }

  # Filter requested parameters
  params <- x$parameters[which]
  se <- x$se[which]

  if (!is.null(boot_ci)) {
    lower <- as.numeric(boot_ci[which, "Lower"])
    upper <- as.numeric(boot_ci[which, "Upper"])
  } else {
    # Normal approximation, clamped to the parameter space. The clamp is a
    # display fix, not an inferential one: an interval that reaches a boundary
    # is a sign the normal approximation is poor there, and the bootstrap
    # intervals from boot_winter_2ht() should be preferred.
    lower <- pmax(0, as.numeric(params) - 1.96 * as.numeric(se))
    upper <- pmin(1, as.numeric(params) + 1.96 * as.numeric(se))
  }

  # Create data frame for plotting
  plot_data <- data.frame(
    parameter = factor(names(params), levels = names(params)),
    estimate = as.numeric(params),
    se = as.numeric(se),
    lower = lower,
    upper = upper
  )

  # Create labels
  param_labels <- c(
    dP = "dP\n(Culprit Presence\nDetection)",
    dA = "dA\n(Culprit Absence\nDetection)",
    b = "b\n(Biased\nSelection)",
    g = "g\n(Guessing)"
  )

  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = parameter, y = estimate)) +
    ggplot2::geom_col(fill = "steelblue", alpha = 0.7, width = 0.6) +
    # Headroom above 1 for the value labels, which sit above the intervals.
    ggplot2::scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.25)) +
    ggplot2::labs(
      title = "2-HT Model Parameter Estimates",
      x = NULL,
      y = "Parameter Value"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 9)
    ) +
    ggplot2::scale_x_discrete(labels = param_labels[which])

  # Add error bars if requested
  available <- is.finite(plot_data$lower) & is.finite(plot_data$upper)
  if (show_ci && any(available)) {
    p <- p + ggplot2::geom_errorbar(
      data = plot_data[available, , drop = FALSE],
      ggplot2::aes(ymin = pmax(lower, 0), ymax = pmin(upper, 1)),
      width = 0.2,
      linewidth = 0.8
    )
  }

  # Add value labels above the interval, where the whisker cannot cover them
  plot_data$label_y <- ifelse(show_ci & available,
                              pmin(plot_data$upper, 1), plot_data$estimate)
  p <- p + ggplot2::labs(caption = if (!is.null(boot_ci))
    "Bootstrap intervals are approximate; parameter boundaries require caution."
    else if (show_ci && any(!available))
      "Regular normal intervals unavailable for this boundary or singular fit."
    else "95% normal approximation; bounds clipped for display.")
  p <- p + ggplot2::geom_text(
    data = plot_data,
    ggplot2::aes(y = label_y, label = sprintf("%.3f", estimate)),
    vjust = -0.6,
    size = 3.5
  )

  return(p)
}


#' Plot Observed vs. Expected Counts
#'
#' Creates a comparison plot showing observed counts vs. model-predicted expected counts
#' for both target-present and target-absent lineups.
#'
#' @param x A winter_2ht object from fit_winter_2ht()
#' @param ... Additional arguments (not used)
#'
#' @return A ggplot object
#'
#' @examples
#' counts <- c(
#'   n_tp_suspect = 147, n_tp_filler = 94, n_tp_reject = 141,
#'   n_ta_suspect = 38, n_ta_filler = 138, n_ta_reject = 206
#' )
#' fit <- fit_winter_2ht(counts, lineup_size = 6)
#' plot_2ht_fit(fit)
#'
#' @export
plot_2ht_fit <- function(x, ...) {

  if (!inherits(x, "winter_2ht")) {
    stop("x must be a winter_2ht object")
  }

  # Create data frame for plotting
  plot_data <- data.frame(
    lineup_type = rep(c("Target-Present", "Target-Absent"), each = 3),
    outcome = rep(c("Suspect ID", "Filler ID", "Reject"), 2),
    observed = c(x$observed_counts$tp, x$observed_counts$ta),
    expected = c(x$expected_counts$tp, x$expected_counts$ta)
  )

  # Reshape for plotting using base R
  plot_data_long <- data.frame(
    lineup_type = rep(plot_data$lineup_type, 2),
    outcome = rep(plot_data$outcome, 2),
    type = rep(c("Observed", "Expected"), each = nrow(plot_data)),
    count = c(plot_data$observed, plot_data$expected)
  )

  plot_data_long$type <- factor(
    plot_data_long$type,
    levels = c("Observed", "Expected")
  )

  plot_data_long$outcome <- factor(
    plot_data_long$outcome,
    levels = c("Suspect ID", "Filler ID", "Reject")
  )

  # Create plot
  p <- ggplot2::ggplot(
    plot_data_long,
    ggplot2::aes(x = outcome, y = count, fill = type)
  ) +
    ggplot2::geom_col(position = "dodge", alpha = 0.8) +
    ggplot2::facet_wrap(~ lineup_type, scales = "free_y") +
    ggplot2::scale_fill_manual(
      values = c("Observed" = "steelblue", "Expected" = "coral"),
      name = NULL
    ) +
    ggplot2::labs(
      title = "2-HT Model Fit: Observed vs. Expected Counts",
      x = NULL,
      y = "Count"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  return(p)
}


#' Plot Method for winter_2ht Objects
#'
#' Creates a default plot for winter_2ht objects showing parameter estimates.
#'
#' @param x A winter_2ht object
#' @param ... Additional arguments passed to plot_2ht_parameters()
#'
#' @export
plot.winter_2ht <- function(x, ...) {
  plot_2ht_parameters(x, ...)
}
