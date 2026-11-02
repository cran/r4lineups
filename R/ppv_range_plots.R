#' Plot PPV Range Across Confidence Levels
#'
#' Creates a ggplot visualization showing the range of PPV estimates across
#' the three correction methods (nominal, effective, none) as recommended by
#' Fitzgerald et al. (2023).
#'
#' @param ppv_range_obj A lineup_ppv_range object from ppv_range_by_confidence()
#' @param show_band Logical. Whether to show shaded uncertainty band between
#'   nominal and none estimates (default = TRUE)
#' @param show_points Logical. Whether to show points at each confidence level
#'   (default = TRUE)
#'
#' @return A ggplot2 object
#'
#' @details
#' The plot shows three curves:
#' \itemize{
#'   \item \strong{Nominal} (blue, dashed): Best-case estimate assuming fair lineup
#'   \item \strong{Effective} (red, solid): Size-based approximation accounting for uneven choices
#'   \item \strong{None} (gray, dashed): Worst-case estimate (no correction)
#' }
#'
#' The shaded band between nominal and none represents the PPV uncertainty range
#' due to unknown lineup fairness conditions. The effective size estimate typically
#' falls within this range and is a size-based approximation, not a validated best estimate.
#'
#' @references
#' Fitzgerald, R. J., Tredoux, C. G., & Juncu, S. (2023). Estimation of
#' eyewitness error rates in fair and biased lineups.
#' \emph{Law and Human Behavior}.
#'
#' @examples
#' data(lineup_example)
#' ppv_range <- ppv_range_by_confidence(lineup_example,
#'                                      confidence_bins = c(0, 60, 80, 100))
#' plot_ppv_range(ppv_range)
#'
#' @export
#' @import ggplot2
plot_ppv_range <- function(ppv_range_obj,
                            show_band = TRUE,
                            show_points = TRUE) {

  if (!inherits(ppv_range_obj, "lineup_ppv_range")) {
    stop("ppv_range_obj must be a lineup_ppv_range object")
  }

  plot_data <- ppv_range_obj$ppv_range_data

  # Create numeric x-axis
  plot_data$conf_numeric <- seq_len(nrow(plot_data))

  # Reshape for ggplot2
  plot_data_long <- data.frame(
    conf_numeric = rep(plot_data$conf_numeric, 3),
    confidence = rep(plot_data$confidence, 3),
    ppv = c(plot_data$ppv_nominal, plot_data$ppv_effective, plot_data$ppv_none),
    correction = rep(c("Nominal (best-case)", "Effective (size-based)", "None (worst-case)"),
                     each = nrow(plot_data))
  )

  # Base plot
  p <- ggplot(plot_data, aes(x = conf_numeric))

  # Add uncertainty band
  if (show_band) {
    p <- p + geom_ribbon(aes(ymin = ppv_none, ymax = ppv_nominal),
                        fill = "gray80", alpha = 0.3)
  }

  # Add lines for each correction method
  p <- p +
    geom_line(data = plot_data_long,
             aes(x = conf_numeric, y = ppv, color = correction, linetype = correction),
             linewidth = 1) +
    scale_color_manual(
      values = c("Nominal (best-case)" = "#2171b5",
                "Effective (size-based)" = "#d7301f",
                "None (worst-case)" = "#636363"),
      name = "Correction Method"
    ) +
    scale_linetype_manual(
      values = c("Nominal (best-case)" = "dashed",
                "Effective (size-based)" = "solid",
                "None (worst-case)" = "dashed"),
      name = "Correction Method"
    )

  # Add points
  if (show_points) {
    p <- p + geom_point(data = plot_data_long,
                       aes(x = conf_numeric, y = ppv, color = correction),
                       size = 2)
  }

  # Formatting
  p <- p +
    scale_x_continuous(
      breaks = plot_data$conf_numeric,
      labels = plot_data$confidence
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    theme_bw(base_size = 12) +
    labs(
      title = "PPV Range Across Lineup Fairness Assumptions",
      subtitle = paste0("Lineup size = ", ppv_range_obj$lineup_size,
                       " | Band = fairness sensitivity, not a confidence interval"),
      x = "Confidence Level",
      y = "Positive Predictive Value (PPV)",
      caption = "Fitzgerald, Tredoux & Juncu (2023)"
    ) +
    geom_hline(yintercept = 0.5, linetype = "dotted", color = "gray50") +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal"
    )

  p
}


#' Plot Effective Size by Confidence Level
#'
#' Creates a ggplot visualization showing how effective lineup size varies
#' across confidence levels.
#'
#' @param ppv_range_obj A lineup_ppv_range object from ppv_range_by_confidence(),
#'   or a dataframe with columns: confidence, effective_size
#' @param show_nominal Logical. Whether to show nominal lineup size as reference
#'   line (default = TRUE)
#' @param show_points Logical. Whether to show points at each confidence level
#'   (default = TRUE)
#'
#' @return A ggplot2 object
#'
#' @details
#' Effective size < nominal size indicates lineup bias, where some members are
#' implausible and rarely chosen. This can vary by confidence level if witnesses
#' at different confidence levels have different lineup viewing strategies.
#'
#' A horizontal line at the nominal lineup size provides a reference. Values
#' below this line indicate bias; values at or above indicate fair lineups.
#'
#' @examples
#' data(lineup_example)
#' ppv_range <- ppv_range_by_confidence(lineup_example,
#'                                      confidence_bins = c(0, 60, 80, 100))
#' plot_effective_size_conf(ppv_range)
#'
#' @export
#' @import ggplot2
plot_effective_size_conf <- function(ppv_range_obj,
                                      show_nominal = TRUE,
                                      show_points = TRUE) {

  # Handle different input types
  if (inherits(ppv_range_obj, "lineup_ppv_range")) {
    plot_data <- ppv_range_obj$ppv_range_data
    lineup_size <- ppv_range_obj$lineup_size
  } else if (is.data.frame(ppv_range_obj)) {
    plot_data <- ppv_range_obj
    lineup_size <- NULL
  } else {
    stop("Input must be a lineup_ppv_range object or dataframe")
  }

  # Validate required columns
  if (!all(c("confidence", "effective_size") %in% names(plot_data))) {
    stop("Data must have columns: confidence, effective_size")
  }
  # Bins with no target-absent trials have no error rate to show
  if ("error_rate" %in% names(plot_data)) {
    plot_data <- plot_data[!is.na(plot_data$error_rate), , drop = FALSE]
  }

  # Create numeric x-axis
  plot_data$conf_numeric <- seq_len(nrow(plot_data))

  # Base plot
  p <- ggplot(plot_data, aes(x = conf_numeric, y = effective_size))

  # Add nominal lineup size reference line
  if (show_nominal && !is.null(lineup_size)) {
    p <- p + geom_hline(yintercept = lineup_size,
                       linetype = "dashed",
                       color = "gray50",
                       linewidth = 0.8) +
      annotate("text",
              x = nrow(plot_data) * 0.05,
              y = lineup_size * 1.05,
              label = paste("Nominal size =", lineup_size),
              hjust = 0,
              color = "gray40",
              size = 3)
  }

  # Add line and points
  p <- p + geom_line(color = "#d7301f", linewidth = 1)

  if (show_points) {
    p <- p + geom_point(color = "#d7301f", size = 3)
  }

  # Formatting
  p <- p +
    scale_x_continuous(
      breaks = plot_data$conf_numeric,
      labels = plot_data$confidence
    ) +
    theme_bw(base_size = 12) +
    labs(
      title = "Effective Lineup Size by Confidence Level",
      subtitle = "Sample estimates reflect both selection patterns and sampling variation",
      x = "Confidence Level",
      y = "Effective Size",
      caption = "Based on Tredoux (1998) effective size"
    )

  p
}


#' Plot Error Rate by Confidence Level
#'
#' Creates a ggplot visualization showing the mistaken identification rate
#' (from target-absent lineups) across confidence levels.
#'
#' @param ppv_range_obj A lineup_ppv_range object from ppv_range_by_confidence(),
#'   or a dataframe with columns: confidence, error_rate
#' @param show_points Logical. Whether to show points at each confidence level
#'   (default = TRUE)
#'
#' @return A ggplot2 object
#'
#' @details
#' The error rate is computed from target-absent (culprit-absent) lineups as:
#' (suspect IDs + filler IDs) / total target-absent trials
#'
#' Higher confidence should generally be associated with lower error rates
#' (better calibration).
#'
#' @examples
#' data(lineup_example)
#' ppv_range <- ppv_range_by_confidence(lineup_example,
#'                                      confidence_bins = c(0, 60, 80, 100))
#' plot_error_rate_conf(ppv_range)
#'
#' @export
#' @import ggplot2
plot_error_rate_conf <- function(ppv_range_obj, show_points = TRUE) {

  # Handle different input types
  if (inherits(ppv_range_obj, "lineup_ppv_range")) {
    plot_data <- ppv_range_obj$ppv_range_data
  } else if (is.data.frame(ppv_range_obj)) {
    plot_data <- ppv_range_obj
  } else {
    stop("Input must be a lineup_ppv_range object or dataframe")
  }

  # Validate required columns
  if (!all(c("confidence", "error_rate") %in% names(plot_data))) {
    stop("Data must have columns: confidence, error_rate")
  }
  # Bins with no target-absent trials have no error rate to show
  if ("error_rate" %in% names(plot_data)) {
    plot_data <- plot_data[!is.na(plot_data$error_rate), , drop = FALSE]
  }

  # Create numeric x-axis
  plot_data$conf_numeric <- seq_len(nrow(plot_data))

  # Base plot
  p <- ggplot(plot_data, aes(x = conf_numeric, y = error_rate)) +
    geom_line(color = "#636363", linewidth = 1)

  if (show_points) {
    p <- p + geom_point(color = "#636363", size = 3)
  }

  # Formatting
  p <- p +
    scale_x_continuous(
      breaks = plot_data$conf_numeric,
      labels = plot_data$confidence
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    theme_bw(base_size = 12) +
    labs(
      title = "Mistaken ID Rate by Confidence Level",
      subtitle = "From target-absent (culprit-absent) lineups",
      x = "Confidence Level",
      y = "Error Rate (Mistaken ID Rate)"
    )

  p
}


#' Main Function to Compute and Visualize PPV Range
#'
#' Convenience wrapper that computes PPV range and creates visualizations.
#'
#' @param data A dataframe with columns: target_present, identification, confidence
#' @param lineup_size Integer. Nominal number of people in lineup (default = 6)
#' @param confidence_bins Numeric vector of bin edges (optional)
#' @param show_plots Logical. Whether to create plots (default = TRUE)
#' @param plot_type Character. Which plots to create: "ppv" (PPV range only),
#'   "effective" (effective size only), "all" (all plots, default)
#'
#' @return A list of class "lineup_ppv_range" containing:
#'   \itemize{
#'     \item ppv_range_data: Dataframe with all PPV estimates
#'     \item plot_ppv_range: ggplot of PPV range (if requested)
#'     \item plot_effective_size: ggplot of effective size (if requested)
#'     \item plot_error_rate: ggplot of error rates (if requested)
#'     \item ... (additional components from ppv_range_by_confidence)
#'   }
#'
#' @examples
#' data(lineup_example)
#' # With confidence binning
#' result <- make_ppv_range(lineup_example, confidence_bins = c(0, 60, 80, 100))
#'
#' # Access plots
#' result$plot_ppv_range
#' result$plot_effective_size
#'
#' @inheritParams make_rocdata
#' @inheritParams ppv_by_confidence
#' @export
make_ppv_range <- function(data,
                            lineup_size = 6,
                            confidence_bins = NULL,
                            show_plots = TRUE,
                            plot_type = "all",
                            innocent_suspect = "auto",
                            base_rate = NULL) {

  # Compute PPV range
  ppv_obj <- ppv_range_by_confidence(data, lineup_size, confidence_bins,
                                     innocent_suspect = innocent_suspect,
                                     base_rate = base_rate)

  # Create plots if requested
  plot_ppv <- NULL
  plot_eff_size <- NULL
  plot_error <- NULL

  if (show_plots) {
    if (plot_type %in% c("ppv", "all")) {
      plot_ppv <- plot_ppv_range(ppv_obj)
    }
    if (plot_type %in% c("effective", "all")) {
      plot_eff_size <- plot_effective_size_conf(ppv_obj)
    }
    if (plot_type %in% c("all")) {
      plot_error <- plot_error_rate_conf(ppv_obj)
    }
  }

  # Add plots to result
  ppv_obj$plot_ppv_range <- plot_ppv
  ppv_obj$plot_effective_size <- plot_eff_size
  ppv_obj$plot_error_rate <- plot_error

  ppv_obj
}
