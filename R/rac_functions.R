# Declare global variables used in ggplot2 aes()
utils::globalVariables("time_numeric")

#' Compute RAC (Response Time-Accuracy Characteristic) Data
#'
#' Computes response time-accuracy data for lineup identifications following
#' the approach described in Seale-Carlisle et al. (2019) and implemented in
#' pyWitness (Mickes et al., 2024). RAC analysis shows the relationship between
#' response time and accuracy for suspect identifications.
#'
#' @param data A dataframe with the following columns:
#'   \itemize{
#'     \item target_present: Logical. TRUE if guilty suspect in lineup
#'     \item identification: Character. "suspect", "filler", or "reject"
#'     \item response_time: Numeric. Response time in milliseconds (or seconds)
#'   }
#' @param lineup_size Integer. Number of people in lineup (default = 6)
#' @param time_bins Numeric vector of bin edges for grouping response times
#'   (e.g., c(0, 5000, 10000, 15000, 20000) creates bins in milliseconds).
#'   If NULL, uses individual response time values (not recommended for
#'   continuous data).
#'
#' @return A list containing:
#'   \itemize{
#'     \item rac_data: Dataframe with response time bins and accuracy
#'     \item n_ids_per_bin: Number of suspect IDs in each time bin
#'     \item overall_accuracy: Overall accuracy across all suspect IDs
#'   }
#'
#' @details
#' RAC analysis computes, for each response time bin:
#' \deqn{Accuracy = \frac{Correct Suspect IDs}{Correct Suspect IDs + Incorrect Suspect IDs}}
#'
#' Explicit target-absent suspect IDs are used when present. Otherwise,
#' target-absent filler IDs are divided by lineup size to estimate incorrect
#' suspect IDs. The two estimators are never added together.
#' The bin mean time uses the same suspect-ID observations as accuracy;
#' estimated-mode TA filler times receive weight \eqn{1/k}. Bin edges must
#' cover all supplied observations. Count accuracy depends on TP/TA sampling
#' fractions and does not adjust to a different population base rate. The
#' displayed binomial SE is approximate, especially for estimated counts.
#'
#' RAC analysis is useful for examining the speed-accuracy tradeoff in
#' eyewitness identifications. Faster responses typically indicate stronger
#' memory, and RAC curves often show higher accuracy for faster response times.
#'
#' @references
#' Seale-Carlisle, T. M., Colloff, M. F., Flowe, H. D., Wells, W., Wixted, J. T.,
#' & Mickes, L. (2019). Confidence and response time as indicators of eyewitness
#' identification accuracy in the lab and in the real world. \emph{Journal of
#' Applied Research in Memory and Cognition, 8}(4), 420-428.
#'
#' Mickes, L., Seale-Carlisle, T. M., Chen, X., & Boogert, S. (2024).
#' pyWitness 1.0: A python eyewitness identification analysis toolkit.
#' \emph{Behavior Research Methods, 56}, 1533-1550.
#'
#' @examples
#' data <- create_example_lineup_data(n_trials = 200,
#'                                    include_response_time = TRUE,
#'                                    seed = 123)
#' rac <- make_racdata(data, time_bins = c(0, 4000, 8000, 12000, 20000))
#' rac$rac_data
#'
#' @inheritParams make_rocdata
#' @export
#' @import tibble
make_racdata <- function(data, lineup_size = 6, time_bins = NULL,
                         innocent_suspect = "auto") {
  .validate_lineup_analysis(data, require_confidence = FALSE,
                            require_response_time = TRUE, lineup_size = lineup_size)
  .validate_bins(data$response_time, time_bins, "time_bins")

  # Validate required columns
  required_cols <- c("target_present", "identification", "response_time")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Separate target-present and target-absent
  tp_data <- data[data$target_present == TRUE, ]
  ta_data <- data[data$target_present == FALSE, ]

  # Apply response time binning
  if (!is.null(time_bins)) {
    data$time_binned <- cut(data$response_time,
                            breaks = time_bins,
                            dig.lab = 10,
                            include.lowest = TRUE,
                            right = TRUE)
    tp_data$time_binned <- cut(tp_data$response_time,
                               breaks = time_bins,
                               dig.lab = 10,
                               include.lowest = TRUE,
                               right = TRUE)
    ta_data$time_binned <- cut(ta_data$response_time,
                               breaks = time_bins,
                               dig.lab = 10,
                               include.lowest = TRUE,
                               right = TRUE)
    time_var <- "time_binned"
  } else {
    time_var <- "response_time"
  }

  # Get unique time levels
  if (!is.null(time_bins)) {
    time_levels <- levels(data$time_binned)
  } else {
    time_levels <- sort(unique(data$response_time), decreasing = FALSE)
  }

  # Initialize results
  innocent_method <- .innocent_suspect_method(ta_data, innocent_suspect)
  rac_results <- tibble::tibble(
    response_time = character(),
    mean_time = numeric(),
    n_correct = numeric(),
    n_incorrect = numeric(),
    n_total = numeric(),
    accuracy = numeric(),
    se = numeric()
  )

  for (time_level in time_levels) {
    # Count correct suspect IDs (target-present, suspect chosen)
    if (!is.null(time_bins)) {
      n_correct <- sum(tp_data$identification == "suspect" &
                      tp_data$time_binned == time_level, na.rm = TRUE)
      # Calculate mean time in this bin
      tp_times <- tp_data$response_time[tp_data$time_binned == time_level &
                                         tp_data$identification == "suspect"]
      ta_category <- if (innocent_method == "designated") "suspect" else "filler"
      ta_times <- ta_data$response_time[ta_data$time_binned == time_level &
                                         ta_data$identification == ta_category]
      ta_weight <- if (innocent_method == "designated") 1 else 1 / lineup_size
      weight_total <- length(tp_times) + ta_weight * length(ta_times)
      mean_time <- if (weight_total > 0)
        (sum(tp_times) + ta_weight * sum(ta_times)) / weight_total else NA_real_
    } else {
      n_correct <- sum(tp_data$identification == "suspect" &
                      tp_data$response_time == time_level, na.rm = TRUE)
      mean_time <- time_level
    }

    # Count incorrect suspect IDs (target-absent, suspect chosen)
    if (!is.null(time_bins)) {
      n_incorrect_suspects <- sum(ta_data$identification == "suspect" &
                                  ta_data$time_binned == time_level, na.rm = TRUE)
      n_incorrect_fillers <- sum(ta_data$identification == "filler" &
                                ta_data$time_binned == time_level, na.rm = TRUE)
    } else {
      n_incorrect_suspects <- sum(ta_data$identification == "suspect" &
                                  ta_data$response_time == time_level, na.rm = TRUE)
      n_incorrect_fillers <- sum(ta_data$identification == "filler" &
                                ta_data$response_time == time_level, na.rm = TRUE)
    }

    n_incorrect_total <- if (innocent_method == "designated") {
      n_incorrect_suspects
    } else {
      n_incorrect_fillers / lineup_size
    }

    # Calculate accuracy
    n_total <- n_correct + n_incorrect_total
    if (n_total > 0) {
      accuracy <- n_correct / n_total
      # Standard error (binomial)
      se <- sqrt((accuracy * (1 - accuracy)) / n_total)
    } else {
      accuracy <- NA
      se <- NA
    }

    rac_results <- rbind(rac_results, tibble::tibble(
      response_time = if (is.factor(time_level)) as.character(time_level) else as.character(time_level),
      mean_time = mean_time,
      n_correct = n_correct,
      n_incorrect = n_incorrect_total,
      n_total = n_total,
      accuracy = accuracy,
      se = se
    ))
  }

  # Calculate overall accuracy
  total_correct <- sum(tp_data$identification == "suspect")
  total_incorrect_suspects <- sum(ta_data$identification == "suspect")
  total_incorrect_fillers <- sum(ta_data$identification == "filler")
  total_incorrect <- if (innocent_method == "designated") {
    total_incorrect_suspects
  } else {
    total_incorrect_fillers / lineup_size
  }
  overall_accuracy <- total_correct / (total_correct + total_incorrect)

  list(
    rac_data = rac_results,
    overall_accuracy = overall_accuracy,
    n_total_suspect_ids = total_correct + total_incorrect,
    lineup_size = lineup_size,
    innocent_suspect_method = innocent_method
  )
}


#' Plot RAC Curve
#'
#' Creates a ggplot2 visualization of RAC data showing the relationship
#' between response time and accuracy.
#'
#' @param racobj_list List output from make_racdata()
#' @param show_errorbars Logical. Whether to show error bars (default = TRUE)
#' @param show_n Logical. Whether to show sample sizes (default = TRUE)
#' @param time_units Character. Label for time units (e.g., "ms", "seconds").
#'   Default = "ms"
#'
#' @return A ggplot2 object
#'
#' @examples
#' data <- create_example_lineup_data(n_trials = 200,
#'                                    include_response_time = TRUE,
#'                                    seed = 123)
#' rac <- make_racdata(data, time_bins = c(0, 4000, 8000, 12000, 20000))
#' make_rac_gg(rac)
#'
#' @export
#' @import ggplot2
make_rac_gg <- function(racobj_list, show_errorbars = TRUE, show_n = TRUE,
                        time_units = "ms") {

  rac_data <- racobj_list$rac_data

  # Create numeric x-axis for plotting
  rac_data$time_numeric <- seq_len(nrow(rac_data))

  # Base plot
  p <- ggplot(rac_data, aes(x = time_numeric, y = accuracy)) +
    geom_line(linewidth = 1, color = "steelblue") +
    geom_point(size = 3, color = "steelblue") +
    theme_bw(base_size = 14) +
    scale_x_continuous(
      breaks = seq_len(nrow(rac_data)),
      labels = rac_data$response_time
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    labs(
      x = paste0("Response Time (", time_units, ")"),
      y = "Proportion Correct (Suspect IDs Only)",
      title = "RAC: Response Time-Accuracy Characteristic",
      caption = "Accuracy = Correct Suspect IDs / (Correct + Incorrect Suspect IDs)"
    ) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Add error bars
  if (show_errorbars) {
    p <- p + geom_errorbar(
      aes(ymin = accuracy - se, ymax = accuracy + se),
      width = 0.2,
      color = "steelblue"
    )
  }

  # Add sample size labels
  if (show_n) {
    p <- p + geom_text(
      aes(label = paste0("n=", round(n_total))),
      nudge_y = -0.08,
      size = 3,
      color = "gray30"
    )
  }

  p
}


#' Compute and Plot RAC for Lineup Identification
#'
#' Main function to compute and plot RAC (Response Time-Accuracy Characteristic)
#' for eyewitness lineup data.
#'
#' @param data A dataframe with columns: target_present, identification, response_time
#' @param lineup_size Integer. Number of people in lineup (default = 6)
#' @param time_bins Numeric vector of bin edges (recommended for continuous RT data)
#' @param show_plot Logical. Whether to display the plot (default = TRUE)
#' @param ... Additional arguments passed to make_rac_gg()
#'
#' @return A list containing RAC data and plot
#'
#' @examples
#' # Example with binned response times (in milliseconds)
#' lineup_data <- create_example_lineup_data(n_trials = 200,
#'                                           include_response_time = TRUE,
#'                                           seed = 123)
#' rac_result <- make_rac(lineup_data,
#'                        time_bins = c(0, 5000, 10000, 15000, 20000))
#' print(rac_result)
#' rac_result$plot
#'
#' @inheritParams make_rocdata
#' @export
make_rac <- function(data, lineup_size = 6, time_bins = NULL,
                     show_plot = TRUE, innocent_suspect = "auto", ...) {

  # Compute RAC data
  rac_obj <- make_racdata(data, lineup_size = lineup_size,
                          time_bins = time_bins,
                          innocent_suspect = innocent_suspect)

  # Create plot
  # The plot is always built, so that it is available as $plot and to the
  # plot() method. show_plot controls only whether it is displayed now:
  # suppressing display should not destroy the object.
  rac_plot <- make_rac_gg(rac_obj, ...)
  if (show_plot) {
    print(rac_plot)
  }

  result <- list(
    plot = rac_plot,
    rac_data = rac_obj$rac_data,
    overall_accuracy = rac_obj$overall_accuracy,
    n_total_suspect_ids = rac_obj$n_total_suspect_ids,
    lineup_size = rac_obj$lineup_size,
    innocent_suspect_method = rac_obj$innocent_suspect_method
  )

  class(result) <- c("lineup_rac", "list")
  result
}


#' Print Method for lineup_rac Objects
#' @param x A lineup_rac object
#' @param ... Additional arguments (ignored)
#' @export
print.lineup_rac <- function(x, ...) {
  cat("\n=== Lineup RAC Analysis ===\n\n")
  cat("Overall Accuracy:", round(x$overall_accuracy, 3), "\n")
  cat("Total Suspect IDs:", round(x$n_total_suspect_ids), "\n")
  cat("Lineup size:", x$lineup_size, "\n\n")

  cat("RAC Data:\n")
  print(x$rac_data, n = Inf)

  if (!is.null(x$plot)) {
    cat("\nPlot: save this object first, then extract it with x$plot\n")
  }

  invisible(x)
}
