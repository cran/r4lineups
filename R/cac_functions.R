#' Compute CAC (Confidence-Accuracy Characteristic) Data
#'
#' Computes confidence-accuracy data for lineup identifications following
#' Mickes (2015). CAC analysis shows the relationship between confidence
#' and accuracy for suspect identifications.
#'
#' @param data A dataframe with the following columns:
#'   \itemize{
#'     \item target_present: Logical. TRUE if guilty suspect in lineup
#'     \item identification: Character. "suspect", "filler", or "reject"
#'     \item confidence: Numeric. Confidence rating
#'   }
#' @param lineup_size Integer. Number of people in lineup (default = 6)
#' @param confidence_bins Numeric vector of bin edges for grouping confidence
#'   (e.g., c(0, 60, 80, 100) creates bins 0-60, 61-80, 81-100).
#'   If NULL, uses individual confidence levels.
#' @param base_rate Prior probability that the suspect is guilty, used to
#'   compute accuracy from rates rather than counts. \code{NULL} (default)
#'   keeps the count definition of Mickes (2015). A number strictly between 0
#'   and 1 gives \eqn{\pi HR(c) / (\pi HR(c) + (1 - \pi) FAR(c))}; see Details.
#'
#' @return A list containing:
#'   \itemize{
#'     \item cac_data: Dataframe with confidence levels and accuracy
#'     \item overall_accuracy: Overall accuracy across all suspect IDs
#'     \item n_total_suspect_ids: Correct plus incorrect suspect IDs
#'     \item base_rate: The base rate used, or NULL for count-based accuracy
#'     \item n_target_present, n_target_absent: Sample sizes of the two arms
#'   }
#'
#' @details
#' CAC analysis computes, for each confidence level:
#' \deqn{Accuracy = \frac{Correct Suspect IDs}{Correct Suspect IDs + Incorrect Suspect IDs}}
#'
#' Explicit target-absent suspect IDs are used when present. Otherwise,
#' target-absent filler IDs are divided by lineup size to estimate incorrect
#' suspect IDs. The two estimators are never added together.
#'
#' The count ratio equals the probability of guilt given a suspect ID only
#' when the target-present and target-absent samples are the same size, which
#' Mickes (2015) assumes; she notes that unequal base rates make the
#' calculation more complicated. With unequal samples the count-based
#' accuracy shifts with the TP:TA split even when hit and false-ID rates are
#' unchanged, and the function says so in a message. Setting \code{base_rate}
#' computes accuracy from the hit rate HR(c) = correct IDs / n(TP) and the
#' false-ID rate FAR(c) = incorrect IDs / n(TA) at the stated prior, so that
#' \code{base_rate = 0.5} gives the balanced-design value whatever the sample
#' sizes. Its standard error is then a delta-method approximation from the two
#' independent binomial rates; the count-based standard error is binomial over
#' the bin's suspect IDs.
#'
#' According to Mickes (2015), CAC analysis is most relevant for triers of fact
#' (judges/jurors) evaluating estimator variables (e.g., exposure duration,
#' retention interval). It shows how trustworthy an ID is at different confidence
#' levels.
#'
#' @references
#' Mickes, L. (2015). Receiver operating characteristic analysis and
#' confidence-accuracy characteristic analysis in investigations of system
#' variables and estimator variables that affect eyewitness memory.
#' \emph{Journal of Applied Research in Memory and Cognition, 4}(2), 93-102.
#'
#' Juslin, P., Olsson, N., & Winman, A. (1996). Calibration and diagnosticity
#' of confidence in eyewitness identification: Comments on what can be inferred
#' from the low confidence-accuracy correlation. \emph{Journal of Experimental
#' Psychology: Learning, Memory, and Cognition, 22}(5), 1304-1316.
#'
#' @examples
#' data(lineup_example)
#' cac <- make_cacdata(lineup_example, confidence_bins = c(0, 60, 80, 100))
#' cac$cac_data
#'
#' @inheritParams make_rocdata
#' @export
#' @import tibble
make_cacdata <- function(data, lineup_size = 6, confidence_bins = NULL,
                         innocent_suspect = "auto", base_rate = NULL) {
  .validate_lineup_analysis(data, lineup_size = lineup_size)
  .validate_bins(data$confidence, confidence_bins)
  .check_base_rate(base_rate)

  # Validate required columns
  required_cols <- c("target_present", "identification", "confidence")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Separate target-present and target-absent
  tp_data <- data[data$target_present == TRUE, ]
  ta_data <- data[data$target_present == FALSE, ]

  # Apply confidence binning if specified
  if (!is.null(confidence_bins)) {
    data$conf_binned <- cut(data$confidence,
                            breaks = confidence_bins,
                            include.lowest = TRUE,
                            right = TRUE)
    tp_data$conf_binned <- cut(tp_data$confidence,
                               breaks = confidence_bins,
                               include.lowest = TRUE,
                               right = TRUE)
    ta_data$conf_binned <- cut(ta_data$confidence,
                               breaks = confidence_bins,
                               include.lowest = TRUE,
                               right = TRUE)
    conf_var <- "conf_binned"
  } else {
    conf_var <- "confidence"
  }

  # Get unique confidence levels
  if (!is.null(confidence_bins)) {
    conf_levels <- levels(data$conf_binned)
  } else {
    conf_levels <- sort(unique(data$confidence), decreasing = FALSE)
  }

  # Initialize results
  innocent_method <- .innocent_suspect_method(ta_data, innocent_suspect)
  n_tp <- nrow(tp_data)
  n_ta <- nrow(ta_data)
  if (is.null(base_rate)) {
    .unbalanced_note(n_tp, n_ta)
  }
  cac_results <- tibble::tibble(
    confidence = character(),
    n_correct = numeric(),
    n_incorrect = numeric(),
    n_total = numeric(),
    accuracy = numeric(),
    se = numeric()
  )

  for (conf in conf_levels) {
    # Count correct suspect IDs (target-present, suspect chosen)
    if (!is.null(confidence_bins)) {
      n_correct <- sum(tp_data$identification == "suspect" &
                      tp_data$conf_binned == conf, na.rm = TRUE)
    } else {
      n_correct <- sum(tp_data$identification == "suspect" &
                      tp_data$confidence == conf, na.rm = TRUE)
    }

    # Count incorrect suspect IDs (target-absent, suspect chosen)
    if (!is.null(confidence_bins)) {
      n_incorrect_suspects <- sum(ta_data$identification == "suspect" &
                                  ta_data$conf_binned == conf, na.rm = TRUE)
      n_incorrect_fillers <- sum(ta_data$identification == "filler" &
                                ta_data$conf_binned == conf, na.rm = TRUE)
    } else {
      n_incorrect_suspects <- sum(ta_data$identification == "suspect" &
                                  ta_data$confidence == conf, na.rm = TRUE)
      n_incorrect_fillers <- sum(ta_data$identification == "filler" &
                                ta_data$confidence == conf, na.rm = TRUE)
    }

    n_incorrect_total <- if (innocent_method == "designated") {
      n_incorrect_suspects
    } else {
      n_incorrect_fillers / lineup_size
    }

    # Calculate accuracy
    n_total <- n_correct + n_incorrect_total
    accuracy <- .suspect_id_ppv(n_correct, n_incorrect_total, n_tp, n_ta,
                                base_rate)
    se <- if (is.na(accuracy)) {
      NA
    } else if (is.null(base_rate)) {
      # Standard error (binomial)
      sqrt((accuracy * (1 - accuracy)) / n_total)
    } else {
      .cac_rate_se(n_correct, n_incorrect_suspects, n_incorrect_fillers,
                   n_tp, n_ta, lineup_size, innocent_method, base_rate)
    }

    cac_results <- rbind(cac_results, tibble::tibble(
      confidence = if (is.factor(conf)) as.character(conf) else as.character(conf),
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
  overall_accuracy <- .suspect_id_ppv(total_correct, total_incorrect, n_tp,
                                      n_ta, base_rate)

  out <- list(
    cac_data = cac_results,
    overall_accuracy = overall_accuracy,
    n_total_suspect_ids = total_correct + total_incorrect,
    lineup_size = lineup_size,
    innocent_suspect_method = innocent_method,
    base_rate = base_rate,
    n_target_present = n_tp,
    n_target_absent = n_ta
  )
  class(out) <- c("lineup_cac_data", "list")
  out
}

# Delta-method SE of rate-based accuracy pi*HR / (pi*HR + (1 - pi)*FAR), with
# HR and FAR independent binomial proportions. Without a designated suspect
# FAR is the filler-ID rate divided by lineup size.
.cac_rate_se <- function(n_correct, n_ta_suspect, n_ta_filler, n_tp, n_ta,
                         lineup_size, method, base_rate) {
  if (n_tp == 0 || n_ta == 0) {
    return(NA_real_)
  }
  hr <- n_correct / n_tp
  var_hr <- hr * (1 - hr) / n_tp
  if (method == "designated") {
    far <- n_ta_suspect / n_ta
    var_far <- far * (1 - far) / n_ta
  } else {
    pf <- n_ta_filler / n_ta
    far <- pf / lineup_size
    var_far <- pf * (1 - pf) / n_ta / lineup_size^2
  }
  a <- base_rate * hr
  b <- (1 - base_rate) * far
  d_hr <- base_rate * b / (a + b)^2
  d_far <- -(1 - base_rate) * a / (a + b)^2
  sqrt(d_hr^2 * var_hr + d_far^2 * var_far)
}


#' Plot CAC Curve
#'
#' Creates a ggplot2 visualization of CAC data showing the relationship
#' between confidence and accuracy.
#'
#' @param cacobj_list List output from make_cacdata()
#' @param show_errorbars Logical. Whether to show error bars (default = TRUE)
#' @param show_n Logical. Whether to show sample sizes (default = TRUE)
#'
#' @return A ggplot2 object
#'
#' @examples
#' data(lineup_example)
#' cac <- make_cacdata(lineup_example, confidence_bins = c(0, 60, 80, 100))
#' make_cac_gg(cac)
#'
#' @export
#' @import ggplot2
make_cac_gg <- function(cacobj_list, show_errorbars = TRUE, show_n = TRUE) {

  cac_data <- cacobj_list$cac_data

  # Create numeric x-axis for plotting
  cac_data$conf_numeric <- seq_len(nrow(cac_data))

  # Base plot
  p <- ggplot(cac_data, aes(x = conf_numeric, y = accuracy)) +
    geom_line(linewidth = 1, color = "darkgreen") +
    geom_point(size = 3, color = "darkgreen") +
    theme_bw(base_size = 14) +
    scale_x_continuous(
      breaks = seq_len(nrow(cac_data)),
      labels = cac_data$confidence
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    labs(
      x = "Confidence Level",
      y = "Proportion Correct (Suspect IDs Only)",
      title = "CAC: Confidence-Accuracy Characteristic",
      caption = "Accuracy = Correct Suspect IDs / (Correct + Incorrect Suspect IDs)"
    ) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50")

  # Add error bars
  if (show_errorbars) {
    p <- p + geom_errorbar(
      aes(ymin = accuracy - se, ymax = accuracy + se),
      width = 0.2,
      color = "darkgreen"
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


#' Compute and Plot CAC for Lineup Identification
#'
#' Main function to compute and plot CAC (Confidence-Accuracy Characteristic)
#' for eyewitness lineup data.
#'
#' @param data A dataframe with columns: target_present, identification, confidence
#' @param lineup_size Integer. Number of people in lineup (default = 6)
#' @param confidence_bins Numeric vector of bin edges (optional)
#' @param show_plot Logical. Whether to display the plot (default = TRUE)
#' @param base_rate See \code{\link{make_cacdata}}.
#' @param ... Additional arguments passed to make_cac_gg()
#'
#' @return A list containing CAC data and plot
#'
#' @examples
#' data(lineup_example)
#' cac_result <- make_cac(lineup_example, confidence_bins = c(0, 60, 80, 100))
#' print(cac_result)
#'
#' @inheritParams make_rocdata
#' @export
make_cac <- function(data, lineup_size = 6, confidence_bins = NULL,
                     show_plot = TRUE, innocent_suspect = "auto",
                     base_rate = NULL, ...) {

  # Compute CAC data
  cac_obj <- make_cacdata(data, lineup_size = lineup_size,
                          confidence_bins = confidence_bins,
                          innocent_suspect = innocent_suspect,
                          base_rate = base_rate)

  # Create plot
  # The plot is always built, so that it is available as $plot and to the
  # plot() method. show_plot controls only whether it is displayed now:
  # suppressing display should not destroy the object.
  cac_plot <- make_cac_gg(cac_obj, ...)
  if (show_plot) {
    print(cac_plot)
  }

  result <- list(
    plot = cac_plot,
    cac_data = cac_obj$cac_data,
    overall_accuracy = cac_obj$overall_accuracy,
    n_total_suspect_ids = cac_obj$n_total_suspect_ids,
    lineup_size = cac_obj$lineup_size,
    innocent_suspect_method = cac_obj$innocent_suspect_method,
    base_rate = cac_obj$base_rate,
    n_target_present = cac_obj$n_target_present,
    n_target_absent = cac_obj$n_target_absent
  )

  class(result) <- c("lineup_cac", "list")
  result
}


#' Print Method for lineup_cac Objects
#' @param x A lineup_cac object
#' @param ... Additional arguments (ignored)
#' @export
print.lineup_cac <- function(x, ...) {
  cat("\n=== Lineup CAC Analysis ===\n\n")
  cat("Overall Accuracy:", round(x$overall_accuracy, 3), "\n")
  if (!is.null(x$base_rate)) {
    cat("Computed from rates at base rate:", x$base_rate, "\n")
  }
  cat("Total Suspect IDs:", round(x$n_total_suspect_ids), "\n")
  cat("Lineup size:", x$lineup_size, "\n\n")

  cat("CAC Data:\n")
  print(x$cac_data, n = Inf)

  if (!is.null(x$plot)) {
    cat("\nPlot: use plot(x), or save the object and extract x$plot\n")
  }

  invisible(x)
}
