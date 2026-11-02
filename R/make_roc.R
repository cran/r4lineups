#' Compute and Plot ROC Curve for Lineup Identification
#'
#' Main function to compute and plot an ROC curve for eyewitness lineup data.
#' This follows the methodology of Wixted & Mickes (2012) and Mickes (2015).
#'
#' @param data A dataframe with the following columns:
#'   \itemize{
#'     \item target_present: Logical. TRUE if guilty suspect in lineup
#'     \item identification: Character. "suspect", "filler", or "reject"
#'     \item confidence: Numeric. Confidence rating
#'   }
#' @param lineup_size Integer. Number of people in lineup (default = 6)
#' @param show_plot Logical. Whether to display the plot (default = TRUE)
#' @inheritParams make_rocdata
#' @param ... Additional arguments passed to make_roc_gg()
#'
#' @return A list containing:
#'   \itemize{
#'     \item plot: ggplot2 object. Always returned, whatever show_plot is
#'       set to; show_plot controls only whether it is displayed immediately
#'     \item roc_data: Dataframe with ROC curve points
#'     \item pauc: Partial area under the curve
#'     \item summary: Summary statistics
#'   }
#'
#' @details
#' ROC analysis measures discriminability - the ability to distinguish innocent
#' from guilty suspects. According to Mickes (2015), ROC analysis is most relevant
#' for policymakers deciding on system variables (e.g., simultaneous vs. sequential
#' lineups, lineup size, etc.).
#'
#' For estimator variables (e.g., exposure duration, retention interval), use
#' CAC analysis instead (see \code{\link{make_cac}}).
#'
#' @examples
#' data(lineup_example)
#' roc_result <- make_roc(lineup_example)
#' print(roc_result$pauc)
#'
#' @references
#' Wixted, J. T., & Mickes, L. (2012). The field of eyewitness memory should
#' abandon probative value and embrace receiver operating characteristic analysis.
#' \emph{Perspectives on Psychological Science, 7}(3), 275-278.
#'
#' Mickes, L. (2015). Receiver operating characteristic analysis and
#' confidence-accuracy characteristic analysis in investigations of system
#' variables and estimator variables that affect eyewitness memory.
#' \emph{Journal of Applied Research in Memory and Cognition, 4}(2), 93-102.
#'
#' @export
make_roc <- function(data, lineup_size = 6, show_plot = TRUE,
                     innocent_suspect = "auto", ...) {

  # Compute ROC data. make_rocdata() records a data-source label from its own
  # call, which here would just be the formal name "data", so overwrite it with
  # the expression this function was called with.
  roc_obj <- make_rocdata(data, lineup_size = lineup_size,
                          innocent_suspect = innocent_suspect)
  attr(roc_obj, "data_source") <- deparse(substitute(data))[1]

  # Create plot
  # The plot is always built, so that it is available as $plot and to the
  # plot() method. show_plot controls only whether it is displayed now:
  # suppressing display should not destroy the object.
  roc_plot <- make_roc_gg(roc_obj, ...)
  if (show_plot) {
    print(roc_plot)
  }

  # Create summary
  summary_stats <- list(
    pauc = roc_obj$pauc,
    n_target_present = roc_obj$n_target_present,
    n_target_absent = roc_obj$n_target_absent,
    lineup_size = roc_obj$lineup_size,
    n_confidence_levels = nrow(roc_obj$roc_data) - 1,  # Exclude (0,0) point
    max_correct_id_rate = max(roc_obj$roc_data$correct_id_rate),
    max_false_id_rate = max(roc_obj$roc_data$false_id_rate)
  )

  result <- list(
    plot = roc_plot,
    roc_data = roc_obj$roc_data,
    pauc = roc_obj$pauc,
    summary = summary_stats
  )

  class(result) <- c("lineup_roc", "list")
  attr(result, "data_source") <- attr(roc_obj, "data_source")
  result
}


#' Print Method for lineup_roc Objects
#' @param x A lineup_roc object
#' @param ... Additional arguments (ignored)
#' @export
print.lineup_roc <- function(x, ...) {
  cat("\n=== Lineup ROC Analysis ===\n\n")
  cat("Partial AUC:", round(x$pauc, 3), "\n")
  cat("Target-present trials (N):", x$summary$n_target_present, "\n")
  cat("Target-absent trials (N):", x$summary$n_target_absent, "\n")
  cat("Lineup size:", x$summary$lineup_size, "\n")
  cat("Confidence levels:", x$summary$n_confidence_levels, "\n\n")

  cat("ROC Data:\n")
  print(x$roc_data, n = Inf)

  if (!is.null(x$plot)) {
    cat("\nPlot: use plot(x), or save the object and extract x$plot\n")
  }

  invisible(x)
}
