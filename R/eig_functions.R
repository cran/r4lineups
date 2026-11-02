#' Compute Shannon Entropy
#'
#' Helper function to compute Shannon entropy for a probability value.
#' Entropy measures uncertainty about guilt/innocence.
#'
#' @param p Probability value (between 0 and 1)
#' @param base Logarithm base (default = 2 for bits)
#'
#' @return Shannon entropy in bits (if base = 2)
#'
#' @details
#' Entropy is computed as: H(p) = -[p*log(p) + (1-p)*log(1-p)]
#' When p is 0 or 1 (certainty), entropy is 0.
#' Maximum entropy (1 bit) occurs at p = 0.5 (maximum uncertainty).
#'
#' @keywords internal
entropy <- function(p, base = 2) {
  if (p <= 0 || p >= 1) {
    return(0)
  }
  -(p * log(p, base = base) + (1 - p) * log(1 - p, base = base))
}


#' Prepare EIG Data from Lineup Identification
#'
#' Prepares response category data for Expected Information Gain (EIG) analysis
#' following Starns et al. (2023). Creates response categories from identification
#' decisions and confidence levels.
#'
#' @param data A dataframe with the following columns:
#'   \itemize{
#'     \item target_present: Logical. TRUE if guilty suspect in lineup
#'     \item identification: Character. "suspect", "filler", or "reject"
#'     \item confidence: Numeric. Confidence rating
#'   }
#' @param confidence_bins Numeric vector of bin edges for grouping confidence
#'   (e.g., c(0, 60, 80, 100) creates bins 0-60, 61-80, 81-100).
#'   If NULL, uses individual confidence levels.
#' @param lineup_size Nominal lineup size. Used to split target-absent filler
#'   identifications when no designated innocent suspect is present.
#'
#' @return A list containing:
#'   \itemize{
#'     \item response_data: Dataframe with response categories and frequencies
#'     \item n_guilty: Total number of target-present lineups
#'     \item n_innocent: Total number of target-absent lineups
#'     \item confidence_bins: The binning specification used (if any)
#'   }
#'
#' @details
#' Response categories are defined as combinations of identification decision
#' (suspect/filler/reject) and confidence level or bin. This function counts
#' the frequency of each response category separately for target-present
#' (guilty) and target-absent (innocent) lineups.
#'
#' @references
#' Starns, J. J., Cohen, A. L., & Rotello, C. M. (2023). A complete method for
#' assessing the effectiveness of eyewitness identification procedures: Expected
#' information gain. \emph{Psychological Review, 130}(3), 677--719.
#'
#' @examples
#' data(lineup_example)
#' eig_data <- make_eig_data(lineup_example, confidence_bins = c(0, 60, 80, 100))
#' eig_data$response_data
#'
#' @inheritParams make_rocdata
#' @export
#' @import tibble
make_eig_data <- function(data, confidence_bins = NULL, lineup_size = 6,
                          innocent_suspect = "auto") {
  .validate_lineup_analysis(data, lineup_size = lineup_size)
  .validate_bins(data$confidence, confidence_bins)
  if (!is.numeric(lineup_size) || length(lineup_size) != 1L ||
      !is.finite(lineup_size) || lineup_size < 2) {
    stop("lineup_size must be a finite number of at least 2.", call. = FALSE)
  }

  # Validate required columns
  required_cols <- c("target_present", "identification", "confidence")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Check that identification has valid values
  valid_ids <- c("suspect", "filler", "reject")
  invalid_ids <- setdiff(unique(data$identification), valid_ids)
  if (length(invalid_ids) > 0) {
    stop("Invalid identification values: ", paste(invalid_ids, collapse = ", "),
         "\nMust be one of: suspect, filler, reject")
  }

  # Apply confidence binning if specified
  if (!is.null(confidence_bins)) {
    data$conf_category <- cut(data$confidence,
                               breaks = confidence_bins,
                               include.lowest = TRUE,
                               right = TRUE)
    conf_var <- "conf_category"
  } else {
    data$conf_category <- factor(data$confidence)
    conf_var <- "conf_category"
  }

  # Separate guilty (target-present) and innocent (target-absent) lineups
  guilty_data <- data[data$target_present == TRUE, ]
  innocent_data <- data[data$target_present == FALSE, ]

  n_guilty <- nrow(guilty_data)
  n_innocent <- nrow(innocent_data)

  if (n_guilty == 0 || n_innocent == 0) {
    stop("Data must include both target-present and target-absent lineups")
  }

  # Create response categories (identification x confidence)
  guilty_data$response_cat <- paste0(guilty_data$identification, "_",
                                     as.character(guilty_data$conf_category))
  innocent_data$response_cat <- paste0(innocent_data$identification, "_",
                                       as.character(innocent_data$conf_category))

  # Count frequencies for each response category
  guilty_counts <- table(guilty_data$response_cat)
  innocent_counts <- table(innocent_data$response_cat)
  innocent_method <- .innocent_suspect_method(innocent_data, innocent_suspect)
  innocent_counts <- .adjust_innocent_response_counts(
    innocent_counts, lineup_size, innocent_method
  )

  # Get all possible response categories
  all_responses <- union(names(guilty_counts), names(innocent_counts))

  # Build response data frame
  response_data <- tibble::tibble(
    response = all_responses,
    n_guilty = as.numeric(guilty_counts[all_responses]),
    n_innocent = as.numeric(innocent_counts[all_responses])
  )

  # Replace NAs with 0 (categories not observed in one condition)
  response_data$n_guilty[is.na(response_data$n_guilty)] <- 0
  response_data$n_innocent[is.na(response_data$n_innocent)] <- 0

  # Compute likelihoods
  response_data$p_x_given_guilty <- response_data$n_guilty / n_guilty
  response_data$p_x_given_innocent <- response_data$n_innocent / n_innocent

  # Extract identification and confidence components for easier interpretation
  split_response <- strsplit(response_data$response, "_", fixed = TRUE)
  response_data$identification <- sapply(split_response, `[`, 1)
  response_data$confidence_level <- sapply(split_response, function(x) {
    paste(x[-1], collapse = "_")
  })

  list(
    response_data = response_data,
    n_guilty = n_guilty,
    n_innocent = n_innocent,
    confidence_bins = confidence_bins,
    lineup_size = lineup_size,
    innocent_suspect_method = innocent_method
  )
}


#' Compute Expected Information Gain (EIG)
#'
#' Computes Expected Information Gain for lineup identification procedures
#' following Starns et al. (2023). EIG quantifies the evidentiary value of
#' witness responses using information theory.
#'
#' @param eig_data List output from make_eig_data(), or a dataframe with columns:
#'   target_present, identification, confidence
#' @param prior_guilt Numeric. Prior probability that suspect is guilty (default = 0.5)
#' @param confidence_bins Numeric vector of bin edges (only used if eig_data is a dataframe)
#' @param lineup_size Nominal lineup size used when raw target-absent data have
#'   no designated innocent suspect.
#'
#' @return A list of class "lineup_eig" containing:
#'   \itemize{
#'     \item eig: Expected Information Gain in bits
#'     \item response_data: Dataframe with information gain for each response category
#'     \item prior_guilt: Prior probability used
#'     \item prior_entropy: Entropy of prior (baseline uncertainty)
#'     \item n_guilty: Number of target-present lineups
#'     \item n_innocent: Number of target-absent lineups
#'   }
#'
#' @details
#' EIG measures how much information (in bits) a witness response provides about
#' guilt vs. innocence. It is computed as:
#'
#' \deqn{EIG = \sum_x p(x) \times [H(prior) - H(p(guilty|x))]}
#'
#' where:
#' \itemize{
#'   \item x = response category (e.g., "suspect_high_confidence")
#'   \item p(x) = probability of that response
#'   \item H() = Shannon entropy
#'   \item p(guilty|x) = posterior probability of guilt given response x
#' }
#'
#' Higher EIG values indicate more diagnostic procedures. EIG = 0 means no
#' information gain (responses don't distinguish guilty from innocent).
#' The maximum EIG is the prior entropy; it is 1 bit only at prior guilt 0.5.
#'
#' @references
#' Starns, J. J., Cohen, A. L., & Rotello, C. M. (2023). A complete method for
#' assessing the effectiveness of eyewitness identification procedures: Expected
#' information gain. \emph{Psychological Review, 130}(3), 677--719.
#'
#' @examples
#' # Compute EIG with binned confidence
#' data(lineup_example)
#' eig_result <- compute_eig(lineup_example,
#'                           prior_guilt = 0.5,
#'                           confidence_bins = c(0, 60, 80, 100))
#' print(eig_result)
#'
#' @inheritParams make_rocdata
#' @export
compute_eig <- function(eig_data, prior_guilt = 0.5, confidence_bins = NULL,
                        lineup_size = 6, innocent_suspect = "auto") {

  # If eig_data is a raw dataframe, prepare it first
  if (is.data.frame(eig_data) && !("response_data" %in% names(eig_data))) {
    eig_data <- make_eig_data(eig_data, confidence_bins = confidence_bins,
                              lineup_size = lineup_size,
                              innocent_suspect = innocent_suspect)
  }

  # Validate prior
  if (prior_guilt <= 0 || prior_guilt >= 1) {
    stop("prior_guilt must be between 0 and 1 (exclusive)")
  }

  response_data <- eig_data$response_data
  n_guilty <- eig_data$n_guilty
  n_innocent <- eig_data$n_innocent

  # Prior entropy (baseline uncertainty)
  prior_entropy <- entropy(prior_guilt, base = 2)

  # Initialize columns for calculations
  response_data$p_response <- NA
  response_data$posterior_guilty <- NA
  response_data$posterior_entropy <- NA
  response_data$information_gain <- NA

  # For each response category
  for (i in seq_len(nrow(response_data))) {
    p_x_g <- response_data$p_x_given_guilty[i]
    p_x_i <- response_data$p_x_given_innocent[i]

    # Probability of this response (law of total probability)
    p_x <- prior_guilt * p_x_g + (1 - prior_guilt) * p_x_i
    response_data$p_response[i] <- p_x

    # Skip if response never occurs
    if (p_x == 0) {
      response_data$posterior_guilty[i] <- NA
      response_data$posterior_entropy[i] <- NA
      response_data$information_gain[i] <- 0
      next
    }

    # Posterior probability of guilt via Bayes' theorem
    posterior <- (prior_guilt * p_x_g) / p_x
    response_data$posterior_guilty[i] <- posterior

    # Posterior entropy
    post_entropy <- entropy(posterior, base = 2)
    response_data$posterior_entropy[i] <- post_entropy

    # Information gain for this response
    ig <- prior_entropy - post_entropy
    response_data$information_gain[i] <- ig
  }

  # Expected Information Gain (weighted average of IG)
  eig_value <- sum(response_data$p_response * response_data$information_gain,
                   na.rm = TRUE)

  # Sort response data by information gain (descending)
  response_data <- response_data[order(-response_data$information_gain), ]

  result <- list(
    eig = eig_value,
    response_data = response_data,
    prior_guilt = prior_guilt,
    prior_entropy = prior_entropy,
    n_guilty = n_guilty,
    n_innocent = n_innocent,
    confidence_bins = eig_data$confidence_bins,
    lineup_size = eig_data$lineup_size,
    innocent_suspect_method = eig_data$innocent_suspect_method
  )

  class(result) <- c("lineup_eig", "list")
  result
}


#' Print Method for lineup_eig Objects
#'
#' @param x A lineup_eig object from compute_eig()
#' @param n_responses Number of response categories to display (default = 10)
#' @param ... Additional arguments (ignored)
#'
#' @export
print.lineup_eig <- function(x, n_responses = 10, ...) {
  cat("\n=== Expected Information Gain (EIG) Analysis ===\n\n")
  cat("EIG:", round(x$eig, 4), "bits\n")
  cat("Prior probability of guilt:", x$prior_guilt, "\n")
  cat("Prior entropy:", round(x$prior_entropy, 4), "bits\n")
  cat("Information efficiency:", round(x$eig / x$prior_entropy * 100, 1), "%\n\n")

  cat("Sample sizes:\n")
  cat("  Target-present (guilty):", x$n_guilty, "\n")
  cat("  Target-absent (innocent):", x$n_innocent, "\n\n")

  if (!is.null(x$confidence_bins)) {
    cat("Confidence bins:", paste(x$confidence_bins, collapse = ", "), "\n\n")
  }

  cat("Top response categories by information gain:\n")
  cat("(Showing up to", n_responses, "of", nrow(x$response_data), "total)\n\n")

  # Select relevant columns for display
  display_data <- x$response_data[1:min(n_responses, nrow(x$response_data)),
                                   c("response", "identification", "confidence_level",
                                     "p_response", "posterior_guilty", "information_gain")]

  # Format for display
  display_data$p_response <- sprintf("%.3f", display_data$p_response)
  display_data$posterior_guilty <- sprintf("%.3f", display_data$posterior_guilty)
  display_data$information_gain <- sprintf("%.4f", display_data$information_gain)

  print(display_data, row.names = FALSE)

  cat("\nFull response data available in $response_data\n")

  invisible(x)
}


#' Summary Method for lineup_eig Objects
#'
#' @param object A lineup_eig object from compute_eig()
#' @param ... Additional arguments (ignored)
#'
#' @export
#' @importFrom utils head
summary.lineup_eig <- function(object, ...) {
  cat("\n=== EIG Summary ===\n\n")
  cat("Expected Information Gain:", round(object$eig, 4), "bits\n")
  cat("Maximum possible IG:", round(object$prior_entropy, 4), "bits\n")
  cat("Information efficiency:", round(object$eig / object$prior_entropy * 100, 1), "%\n\n")

  # Response category statistics
  cat("Response categories:\n")
  cat("  Total unique responses:", nrow(object$response_data), "\n")
  cat("  Avg IG per response:",
      round(mean(object$response_data$information_gain, na.rm = TRUE), 4), "bits\n")
  cat("  Max IG (single response):",
      round(max(object$response_data$information_gain, na.rm = TRUE), 4), "bits\n\n")

  # Most informative response categories
  cat("Most informative responses:\n")
  top_3 <- utils::head(object$response_data, 3)
  for (i in seq_len(nrow(top_3))) {
    cat("  ", i, ". ", top_3$response[i],
        " (IG = ", round(top_3$information_gain[i], 4),
        " bits, p = ", round(top_3$p_response[i], 3), ")\n", sep = "")
  }

  invisible(object)
}


#' Plot Information Gain by Response Category
#'
#' Creates a ggplot2 visualization showing information gain for each response
#' category in an EIG analysis.
#'
#' @param eig_obj A lineup_eig object from compute_eig()
#' @param max_responses Maximum number of response categories to display (default = 15).
#'   Categories are sorted by information gain (highest first).
#' @param color_by Character. Color bars by "identification" decision or "ig" (information gain).
#'   Default = "identification".
#'
#' @return A ggplot2 object
#'
#' @details
#' This function creates a bar plot showing information gain for each response
#' category. Response categories are ordered by IG (descending), with the most
#' informative responses shown first. Colors distinguish different identification
#' decisions (suspect/filler/reject).
#'
#' @examples
#' data(lineup_example)
#' eig_result <- compute_eig(lineup_example, confidence_bins = c(0, 60, 80, 100))
#' plot_eig(eig_result)
#'
#' @export
#' @import ggplot2
#' @importFrom utils head
plot_eig <- function(eig_obj, max_responses = 15, color_by = "identification") {

  if (!inherits(eig_obj, "lineup_eig")) {
    stop("eig_obj must be a lineup_eig object from compute_eig()")
  }

  # Get response data
  plot_data <- eig_obj$response_data

  # Limit to top N responses
  if (nrow(plot_data) > max_responses) {
    plot_data <- utils::head(plot_data, max_responses)
  }

  # Create shorter labels for plotting
  plot_data$response_label <- paste0(
    substr(plot_data$identification, 1, 3),
    "_",
    plot_data$confidence_level
  )

  # Reorder factor levels by information gain
  plot_data$response_label <- factor(plot_data$response_label,
                                      levels = rev(plot_data$response_label))

  # Choose color scale
  if (color_by == "identification") {
    id_colors <- c("suspect" = "#e41a1c", "filler" = "#377eb8", "reject" = "#4daf4a")
    p <- ggplot(plot_data, aes(x = response_label, y = information_gain,
                                fill = identification)) +
      scale_fill_manual(values = id_colors, name = "Decision")
  } else {
    p <- ggplot(plot_data, aes(x = response_label, y = information_gain,
                                fill = information_gain)) +
      scale_fill_gradient(low = "#fee5d9", high = "#a50f15", name = "IG (bits)")
  }

  p <- p +
    geom_col() +
    coord_flip() +
    theme_bw(base_size = 12) +
    labs(
      title = "Information Gain by Response Category",
      subtitle = paste0("EIG = ", round(eig_obj$eig, 4), " bits (",
                       round(eig_obj$eig / eig_obj$prior_entropy * 100, 1),
                       "% efficiency)"),
      x = "Response Category",
      y = "Information Gain (bits)"
    ) +
    geom_hline(yintercept = 0, color = "gray50") +
    theme(legend.position = "right")

  p
}


#' Plot Posterior Probabilities for Response Categories
#'
#' Creates a ggplot2 visualization showing how each response category updates
#' beliefs about guilt (posterior probabilities).
#'
#' @param eig_obj A lineup_eig object from compute_eig()
#' @param max_responses Maximum number of response categories to display (default = 15)
#' @param show_prior Logical. Whether to show prior probability line (default = TRUE)
#'
#' @return A ggplot2 object
#'
#' @details
#' This function creates a bar plot showing the posterior probability of guilt
#' for each response category. The prior probability is shown as a dashed line
#' for reference. Response categories that push beliefs toward guilt (posterior > prior)
#' are colored red, while those pushing toward innocence are colored blue.
#'
#' @examples
#' data(lineup_example)
#' eig_result <- compute_eig(lineup_example, confidence_bins = c(0, 60, 80, 100))
#' plot_eig_posteriors(eig_result)
#'
#' @export
#' @import ggplot2
#' @importFrom utils head
plot_eig_posteriors <- function(eig_obj, max_responses = 15, show_prior = TRUE) {

  if (!inherits(eig_obj, "lineup_eig")) {
    stop("eig_obj must be a lineup_eig object from compute_eig()")
  }

  # Get response data
  plot_data <- eig_obj$response_data

  # Limit to top N responses by IG
  if (nrow(plot_data) > max_responses) {
    plot_data <- utils::head(plot_data, max_responses)
  }

  # Create shorter labels
  plot_data$response_label <- paste0(
    substr(plot_data$identification, 1, 3),
    "_",
    plot_data$confidence_level
  )

  # Reorder by posterior (or could use IG)
  plot_data$response_label <- factor(plot_data$response_label,
                                      levels = plot_data$response_label[order(plot_data$posterior_guilty)])

  # A likelihood ratio of one leaves prior odds unchanged. Compare likelihoods
  # directly so very small prior/posterior probabilities do not hide direction.
  plot_data$evidence_direction <- ifelse(
    plot_data$p_x_given_guilty > plot_data$p_x_given_innocent,
    "Evidence for guilt",
    ifelse(plot_data$p_x_given_guilty < plot_data$p_x_given_innocent,
           "Evidence for innocence", "No directional evidence")
  )

  p <- ggplot(plot_data, aes(x = response_label, y = posterior_guilty,
                              fill = evidence_direction)) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(
      values = c("Evidence for guilt" = "#d7301f", "Evidence for innocence" = "#2171b5",
                 "No directional evidence" = "#777777"),
      name = ""
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    theme_bw(base_size = 12) +
    labs(
      title = "Posterior Probability of Guilt by Response",
      subtitle = paste0("Prior = ", eig_obj$prior_guilt),
      x = "Response Category",
      y = "Posterior P(Guilty | Response)"
    ) +
    theme(legend.position = "bottom")

  if (show_prior) {
    p <- p + geom_hline(yintercept = eig_obj$prior_guilt,
                       linetype = "dashed",
                       color = "gray40",
                       linewidth = 0.8)
  }

  p
}


#' Main Function to Compute and Visualize EIG
#'
#' Convenience wrapper that computes EIG and creates visualizations.
#'
#' @param data A dataframe with columns: target_present, identification, confidence
#' @param prior_guilt Numeric. Prior probability that suspect is guilty (default = 0.5)
#' @param confidence_bins Numeric vector of bin edges (optional)
#' @param lineup_size Nominal lineup size used when no designated innocent
#'   suspect is present.
#' @param show_plot Logical. Whether to create plots (default = TRUE)
#' @param plot_type Character. Which plot to create: "ig" (information gain),
#'   "posteriors" (posterior probabilities), or "both" (default)
#'
#' @return A list of class "lineup_eig" containing:
#'   \itemize{
#'     \item eig: Expected Information Gain value
#'     \item response_data: Full response category data
#'     \item plot_ig: ggplot of information gain (if requested)
#'     \item plot_posteriors: ggplot of posteriors (if requested)
#'     \item ... (additional components from compute_eig)
#'   }
#'
#' @examples
#' data(lineup_example)
#' # With confidence binning
#' result <- make_eig(lineup_example, confidence_bins = c(0, 60, 80, 100))
#'
#' # Access plots
#' result$plot_ig
#' result$plot_posteriors
#'
#' @inheritParams make_rocdata
#' @export
make_eig <- function(data, prior_guilt = 0.5, confidence_bins = NULL,
                     show_plot = TRUE, plot_type = "both", lineup_size = 6,
                     innocent_suspect = "auto") {

  # Compute EIG
  eig_obj <- compute_eig(data, prior_guilt = prior_guilt,
                         confidence_bins = confidence_bins,
                         lineup_size = lineup_size,
                         innocent_suspect = innocent_suspect)

  # Create plots if requested
  plot_ig <- NULL
  plot_posteriors <- NULL

  if (show_plot) {
    if (plot_type %in% c("ig", "both")) {
      plot_ig <- plot_eig(eig_obj)
    }
    if (plot_type %in% c("posteriors", "both")) {
      plot_posteriors <- plot_eig_posteriors(eig_obj)
    }
  }

  # Add plots to result
  eig_obj$plot_ig <- plot_ig
  eig_obj$plot_posteriors <- plot_posteriors

  eig_obj
}
