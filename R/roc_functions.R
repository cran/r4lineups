#' Compute ROC Data for Lineup Identification
#'
#' Computes Receiver Operating Characteristic (ROC) data from lineup identification
#' experiments following the methodology of Mickes, Wixted, and Gronlund.
#'
#' @param data A dataframe with the following columns:
#'   \itemize{
#'     \item target_present: Logical. TRUE if guilty suspect in lineup, FALSE if innocent
#'     \item identification: Character. "suspect", "filler", or "reject"
#'     \item confidence: Numeric. Confidence rating (higher = more confident)
#'   }
#' @param lineup_size Integer. Number of people in the lineup (default = 6).
#'   Used to estimate false ID rate when fillers are chosen from target-absent lineups.
#' @param innocent_suspect How innocent-suspect identifications in
#'   target-absent lineups are obtained. \code{"designated"}: the lineups had a
#'   designated innocent suspect, so target-absent \code{"suspect"} rows are
#'   counted directly, and zero such rows means a false-ID rate of zero.
#'   \code{"estimated"}: there was no designated innocent suspect, so the
#'   innocent-suspect count is estimated as filler IDs divided by
#'   \code{lineup_size}. \code{"auto"} (the default) chooses
#'   \code{"designated"} when any target-absent row is a suspect ID or the data
#'   have a non-missing \code{suspect_pos} column, and \code{"estimated"}
#'   otherwise, with a message.
#' @param thresholds Optional numeric vector of confidence criteria at which
#'   to evaluate the ROC. The default uses the confidence levels of the
#'   responses that can move an operating point; see Details. Supplying the
#'   criteria of a full sample keeps them fixed across bootstrap resamples.
#'
#' @return A list containing:
#'   \itemize{
#'     \item roc_data: Dataframe with correct_id_rate, false_id_rate, and confidence
#'     \item pauc: Partial area under the curve
#'     \item n_target_present: Number of target-present lineups
#'     \item n_target_absent: Number of target-absent lineups
#'     \item thresholds: The confidence criteria used, highest first
#'   }
#'
#' @details
#' This function computes ROC curves following Wixted & Mickes (2012) and
#' Mickes (2015). For each confidence criterion:
#' \itemize{
#'   \item Correct ID rate = proportion of target-present lineups where suspect identified
#'   \item False ID rate = proportion of target-absent lineups where suspect identified
#' }
#'
#' The criteria are the confidence levels of the responses that enter the
#' ROC: target-present suspect IDs, and target-absent suspect IDs (designated
#' suspect) or filler IDs (estimated). The confidence given to rejections, or
#' to filler IDs in target-present lineups, does not define a criterion,
#' because it cannot move an operating point. Each criterion therefore gives a
#' distinct point, and averages over points (DPP, z-ROC d') do not depend on
#' how rejection confidence was coded.
#'
#' With a designated innocent suspect, target-absent \code{"suspect"}
#' responses are used directly. Without one, filler IDs are divided by lineup
#' size. The two estimators are never added together. See
#' \code{innocent_suspect} for how the design is determined.
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
#' Gronlund, S. D., Wixted, J. T., & Mickes, L. (2014). Evaluating eyewitness
#' identification procedures using receiver operating characteristic analysis.
#' \emph{Current Directions in Psychological Science, 23}(1), 3-10.
#'
#' @examples
#' data(lineup_example)
#' roc <- make_rocdata(lineup_example)
#' roc$roc_data
#' roc$pauc
#'
#' @export
#' @import tibble
make_rocdata <- function(data, lineup_size = 6, innocent_suspect = "auto",
                         thresholds = NULL) {
  .validate_lineup_analysis(data, lineup_size = lineup_size)

  # Validate required columns
  required_cols <- c("target_present", "identification", "confidence")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Separate target-present and target-absent lineups
  tp_data <- data[data$target_present == TRUE, ]
  ta_data <- data[data$target_present == FALSE, ]

  n_tp <- nrow(tp_data)
  n_ta <- nrow(ta_data)

  if (n_tp == 0 | n_ta == 0) {
    stop("Data must include both target-present and target-absent lineups")
  }

  method <- .innocent_suspect_method(ta_data, innocent_suspect)

  # Confidence criteria, from high to low
  conf_levels <- .roc_thresholds(tp_data, ta_data, method, thresholds)

  # Initialize results
  roc_results <- tibble::tibble(
    confidence = numeric(),
    correct_id_rate = numeric(),
    false_id_rate = numeric(),
    n_correct_ids = numeric(),
    n_false_ids = numeric()
  )

  # For each confidence level (cumulative from high to low)
  for (conf in conf_levels) {
    # Target-present: count suspect IDs at or above this confidence
    n_correct <- sum(tp_data$identification == "suspect" & tp_data$confidence >= conf)
    correct_id_rate <- n_correct / n_tp

    n_false <- .innocent_suspect_count(
      ta_data, ta_data$confidence >= conf, lineup_size, method
    )
    false_id_rate <- n_false / n_ta

    roc_results <- rbind(roc_results, tibble::tibble(
      confidence = conf,
      correct_id_rate = correct_id_rate,
      false_id_rate = false_id_rate,
      n_correct_ids = n_correct,
      n_false_ids = n_false
    ))
  }

  # Add point at (0, 0) for reject all
  roc_results <- rbind(tibble::tibble(
    confidence = .reject_all_criterion(tp_data, ta_data, method, conf_levels),
    correct_id_rate = 0,
    false_id_rate = 0,
    n_correct_ids = 0,
    n_false_ids = 0
  ), roc_results)

  # Partial AUC over the observed false-ID range (trapezoidal rule)
  pauc <- .roc_area(roc_results$false_id_rate, roc_results$correct_id_rate)

  out <- list(
    roc_data = roc_results,
    pauc = pauc,
    n_target_present = n_tp,
    n_target_absent = n_ta,
    lineup_size = lineup_size,
    innocent_suspect_method = method,
    thresholds = conf_levels,
    synthetic_origin = 1L,
    raw_data = data
  )
  class(out) <- c("lineup_roc_data", "list")
  attr(out, "data_source") <- deparse(substitute(data))[1]
  out
}


#' Plot ROC Curve for Lineup Identification
#'
#' Creates a ggplot2 visualization of ROC data from lineup identification experiments.
#'
#' @param rocobj_list List output from make_rocdata(), or the lineup_roc object
#'   from make_roc()
#' @param show_pauc Logical. Whether to display pAUC value on plot (default = TRUE)
#' @param point_labels Logical. Whether to label points with confidence levels (default = TRUE)
#' @param title Character. Plot title. The default names the ROC variant, so that
#'   a partial ROC is not mistaken for a full ROC (see \code{\link{plot_fullroc}}).
#' @param data_source Character. Name of the dataset the curve was computed from,
#'   appended to the caption. Defaults to the \code{"data_source"} attribute
#'   recorded by \code{\link{make_rocdata}}, and is omitted when unavailable.
#'
#' @return A ggplot2 object. Being an ordinary ggplot, it can be modified with
#'   the usual \code{+} syntax, e.g. \code{p + ggplot2::theme_minimal()}.
#'
#' @examples
#' data(lineup_example)
#' roc <- make_rocdata(lineup_example)
#' make_roc_gg(roc)
#'
#' @export
#' @import ggplot2 ggrepel
make_roc_gg <- function(rocobj_list, show_pauc = TRUE, point_labels = TRUE,
                        title = "Partial ROC Curve (suspect IDs only)",
                        data_source = NULL) {

  roc_data <- rocobj_list$roc_data
  pauc <- rocobj_list$pauc

  if (is.null(data_source)) {
    data_source <- attr(rocobj_list, "data_source")
  }
  caption <- "Dashed line represents chance performance"
  if (!is.null(data_source) && nzchar(data_source)) {
    caption <- paste0(caption, "\nData: ", data_source)
  }

  # Create base plot
  p <- ggplot(roc_data, aes(x = false_id_rate, y = correct_id_rate)) +
    geom_line(linewidth = 1, color = "steelblue") +
    geom_point(shape = 21, color = "black", fill = "white", size = 3) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    theme_bw(base_size = 14) +
    labs(
      x = "False ID Rate (Innocent Suspect)",
      y = "Correct ID Rate (Guilty Suspect)",
      title = title,
      caption = caption
    ) +
    coord_cartesian(xlim = c(0, max(roc_data$false_id_rate) * 1.1),
                    ylim = c(0, 1))

  # Add confidence labels
  if (point_labels) {
    p <- p + geom_text_repel(
      aes(label = round(confidence, 1)),
      nudge_x = 0.01,
      nudge_y = 0.02,
      size = 3
    )
  }

  # Add shaded area under curve
  p <- p + geom_ribbon(
    aes(ymin = 0, ymax = correct_id_rate),
    alpha = 0.2,
    fill = "steelblue"
  )

  # Add pAUC annotation. A partial area is bounded by the false-ID range it
  # covers, so give that range, the perfect value and chance with it; readers
  # used to the full AUC otherwise expect a value above .5.
  if (show_pauc) {
    max_far <- max(roc_data$false_id_rate)
    p <- p + annotate(
      "text",
      x = 0,
      y = 0.95,
      hjust = 0,
      vjust = 1,
      label = sprintf("pAUC = %.3f over false-ID rates 0 to %.3f\n(perfect %.3f, chance %.3f)",
                      pauc, max_far, max_far, max_far^2 / 2),
      size = 3.5
    )
  }

  p
}


# Area under a cumulative ROC path by the trapezoidal rule, up to max_far.
# Points are ordered by false-ID rate and then by correct-ID rate, so a vertical
# segment keeps both of its endpoints and the next sloping segment starts from
# the right height. The segment that crosses max_far is interpolated linearly.
.roc_area <- function(far, hr, max_far = Inf) {
  ord <- order(far, hr)
  far <- far[ord]
  hr <- hr[ord]
  area <- 0
  for (i in seq_along(far)[-1]) {
    x0 <- far[i - 1]
    if (x0 >= max_far) break
    x1 <- far[i]
    y0 <- hr[i - 1]
    y1 <- hr[i]
    if (x1 > max_far) {
      y1 <- y0 + (y1 - y0) * (max_far - x0) / (x1 - x0)
      x1 <- max_far
    }
    area <- area + (x1 - x0) * (y0 + y1) / 2
  }
  area
}
