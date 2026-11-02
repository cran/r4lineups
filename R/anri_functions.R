#' Compute ANRI (Adjusted Normalized Resolution Index)
#'
#' Computes ANRI, a bias-corrected version of NRI following Yaniv et al. (1991).
#' ANRI adjusts NRI for the number of confidence bins and sample size, providing
#' more accurate estimates especially when J (bins) is small relative to N.
#'
#' @param data A dataframe with the following columns:
#'   \itemize{
#'     \item target_present: Logical. TRUE if guilty suspect in lineup
#'     \item identification: Character. "suspect", "filler", or "reject"
#'     \item confidence: Numeric. Confidence rating
#'   }
#' @param confidence_bins Numeric vector of bin edges (required for ANRI)
#' @param choosers_only Logical. Whether to analyze only suspect IDs (default = TRUE)
#' @param lineup_size Integer. Lineup size (default = 6)
#' @param confidence_scale How the confidence scale is interpreted: "auto" (default),
#'   "0-1", or "0-100". See \code{\link{make_calibration_data}}.
#'
#' @return A list containing:
#'   \itemize{
#'     \item anri: Adjusted Normalized Resolution Index
#'     \item nri: Original Normalized Resolution Index (unadjusted)
#'     \item n_total: Total sample size
#'     \item n_bins: Number of confidence bins
#'     \item calibration_data: Per-bin accuracy and sample sizes
#'     \item overall_accuracy: Mean accuracy
#'   }
#'
#' @details
#' ANRI corrects NRI for small-sample and small-bin-count bias:
#'
#' \deqn{NRI = \frac{\frac{1}{N} \sum_{j=1}^{J} n_j (a_j - \bar{a})^2}{\bar{a}(1-\bar{a})}}
#'
#' \deqn{ANRI = \frac{N \cdot NRI - J + 1}{N - J + 1}}
#'
#' Where:
#' \itemize{
#'   \item N = total sample size
#'   \item J = number of bins
#'   \item n_j = sample size in bin j
#'   \item a_j = accuracy (proportion correct) in bin j
#'   \item a_bar = overall accuracy
#' }
#'
#' **When to use ANRI vs NRI:**
#' \itemize{
#'   \item Use ANRI when J is small (e.g., 3-5 bins)
#'   \item Use ANRI when N is modest (e.g., < 200)
#'   \item Use ANRI when comparing across different numbers of bins
#'   \item NRI is asymptotically unbiased as N approaches infinity
#' }
#'
#' ANRI provides a less biased estimate of the population resolution, making it
#' more appropriate for hypothesis testing and group comparisons.
#' When target-absent suspect identifications are estimated from filler choices,
#' \code{N} and the bin counts are effective (possibly fractional) counts from
#' \code{make_calibration_data()}, so the correction is an approximation. See
#' \code{\link{make_calibration_data}} for the designated-suspect distinction.
#'
#' @references
#' Yaniv, I., Yates, J. F., & Smith, J. E. K. (1991). Measures of discrimination
#' skill in probabilistic judgment. \emph{Psychological Bulletin, 110}(3), 611-617.
#'
#' Juslin, P., Olsson, N., & Winman, A. (1996). Calibration and diagnosticity
#' of confidence in eyewitness identification. \emph{Journal of Experimental
#' Psychology: Learning, Memory, and Cognition, 22}(5), 1304-1316.
#'
#' @examples
#' set.seed(123)
#' n <- 200
#' data <- data.frame(
#'   target_present = rep(c(TRUE, FALSE), each = n / 2),
#'   identification = sample(c("suspect", "filler", "reject"), n,
#'                           replace = TRUE, prob = c(0.5, 0.25, 0.25)),
#'   confidence = round(runif(n, 0, 100))
#' )
#' result <- compute_anri(data, confidence_bins = seq(0, 100, 20))
#' result$anri
#'
#' @inheritParams make_rocdata
#' @export
#' @import tibble
compute_anri <- function(data,
                        confidence_bins,
                        choosers_only = TRUE,
                        lineup_size = 6,
                        confidence_scale = c("auto", "0-1", "0-100"),
                        innocent_suspect = "auto") {

  confidence_scale <- match.arg(confidence_scale)

  if (is.null(confidence_bins)) {
    stop("confidence_bins must be provided for ANRI calculation")
  }

  # Use existing calibration function to get NRI
  cal_obj <- make_calibration_data(
    data,
    confidence_bins = confidence_bins,
    choosers_only = choosers_only,
    lineup_size = lineup_size,
    confidence_scale = confidence_scale,
    innocent_suspect = innocent_suspect
  )

  N <- cal_obj$n_total
  J <- nrow(cal_obj$calibration_data)
  NRI <- cal_obj$NRI

  # Compute ANRI (bias-corrected NRI)
  if (N <= J) {
    warning("Sample size (N) must be greater than number of bins (J). Cannot compute ANRI.")
    ANRI <- NA
  } else {
    ANRI <- (N * NRI - J + 1) / (N - J + 1)
  }

  list(
    anri = ANRI,
    nri = NRI,
    n_total = N,
    n_bins = J,
    calibration_data = cal_obj$calibration_data,
    overall_accuracy = cal_obj$overall_accuracy,
    overall_confidence = cal_obj$overall_confidence,
    choosers_only = choosers_only
  )
}


#' Bootstrap Confidence Intervals for ANRI
#'
#' Computes bootstrap confidence intervals around ANRI estimates using
#' percentile method.
#'
#' @param data Dataframe with standard lineup format
#' @param confidence_bins Numeric vector of bin edges
#' @param choosers_only Logical. Whether to analyze only suspect IDs (default = TRUE)
#' @param lineup_size Integer. Lineup size (default = 6)
#' @param confidence_scale How the confidence scale is interpreted: "auto" (default),
#'   "0-1", or "0-100". See \code{\link{make_calibration_data}}.
#' @param n_bootstrap Integer. Number of bootstrap replications (default = 1000)
#' @param conf_level Numeric. Confidence level for CIs (default = 0.95)
#' @param seed Integer. Random seed for reproducibility (default = NULL)
#'
#' @return A list containing:
#'   \itemize{
#'     \item anri: Point estimate of ANRI
#'     \item nri: Point estimate of NRI
#'     \item ci_lower: Lower bound of bootstrap CI
#'     \item ci_upper: Upper bound of bootstrap CI
#'     \item conf_level: Confidence level used
#'     \item n_bootstrap: Number of bootstrap replications
#'     \item bootstrap_distribution: Vector of bootstrap ANRI values
#'   }
#'
#' @details
#' Bootstrap resampling procedure:
#' 1. Resample observations with replacement
#' 2. Compute ANRI for each bootstrap sample
#' 3. Construct percentile-based confidence interval
#'
#' The percentile method uses the empirical quantiles of the bootstrap
#' distribution. For a 95% CI, we use the 2.5th and 97.5th percentiles.
#'
#' @references
#' Efron, B., & Tibshirani, R. J. (1994). \emph{An Introduction to the Bootstrap}.
#' Chapman & Hall/CRC.
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' n <- 200
#' data <- data.frame(
#'   target_present = rep(c(TRUE, FALSE), each = n / 2),
#'   identification = sample(c("suspect", "filler", "reject"), n,
#'                           replace = TRUE, prob = c(0.5, 0.25, 0.25)),
#'   confidence = round(runif(n, 0, 100))
#' )
#' boot_result <- bootstrap_anri(data, confidence_bins = seq(0, 100, 20),
#'                               n_bootstrap = 200, seed = 1)
#' c(boot_result$ci_lower, boot_result$ci_upper)
#' }
#'
#' @inheritParams make_rocdata
#' @export
bootstrap_anri <- function(data,
                          confidence_bins,
                          choosers_only = TRUE,
                          lineup_size = 6,
                          n_bootstrap = 1000,
                          conf_level = 0.95,
                          seed = NULL,
                          confidence_scale = c("auto", "0-1", "0-100"),
                          innocent_suspect = "auto") {

  confidence_scale <- match.arg(confidence_scale)
  # Resolve the design once, so every resample uses the same one
  if (choosers_only) {
    innocent_suspect <- .resolve_innocent_suspect(data, innocent_suspect)
  }

  # Set seed for reproducibility if provided
  restore_rng <- .local_seed(seed)
  on.exit(restore_rng(), add = TRUE)

  # Compute point estimate
  anri_point <- compute_anri(data, confidence_bins, choosers_only, lineup_size,
                             confidence_scale = confidence_scale,
                             innocent_suspect = innocent_suspect)

  # Bootstrap function
  bootstrap_anri_once <- function(indices) {
    boot_data <- data[indices, ]
    tryCatch({
      boot_anri <- compute_anri(boot_data, confidence_bins, choosers_only, lineup_size,
                                confidence_scale = confidence_scale,
                                innocent_suspect = innocent_suspect)
      boot_anri$anri
    }, error = function(e) {
      NA_real_
    })
  }

  # Generate bootstrap samples
  n_obs <- nrow(data)
  bootstrap_values <- replicate(n_bootstrap, {
    # TP and TA totals are fixed by the experimental design.
    strata <- split(seq_len(n_obs), as.logical(data$target_present))
    indices <- unlist(lapply(strata, .resample_values),
                      use.names = FALSE)
    bootstrap_anri_once(indices)
  })

  valid <- is.finite(bootstrap_values)
  if (any(!valid)) {
    warning(sprintf("%d/%d bootstrap samples were undefined or failed; reported intervals condition on the %d successful fits.",
                    sum(!valid), n_bootstrap, sum(valid)), call. = FALSE)
  }

  # Compute percentile CIs
  alpha <- 1 - conf_level
  ci_lower <- if (sum(valid) >= 2L) quantile(bootstrap_values[valid], alpha / 2) else NA_real_
  ci_upper <- if (sum(valid) >= 2L) quantile(bootstrap_values[valid], 1 - alpha / 2) else NA_real_

  list(
    anri = anri_point$anri,
    nri = anri_point$nri,
    ci_lower = as.numeric(ci_lower),
    ci_upper = as.numeric(ci_upper),
    conf_level = conf_level,
    n_bootstrap = sum(valid),
    n_bootstrap_requested = n_bootstrap,
    n_failed = sum(!valid),
    n_total = anri_point$n_total,
    n_bins = anri_point$n_bins,
    bootstrap_distribution = bootstrap_values
  )
}


#' Compare ANRI Between Groups with Bootstrap
#'
#' Computes ANRI for two groups and tests whether they differ using
#' bootstrap confidence intervals for the difference.
#'
#' @param data Dataframe with standard lineup format plus a grouping variable
#' @param group_var Character. Name of grouping variable (must have exactly 2 levels)
#' @param confidence_bins Numeric vector of bin edges
#' @param choosers_only Logical. Whether to analyze only suspect IDs (default = TRUE)
#' @param lineup_size Integer. Lineup size (default = 6)
#' @param confidence_scale How the confidence scale is interpreted: "auto" (default),
#'   "0-1", or "0-100". See \code{\link{make_calibration_data}}.
#' @param n_bootstrap Integer. Number of bootstrap replications (default = 1000)
#' @param conf_level Numeric. Confidence level for CIs (default = 0.95)
#' @param seed Integer. Random seed for reproducibility (default = NULL)
#'
#' @return A list containing:
#'   \itemize{
#'     \item anri_group1: ANRI for first group
#'     \item anri_group2: ANRI for second group
#'     \item difference: Point estimate of difference (group1 - group2)
#'     \item ci_lower: Lower bound of CI for difference
#'     \item ci_upper: Upper bound of CI for difference
#'     \item significant: Whether difference is significant (CI excludes 0)
#'     \item group_names: Names of the two groups
#'     \item bootstrap_results_group1: Bootstrap object for group 1
#'     \item bootstrap_results_group2: Bootstrap object for group 2
#'   }
#'
#' @details
#' This function:
#' 1. Computes ANRI separately for each group
#' 2. Bootstraps each group independently
#' 3. Computes bootstrap distribution of the difference
#' 4. Tests H0: ANRI_1 = ANRI_2 using CI for difference
#'
#' **Interpretation**:
#' \itemize{
#'   \item Positive difference: Group 1 has higher resolution
#'   \item Negative difference: Group 2 has higher resolution
#'   \item CI excludes 0: Statistically significant difference
#' }
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' n <- 300
#' data <- data.frame(
#'   target_present = rep(c(TRUE, FALSE), each = n / 2),
#'   identification = sample(c("suspect", "filler", "reject"), n,
#'                           replace = TRUE, prob = c(0.5, 0.25, 0.25)),
#'   confidence = round(runif(n, 0, 100)),
#'   condition = rep(c("sequential", "simultaneous"), times = n / 2)
#' )
#' cmp <- compare_anri(data, group_var = "condition",
#'                     confidence_bins = seq(0, 100, 20),
#'                     n_bootstrap = 100, seed = 1)
#' cmp$difference
#' c(cmp$ci_lower, cmp$ci_upper)
#' }
#'
#' @inheritParams make_rocdata
#' @export
#' @import tibble
compare_anri <- function(data,
                        group_var,
                        confidence_bins,
                        choosers_only = TRUE,
                        lineup_size = 6,
                        n_bootstrap = 1000,
                        conf_level = 0.95,
                        seed = NULL,
                        confidence_scale = c("auto", "0-1", "0-100"),
                        innocent_suspect = "auto") {

  confidence_scale <- match.arg(confidence_scale)

  # Validate group variable
  if (!group_var %in% names(data)) {
    stop(sprintf("Group variable '%s' not found in data", group_var))
  }

  groups <- unique(data[[group_var]])
  if (length(groups) != 2) {
    stop(sprintf("Group variable must have exactly 2 levels. Found: %d", length(groups)))
  }

  # Set seed if provided
  restore_rng <- .local_seed(seed)
  on.exit(restore_rng(), add = TRUE)

  # Split data by group
  data_group1 <- data[data[[group_var]] == groups[1], ]
  data_group2 <- data[data[[group_var]] == groups[2], ]
  method1 <- if (choosers_only) .resolve_innocent_suspect(data_group1, innocent_suspect) else innocent_suspect
  method2 <- if (choosers_only) .resolve_innocent_suspect(data_group2, innocent_suspect) else innocent_suspect

  # Bootstrap each group
  boot_group1 <- bootstrap_anri(
    data_group1,
    confidence_bins = confidence_bins,
    choosers_only = choosers_only,
    lineup_size = lineup_size,
    n_bootstrap = n_bootstrap,
    conf_level = conf_level,
    seed = if (!is.null(seed)) seed else NULL,
    confidence_scale = confidence_scale,
    innocent_suspect = method1
  )

  boot_group2 <- bootstrap_anri(
    data_group2,
    confidence_bins = confidence_bins,
    choosers_only = choosers_only,
    lineup_size = lineup_size,
    n_bootstrap = n_bootstrap,
    conf_level = conf_level,
    seed = if (!is.null(seed)) seed + 1 else NULL,
    confidence_scale = confidence_scale,
    innocent_suspect = method2
  )

  # Compute bootstrap distribution of difference
  # Match lengths (use minimum)
  n_boot <- min(length(boot_group1$bootstrap_distribution),
                length(boot_group2$bootstrap_distribution))

  diff_distribution <- boot_group1$bootstrap_distribution[1:n_boot] -
                       boot_group2$bootstrap_distribution[1:n_boot]
  valid_diff <- is.finite(diff_distribution)
  if (sum(valid_diff) < 2L) stop("Fewer than two valid ANRI comparison replicates; no interval can be estimated.", call. = FALSE)

  # Point estimate and CI for difference
  diff_point <- boot_group1$anri - boot_group2$anri
  alpha <- 1 - conf_level
  ci_lower <- quantile(diff_distribution[valid_diff], alpha / 2)
  ci_upper <- quantile(diff_distribution[valid_diff], 1 - alpha / 2)

  # Check if significant (CI excludes 0)
  significant <- (ci_lower > 0 & ci_upper > 0) | (ci_lower < 0 & ci_upper < 0)

  list(
    anri_group1 = boot_group1$anri,
    anri_group2 = boot_group2$anri,
    nri_group1 = boot_group1$nri,
    nri_group2 = boot_group2$nri,
    difference = diff_point,
    ci_lower = as.numeric(ci_lower),
    ci_upper = as.numeric(ci_upper),
    conf_level = conf_level,
    significant = significant,
    group_names = c(as.character(groups[1]), as.character(groups[2])),
    n_bootstrap = sum(valid_diff),
    n_bootstrap_requested = n_boot,
    n_failed = sum(!valid_diff),
    bootstrap_results_group1 = boot_group1,
    bootstrap_results_group2 = boot_group2,
    difference_distribution = diff_distribution
  )
}


#' Plot ANRI Comparison Between Groups
#'
#' Creates a visualization comparing ANRI estimates between two groups
#' with bootstrap confidence intervals.
#'
#' @param compare_obj List output from compare_anri()
#'
#' @return A ggplot2 object
#'
#' @details
#' Creates a point-and-interval plot showing ANRI estimates and bootstrap
#' CIs for each group. Helps visualize the magnitude and uncertainty of
#' group differences.
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' n <- 300
#' data <- data.frame(
#'   target_present = rep(c(TRUE, FALSE), each = n / 2),
#'   identification = sample(c("suspect", "filler", "reject"), n,
#'                           replace = TRUE, prob = c(0.5, 0.25, 0.25)),
#'   confidence = round(runif(n, 0, 100)),
#'   condition = rep(c("sequential", "simultaneous"), times = n / 2)
#' )
#' cmp <- compare_anri(data, group_var = "condition",
#'                     confidence_bins = seq(0, 100, 20),
#'                     n_bootstrap = 100, seed = 1)
#' plot_anri_comparison(cmp)
#' }
#'
#' @export
#' @import ggplot2 tibble
plot_anri_comparison <- function(compare_obj) {

  # Prepare data for plotting
  plot_data <- tibble::tibble(
    group = compare_obj$group_names,
    anri = c(compare_obj$anri_group1, compare_obj$anri_group2),
    nri = c(compare_obj$nri_group1, compare_obj$nri_group2),
    ci_lower = c(compare_obj$bootstrap_results_group1$ci_lower,
                 compare_obj$bootstrap_results_group2$ci_lower),
    ci_upper = c(compare_obj$bootstrap_results_group1$ci_upper,
                 compare_obj$bootstrap_results_group2$ci_upper)
  )

  # Create plot
  p <- ggplot(plot_data, aes(x = group, y = anri)) +
    geom_point(size = 4, color = "darkblue") +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                 width = 0.2, linewidth = 1, color = "darkblue") +
    theme_bw(base_size = 14) +
    labs(
      x = "Group",
      y = "ANRI (Adjusted Normalized Resolution Index)",
      title = "ANRI Comparison Between Groups",
      subtitle = sprintf(
        "Difference = %.3f [%.3f, %.3f]%s",
        compare_obj$difference,
        compare_obj$ci_lower,
        compare_obj$ci_upper,
        if (compare_obj$significant) " *" else ""
      ),
      caption = sprintf(
        "Error bars: %d%% bootstrap CI\n* = Significant difference",
        compare_obj$conf_level * 100
      )
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")

  p
}


#' Plot Bootstrap Distribution of ANRI Difference
#'
#' Creates a histogram of the bootstrap distribution of the difference
#' between two groups.
#'
#' @param compare_obj List output from compare_anri()
#'
#' @return A ggplot2 object
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' n <- 300
#' data <- data.frame(
#'   target_present = rep(c(TRUE, FALSE), each = n / 2),
#'   identification = sample(c("suspect", "filler", "reject"), n,
#'                           replace = TRUE, prob = c(0.5, 0.25, 0.25)),
#'   confidence = round(runif(n, 0, 100)),
#'   condition = rep(c("sequential", "simultaneous"), times = n / 2)
#' )
#' cmp <- compare_anri(data, group_var = "condition",
#'                     confidence_bins = seq(0, 100, 20),
#'                     n_bootstrap = 100, seed = 1)
#' plot_anri_difference_distribution(cmp)
#' }
#'
#' @export
#' @import ggplot2
plot_anri_difference_distribution <- function(compare_obj) {

  diff_data <- data.frame(
    difference = compare_obj$difference_distribution
  )

  p <- ggplot(diff_data, aes(x = difference)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
    geom_vline(xintercept = compare_obj$difference,
              color = "darkblue", linewidth = 1.2) +
    geom_vline(xintercept = compare_obj$ci_lower,
              linetype = "dotted", color = "darkblue", linewidth = 0.8) +
    geom_vline(xintercept = compare_obj$ci_upper,
              linetype = "dotted", color = "darkblue", linewidth = 0.8) +
    theme_bw(base_size = 14) +
    labs(
      x = sprintf("ANRI Difference (%s - %s)",
                 compare_obj$group_names[1],
                 compare_obj$group_names[2]),
      y = "Frequency",
      title = "Bootstrap Distribution of ANRI Difference",
      subtitle = sprintf(
        "Observed: %.3f | %d%% CI: [%.3f, %.3f]",
        compare_obj$difference,
        compare_obj$conf_level * 100,
        compare_obj$ci_lower,
        compare_obj$ci_upper
      ),
      caption = "Solid line = observed difference\nDotted lines = CI bounds\nDashed line = null hypothesis (0)"
    )

  p
}


#' Print Method for ANRI Objects
#' @param x An anri object from compute_anri()
#' @param ... Additional arguments (ignored)
#' @export
print.lineup_anri <- function(x, ...) {
  cat("\n=== ANRI (Adjusted Normalized Resolution Index) ===\n\n")
  cat("Analysis type:", ifelse(x$choosers_only, "Choosers only\n", "All responses\n"))
  cat("Total N:", x$n_total, "\n")
  cat("Number of bins:", x$n_bins, "\n\n")

  cat("Resolution Indices:\n")
  cat("  ANRI (bias-corrected):  ", sprintf("%.4f", x$anri), "\n")
  cat("  NRI  (original):        ", sprintf("%.4f", x$nri), "\n\n")

  cat("Overall accuracy:", sprintf("%.3f", x$overall_accuracy), "\n\n")

  cat("Calibration Data by Bin:\n")
  print(x$calibration_data, n = Inf)

  invisible(x)
}


#' Print Method for ANRI Comparison Objects
#' @param x A comparison object from compare_anri()
#' @param ... Additional arguments (ignored)
#' @export
print.lineup_anri_comparison <- function(x, ...) {
  cat("\n=== ANRI Comparison Between Groups ===\n\n")

  cat("Group 1:", x$group_names[1], "\n")
  cat("  ANRI:", sprintf("%.4f", x$anri_group1), "\n")
  cat("  NRI: ", sprintf("%.4f", x$nri_group1), "\n\n")

  cat("Group 2:", x$group_names[2], "\n")
  cat("  ANRI:", sprintf("%.4f", x$anri_group2), "\n")
  cat("  NRI: ", sprintf("%.4f", x$nri_group2), "\n\n")

  cat("Difference (Group 1 - Group 2):\n")
  cat("  Point estimate:", sprintf("%.4f", x$difference), "\n")
  cat("  ", sprintf("%d%% CI:", x$conf_level * 100),
     sprintf("[%.4f, %.4f]", x$ci_lower, x$ci_upper), "\n")
  cat("  Significant:", ifelse(x$significant, "YES", "NO"), "\n\n")

  if (x$significant) {
    if (x$difference > 0) {
      cat(">> ", x$group_names[1], "has significantly higher resolution\n")
    } else {
      cat(">> ", x$group_names[2], "has significantly higher resolution\n")
    }
  } else {
    cat(">> No significant difference in resolution between groups\n")
  }

  cat("\nBootstrap replications:", x$n_bootstrap, "\n")

  invisible(x)
}
