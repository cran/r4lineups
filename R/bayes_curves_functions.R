# `entropy()` is defined once in R/eig_functions.R (entropy(p, base = 2)); the
# calls below resolve to it (base defaults to 2). A second definition previously
# lived here, which collided in the package namespace and duplicated entropy.Rd.


#' Compute Bayesian Prior-Posterior and Information Gain Curves
#'
#' Computes Bayesian updating curves showing how different lineup responses
#' update beliefs about suspect guilt, following Wells, Yang, & Smalarz (2015).
#'
#' @param data A dataframe with the following columns:
#'   \itemize{
#'     \item target_present: Logical. TRUE if guilty suspect in lineup
#'     \item identification: Character. "suspect", "filler", or "reject"
#'     \item confidence: Numeric. Confidence rating (optional for response categories)
#'   }
#' @param response_categories Character vector specifying how to categorize responses.
#'   Options:
#'   \itemize{
#'     \item "simple": Three categories (suspect, filler, reject)
#'     \item "confidence": Combines identification with confidence bins
#'   }
#' @param confidence_bins Numeric vector of bin edges if using confidence categories
#'   (e.g., c(0, 60, 80, 100)). Ignored if response_categories = "simple".
#' @param prior_grid Numeric vector of prior probabilities to evaluate
#'   (default: seq(0.01, 0.99, 0.01))
#' @param lineup_size Nominal lineup size, used when target-absent data have no
#'   designated innocent suspect.
#'
#' @return A list containing:
#'   \itemize{
#'     \item curves: Dataframe with prior, posterior, and information gain for each response
#'     \item likelihoods: Dataframe with p(x|guilty) and p(x|innocent) for each response
#'     \item response_counts: Count of each response type by target presence
#'     \item n_guilty: Number of target-present lineups
#'     \item n_innocent: Number of target-absent lineups
#'   }
#'
#' @details
#' This function treats lineup outcomes as evidence for Bayesian updating. For each
#' response type x (e.g., high-confidence suspect ID, rejection, etc.), it computes:
#'
#' \strong{Likelihoods from observed data:}
#' \deqn{p(x|guilty) = \frac{count(x | target\_present)}{N_{guilty}}}
#' \deqn{p(x|innocent) = \frac{count(x | target\_absent)}{N_{innocent}}}
#'
#' \strong{For each prior probability p(guilty):}
#' \deqn{p(x) = p(guilty) \cdot p(x|guilty) + (1-p(guilty)) \cdot p(x|innocent)}
#' \deqn{p(guilty|x) = \frac{p(guilty) \cdot p(x|guilty)}{p(x)}}
#'
#' \strong{Information gain:}
#' \deqn{IG(x) = H(prior) - H(posterior)}
#'
#' where H(p) is binary entropy.
#'
#' The information gain quantifies how much uncertainty is reduced by observing
#' response x. It is negative when the response moves belief towards 0.5, as a
#' suspect identification does at a low prior; that is a correct update that
#' leaves more uncertainty, not misleading evidence.
#'
#' @references
#' Wells, G. L., Yang, Y., & Smalarz, L. (2015). Eyewitness identification:
#' Bayesian information gain, base-rate effect-equivalency curves, and reasonable
#' suspicion. \emph{Law and Human Behavior, 39}(2), 99-122.
#'
#' @examples
#' data(lineup_example)
#' bayes <- make_bayes_curves(lineup_example)
#' bayes$likelihoods
#'
#' @inheritParams make_rocdata
#' @export
#' @import tibble
make_bayes_curves <- function(data,
                               response_categories = c("simple", "confidence"),
                               confidence_bins = NULL,
                               prior_grid = seq(0.01, 0.99, 0.01),
                               lineup_size = 6,
                               innocent_suspect = "auto") {

  # Validate required columns
  required_cols <- c("target_present", "identification")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  response_categories <- match.arg(response_categories)
  .validate_lineup_analysis(data,
    require_confidence = identical(response_categories, "confidence"), lineup_size = lineup_size)
  if (response_categories == "confidence") .validate_bins(data$confidence, confidence_bins)
  if (!is.numeric(lineup_size) || length(lineup_size) != 1L ||
      !is.finite(lineup_size) || lineup_size < 2) {
    stop("lineup_size must be a finite number of at least 2.", call. = FALSE)
  }

  # Create response categories
  if (response_categories == "simple") {
    data$response_cat <- data$identification
  } else if (response_categories == "confidence") {
    if (is.null(confidence_bins)) {
      stop("confidence_bins must be provided when response_categories = 'confidence'")
    }
    if (!"confidence" %in% names(data)) {
      stop("confidence column required when response_categories = 'confidence'")
    }

    data$conf_bin <- cut(data$confidence,
                         breaks = confidence_bins,
                         include.lowest = TRUE,
                         right = TRUE)
    data$response_cat <- paste(data$identification, data$conf_bin, sep = "_")
  }

  # Separate by target presence
  guilty_data <- data[data$target_present == TRUE, ]
  innocent_data <- data[data$target_present == FALSE, ]

  n_guilty <- nrow(guilty_data)
  n_innocent <- nrow(innocent_data)

  if (n_guilty == 0 | n_innocent == 0) {
    stop("Data must include both target-present and target-absent lineups")
  }

  guilty_counts <- table(guilty_data$response_cat)
  innocent_method <- .innocent_suspect_method(innocent_data, innocent_suspect)
  innocent_counts <- .adjust_innocent_response_counts(
    table(innocent_data$response_cat), lineup_size, innocent_method
  )
  response_types <- union(names(guilty_counts), names(innocent_counts))

  # Compute likelihoods for each response type
  likelihood_results <- tibble::tibble(
    response = character(),
    p_x_given_guilty = numeric(),
    p_x_given_innocent = numeric(),
    n_guilty = numeric(),
    n_innocent = numeric()
  )

  for (resp in response_types) {
    n_resp_guilty <- unname(guilty_counts[resp])
    n_resp_innocent <- unname(innocent_counts[resp])
    if (is.na(n_resp_guilty)) n_resp_guilty <- 0
    if (is.na(n_resp_innocent)) n_resp_innocent <- 0

    p_x_g <- n_resp_guilty / n_guilty
    p_x_i <- n_resp_innocent / n_innocent

    likelihood_results <- rbind(likelihood_results, tibble::tibble(
      response = resp,
      p_x_given_guilty = p_x_g,
      p_x_given_innocent = p_x_i,
      n_guilty = n_resp_guilty,
      n_innocent = n_resp_innocent
    ))
  }

  # A zero likelihood makes the ratio unbounded, so the prior-posterior curve
  # for that response is pinned flat at 0 or 1 for every prior. The plot then
  # looks broken rather than informative, so say what happened.
  degenerate <- likelihood_results$response[
    likelihood_results$p_x_given_guilty == 0 |
      likelihood_results$p_x_given_innocent == 0
  ]
  if (length(degenerate) > 0) {
    warning("No observations in one arm for response category ",
            paste(sQuote(degenerate), collapse = ", "),
            ". The likelihood ratio is unbounded, so the prior-posterior ",
            "curve is flat at 0 or 1 for every prior. This usually means the ",
            "confidence bins are too narrow, or the sample too small, for the ",
            "rate being estimated rather than that the evidence is perfectly ",
            "diagnostic.", call. = FALSE)
  }

  # Compute prior-posterior curves for each response type
  curve_results <- tibble::tibble(
    response = character(),
    prior = numeric(),
    posterior = numeric(),
    information_gain = numeric()
  )

  for (resp in response_types) {
    p_x_g <- likelihood_results$p_x_given_guilty[likelihood_results$response == resp]
    p_x_i <- likelihood_results$p_x_given_innocent[likelihood_results$response == resp]

    for (prior in prior_grid) {
      # Compute p(x)
      p_x <- prior * p_x_g + (1 - prior) * p_x_i

      # Avoid division by zero
      if (p_x == 0) {
        posterior <- prior  # No update possible
        info_gain <- 0
      } else {
        # Compute posterior
        posterior <- (prior * p_x_g) / p_x

        # Compute information gain
        H_prior <- entropy(prior)
        H_post <- entropy(posterior)
        info_gain <- H_prior - H_post
      }

      curve_results <- rbind(curve_results, tibble::tibble(
        response = resp,
        prior = prior,
        posterior = posterior,
        information_gain = info_gain
      ))
    }
  }

  # Store the same (possibly filler-adjusted) counts used for likelihoods.
  response_counts <- likelihood_results[, c("response", "n_guilty", "n_innocent")]

  list(
    curves = curve_results,
    likelihoods = likelihood_results,
    response_counts = response_counts,
    n_guilty = n_guilty,
    n_innocent = n_innocent,
    response_categories = response_categories,
    lineup_size = lineup_size,
    innocent_suspect_method = innocent_method
  )
}


#' Compute Base-Rate Effect-Equivalency (BREE) Curves
#'
#' Computes BREE curves showing the base-rate shift required for one procedure
#' to produce the same posterior probability as another procedure, following
#' Wells, Yang, & Smalarz (2015).
#'
#' @param data_proc_a A dataframe for procedure A with columns: target_present,
#'   identification, confidence (standard lineup data format)
#' @param data_proc_b A dataframe for procedure B (same format as proc_a)
#' @param reference_response Character string specifying which response to compare
#'   (e.g., "suspect" for suspect IDs). Default = "suspect".
#' @param confidence_bins Optional numeric vector of bin edges if using confidence-based
#'   response (e.g., c(0, 60, 80, 100)). If provided, reference_response should
#'   specify both identification and bin (e.g., "suspect_(80,100]")
#' @param prior_grid Numeric vector of prior probabilities for procedure A
#'   (default: seq(0.01, 0.99, 0.01))
#' @param lineup_size Nominal lineup size for procedures without a designated
#'   innocent suspect.
#'
#' @return A list containing:
#'   \itemize{
#'     \item bree_curve: Dataframe with prior_a, posterior_a, prior_b, and delta
#'     \item proc_a_likelihoods: Likelihoods for procedure A
#'     \item proc_b_likelihoods: Likelihoods for procedure B
#'     \item reference_response: The response category used for comparison
#'   }
#'
#' @details
#' BREE curves answer the question: "How much would the base rate need to change
#' for Procedure B to yield the same posterior probability as Procedure A?"
#'
#' For each prior probability in procedure A:
#' 1. Compute the posterior p(guilty|x) for the reference response in procedure A
#' 2. Find the prior probability in procedure B that yields the same posterior
#' 3. Delta = prior_B - prior_A
#'
#' Interpretation:
#' \itemize{
#'   \item Delta > 0: Procedure B requires a higher base rate to match A's posterior
#'     (A is more diagnostic)
#'   \item Delta < 0: Procedure B requires a lower base rate (B is more diagnostic)
#'   \item Delta = 0: Procedures are equally diagnostic
#' }
#'
#' @references
#' Wells, G. L., Yang, Y., & Smalarz, L. (2015). Eyewitness identification:
#' Bayesian information gain, base-rate effect-equivalency curves, and reasonable
#' suspicion. \emph{Law and Human Behavior, 39}(2), 99-122.
#'
#' @examples
#' data(lineup_example)
#' odd <- seq(1, nrow(lineup_example), by = 2)
#' bree <- make_bree_curve(lineup_example[odd, ], lineup_example[-odd, ])
#' head(bree$bree_curve)
#'
#' @inheritParams make_rocdata
#' @export
#' @import tibble
make_bree_curve <- function(data_proc_a,
                             data_proc_b,
                             reference_response = "suspect",
                             confidence_bins = NULL,
                             prior_grid = seq(0.01, 0.99, 0.01),
                             lineup_size = 6,
                             innocent_suspect = "auto") {

  # Determine response category type
  if (is.null(confidence_bins)) {
    response_type <- "simple"
  } else {
    response_type <- "confidence"
  }

  # Compute Bayes curves for both procedures
  curves_a <- make_bayes_curves(data_proc_a,
                                response_categories = response_type,
                                confidence_bins = confidence_bins,
                                prior_grid = prior_grid,
                                lineup_size = lineup_size,
                                innocent_suspect = innocent_suspect)

  curves_b <- make_bayes_curves(data_proc_b,
                                response_categories = response_type,
                                confidence_bins = confidence_bins,
                                prior_grid = prior_grid,
                                lineup_size = lineup_size,
                                innocent_suspect = innocent_suspect)

  # Check if reference response exists in both procedures
  if (!reference_response %in% curves_a$likelihoods$response) {
    stop("Reference response '", reference_response, "' not found in procedure A")
  }
  if (!reference_response %in% curves_b$likelihoods$response) {
    stop("Reference response '", reference_response, "' not found in procedure B")
  }

  # Extract likelihoods for reference response
  lik_a <- curves_a$likelihoods[curves_a$likelihoods$response == reference_response, ]
  lik_b <- curves_b$likelihoods[curves_b$likelihoods$response == reference_response, ]

  p_x_g_a <- lik_a$p_x_given_guilty
  p_x_i_a <- lik_a$p_x_given_innocent
  p_x_g_b <- lik_b$p_x_given_guilty
  p_x_i_b <- lik_b$p_x_given_innocent

  # Compute BREE curve
  bree_results <- tibble::tibble(
    prior_a = numeric(),
    posterior_a = numeric(),
    prior_b = numeric(),
    delta = numeric()
  )

  for (prior_a in prior_grid) {
    # Compute posterior for procedure A at this prior
    p_x_a <- prior_a * p_x_g_a + (1 - prior_a) * p_x_i_a

    if (p_x_a == 0) {
      posterior_a <- prior_a
    } else {
      posterior_a <- (prior_a * p_x_g_a) / p_x_a
    }

    # Find prior_b that yields same posterior in procedure B
    # posterior = (prior * p_x_g) / (prior * p_x_g + (1-prior) * p_x_i)
    # Solve for prior:
    # posterior * (prior * p_x_g + (1-prior) * p_x_i) = prior * p_x_g
    # posterior * prior * p_x_g + posterior * (1-prior) * p_x_i = prior * p_x_g
    # posterior * prior * p_x_g + posterior * p_x_i - posterior * prior * p_x_i = prior * p_x_g
    # posterior * p_x_i = prior * p_x_g - posterior * prior * p_x_g + posterior * prior * p_x_i
    # posterior * p_x_i = prior * (p_x_g - posterior * p_x_g + posterior * p_x_i)
    # posterior * p_x_i = prior * (p_x_g * (1 - posterior) + posterior * p_x_i)
    # prior = (posterior * p_x_i) / (p_x_g * (1 - posterior) + posterior * p_x_i)

    # Rearranging more clearly:
    # posterior = (prior * p_x_g) / (prior * p_x_g + (1-prior) * p_x_i)
    # Let's solve: posterior * [prior * p_x_g + (1-prior) * p_x_i] = prior * p_x_g
    # posterior * prior * p_x_g + posterior * p_x_i - posterior * prior * p_x_i = prior * p_x_g
    # posterior * p_x_i = prior * p_x_g - posterior * prior * p_x_g + posterior * prior * p_x_i
    # posterior * p_x_i = prior * [p_x_g - posterior * p_x_g + posterior * p_x_i]
    # posterior * p_x_i = prior * [p_x_g * (1 - posterior) + posterior * p_x_i]
    # prior = posterior * p_x_i / [p_x_g * (1 - posterior) + posterior * p_x_i]

    numerator <- posterior_a * p_x_i_b
    denominator <- p_x_g_b * (1 - posterior_a) + posterior_a * p_x_i_b

    if (denominator == 0) {
      prior_b <- NA
    } else {
      prior_b <- numerator / denominator
      # Constrain to valid probability range
      prior_b <- max(0, min(1, prior_b))
    }

    delta <- prior_b - prior_a

    bree_results <- rbind(bree_results, tibble::tibble(
      prior_a = prior_a,
      posterior_a = posterior_a,
      prior_b = prior_b,
      delta = delta
    ))
  }

  list(
    bree_curve = bree_results,
    proc_a_likelihoods = lik_a,
    proc_b_likelihoods = lik_b,
    reference_response = reference_response
  )
}


# Order responses from the most to the least indicative of guilt, so the
# legend reads in the same order as the prior-posterior curves stack (curves
# for different likelihood ratios never cross). The posterior at the prior
# nearest 0.5 orders them by likelihood ratio.
.order_bayes_responses <- function(curve_data) {
  at_half <- curve_data[abs(curve_data$prior - 0.5) ==
                          min(abs(curve_data$prior - 0.5)), ]
  lv <- at_half$response[order(at_half$posterior, decreasing = TRUE)]
  curve_data$response <- factor(curve_data$response, levels = unique(lv))
  curve_data
}

#' Plot Prior-Posterior Curves
#'
#' Creates a plot showing how different lineup responses update prior beliefs
#' about suspect guilt to posterior beliefs.
#'
#' @param bayes_obj List output from make_bayes_curves()
#' @param selected_responses Character vector of response categories to plot.
#'   If NULL (default), plots all responses.
#' @param show_diagonal Logical. Whether to show diagonal line (no update).
#'   Default = TRUE.
#'
#' @return A ggplot2 object
#'
#' @details
#' Prior-posterior curves show Bayesian updating for different responses.
#' The diagonal line represents no update (posterior = prior). Curves above
#' the diagonal indicate responses that increase belief in guilt, while curves
#' below decrease belief in guilt.
#'
#' @examples
#' data(lineup_example)
#' bayes <- make_bayes_curves(lineup_example)
#' plot_bayes_prior_posterior(bayes)
#'
#' @export
#' @import ggplot2
plot_bayes_prior_posterior <- function(bayes_obj,
                                       selected_responses = NULL,
                                       show_diagonal = TRUE) {

  curve_data <- bayes_obj$curves

  # Filter to selected responses if specified
  if (!is.null(selected_responses)) {
    curve_data <- curve_data[curve_data$response %in% selected_responses, ]
  }
  curve_data <- .order_bayes_responses(curve_data)

  # Create plot
  p <- ggplot(curve_data, aes(x = prior, y = posterior, color = response)) +
    geom_line(linewidth = 1) +
    theme_bw(base_size = 14) +
    labs(
      x = "Prior Probability of Guilt",
      y = "Posterior Probability of Guilt",
      title = "Prior-Posterior Curves",
      subtitle = "Bayesian updating for different lineup responses",
      color = "Response"
    ) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    coord_fixed(ratio = 1)

  # Add diagonal line (no update)
  if (show_diagonal) {
    p <- p + geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         color = "gray50", linewidth = 0.6)
  }

  p
}


#' Plot Information Gain Curves
#'
#' Creates a plot showing how much uncertainty is reduced by different lineup responses.
#'
#' @param bayes_obj List output from make_bayes_curves()
#' @param selected_responses Character vector of response categories to plot.
#'   If NULL (default), plots all responses.
#'
#' @return A ggplot2 object
#'
#' @details
#' Information gain curves show the reduction in uncertainty (measured in bits)
#' for each response type across different prior probabilities. A response that
#' points to guilt gains most at priors above 0.5 and one that points to
#' innocence at priors below 0.5; either can be negative where it moves belief
#' towards 0.5. The legend lists responses from the most to the least
#' indicative of guilt.
#'
#' @examples
#' data(lineup_example)
#' bayes <- make_bayes_curves(lineup_example)
#' plot_bayes_information_gain(bayes)
#'
#' @export
#' @import ggplot2
plot_bayes_information_gain <- function(bayes_obj,
                                        selected_responses = NULL) {

  curve_data <- bayes_obj$curves

  # Filter to selected responses if specified
  if (!is.null(selected_responses)) {
    curve_data <- curve_data[curve_data$response %in% selected_responses, ]
  }
  curve_data <- .order_bayes_responses(curve_data)

  # Create plot
  p <- ggplot(curve_data, aes(x = prior, y = information_gain, color = response)) +
    geom_line(linewidth = 1) +
    theme_bw(base_size = 14) +
    labs(
      x = "Prior Probability of Guilt",
      y = "Information Gain (bits)",
      title = "Information Gain Curves",
      subtitle = "Reduction in uncertainty for different lineup responses",
      color = "Response"
    ) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.6)

  p
}


#' Plot BREE Curve
#'
#' Creates a plot showing the base-rate shift required for one procedure to
#' match another's diagnostic value.
#'
#' @param bree_obj List output from make_bree_curve()
#' @param show_reference Logical. Whether to show reference line at delta = 0.
#'   Default = TRUE.
#'
#' @return A ggplot2 object
#'
#' @details
#' The BREE curve shows how much the base rate (prior probability) would need
#' to shift for Procedure B to yield the same posterior as Procedure A.
#'
#' Positive delta: Procedure A is more diagnostic (B needs higher base rate to match)
#' Negative delta: Procedure B is more diagnostic (B needs lower base rate to match)
#' Zero delta: Procedures are equally diagnostic
#'
#' @examples
#' data(lineup_example)
#' odd <- seq(1, nrow(lineup_example), by = 2)
#' bree <- make_bree_curve(lineup_example[odd, ], lineup_example[-odd, ])
#' plot_bree(bree)
#'
#' @export
#' @import ggplot2
plot_bree <- function(bree_obj, show_reference = TRUE) {

  bree_data <- bree_obj$bree_curve

  # Create plot
  p <- ggplot(bree_data, aes(x = prior_a, y = delta)) +
    geom_line(linewidth = 1, color = "darkred") +
    theme_bw(base_size = 14) +
    labs(
      x = "Prior Probability (Procedure A)",
      y = "Base-Rate Shift (Delta)",
      title = "Base-Rate Effect-Equivalency (BREE) Curve",
      subtitle = paste("Reference response:", bree_obj$reference_response),
      caption = "Positive delta: Proc A more diagnostic; Negative: Proc B more diagnostic"
    ) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))

  # Add reference line at delta = 0
  if (show_reference) {
    p <- p + geom_hline(yintercept = 0, linetype = "dashed",
                        color = "gray50", linewidth = 0.6)
  }

  p
}


#' Print Method for Bayes Curves Objects
#' @param x A bayes_curves object from make_bayes_curves()
#' @param ... Additional arguments (ignored)
#' @export
print.lineup_bayes_curves <- function(x, ...) {
  cat("\n=== Bayesian Prior-Posterior Analysis ===\n\n")
  cat("Sample sizes:\n")
  cat("  Target-present (guilty):  ", x$n_guilty, "\n")
  cat("  Target-absent (innocent): ", x$n_innocent, "\n\n")

  cat("Response categories:", x$response_categories, "\n\n")

  cat("Likelihoods by Response:\n")
  print(x$likelihoods, n = Inf)

  cat("\n\nPrior-Posterior curves computed for", nrow(x$likelihoods), "response types\n")
  cat("Use plot_bayes_prior_posterior() and plot_bayes_information_gain() to visualize\n")

  invisible(x)
}
