#' Apply Decision Rule to Lineup Strengths
#'
#' Internal helper function that computes decision variable based on
#' different decision strategies.
#'
#' @param strengths Numeric vector of memory strengths for all lineup members
#' @param rule Character. Decision rule to apply
#' @param lineup_size Integer. Number of lineup members
#' @param d_prime Numeric. Used for some rule calculations
#'
#' @return List with decision_value and chosen_position
#' @keywords internal
.apply_decision_rule <- function(strengths, rule, lineup_size, d_prime = NULL) {

  if (!is.numeric(strengths) || length(strengths) != lineup_size ||
      anyNA(strengths) || any(!is.finite(strengths))) {
    stop("strengths must contain one finite numeric value per lineup member.",
         call. = FALSE)
  }

  if (rule == "max") {
    # Independent Observations Model - MAX rule
    # Choose lineup member with highest memory strength
    decision_value <- max(strengths)
    chosen_position <- which.max(strengths)

  } else if (rule == "best_rest") {
    # BEST-REST Model
    # Compare best match vs average of remaining members
    # Based on Wixted et al. (2018) and pyWitness implementation

    # Find best match
    max_idx <- which.max(strengths)
    best_strength <- strengths[max_idx]

    # Compute average of rest
    rest_strengths <- strengths[-max_idx]
    rest_mean <- mean(rest_strengths)

    # Decision variable: best - average(rest)
    decision_value <- best_strength - rest_mean
    chosen_position <- max_idx

  } else if (rule == "ensemble") {
    # Ensemble Model: the MAX strength minus the lineup mean.

    # Find best match first
    max_idx <- which.max(strengths)
    best_strength <- strengths[max_idx]

    # This equals (n - 1) / n times BEST-Rest. The two rules therefore
    # represent the same model under a corresponding rescaling of criteria.
    other_strengths <- strengths[-max_idx]
    ensemble_avg <- (best_strength * (lineup_size - 1) - sum(other_strengths)) / lineup_size

    decision_value <- ensemble_avg
    chosen_position <- max_idx

  } else if (rule == "integration") {
    # Integration Model
    # Sum memory strengths across all members
    # Based on Wixted et al. (2018)

    # Find best match
    max_idx <- which.max(strengths)

    # Decision variable: sum of all strengths
    # This represents integration of evidence across lineup
    decision_value <- sum(strengths)
    chosen_position <- max_idx

  } else {
    stop("Unknown decision rule: ", rule)
  }

  list(
    decision_value = decision_value,
    chosen_position = chosen_position
  )
}

.simulation_criteria <- function(c_criterion, conf_levels) {
  if (!is.numeric(c_criterion) || length(c_criterion) < 1L ||
      anyNA(c_criterion) || any(!is.finite(c_criterion))) {
    stop("c_criterion must contain finite numeric decision criteria.",
         call. = FALSE)
  }

  if (is.null(conf_levels)) {
    if (length(c_criterion) != 1L) {
      stop("c_criterion must be a scalar when conf_levels is NULL.",
           call. = FALSE)
    }
    return(as.numeric(c_criterion))
  }

  if (!is.numeric(conf_levels) || length(conf_levels) != 1L ||
      is.na(conf_levels) || !is.finite(conf_levels) ||
      conf_levels < 2L || conf_levels != as.integer(conf_levels)) {
    stop("conf_levels must be at least 2 and integer-valued, or NULL.",
         call. = FALSE)
  }
  conf_levels <- as.integer(conf_levels)

  if (length(c_criterion) == 1L) {
    # A scalar is the actual ID/rejection threshold. Additional confidence
    # thresholds are placed over a documented two-unit range above it.
    criteria <- c_criterion + seq(0, 2, length.out = conf_levels)
  } else if (length(c_criterion) == conf_levels) {
    criteria <- as.numeric(c_criterion)
  } else {
    stop("c_criterion must be a scalar or have one value per confidence level.",
         call. = FALSE)
  }

  if (is.unsorted(criteria, strictly = TRUE)) {
    stop("Confidence criteria must be strictly increasing.", call. = FALSE)
  }
  criteria
}

.simulation_response <- function(decision_value, criteria, conf_levels) {
  makes_id <- decision_value > criteria[1L]
  if (is.null(conf_levels)) {
    confidence <- NA_integer_
  } else {
    confidence <- sum(decision_value > criteria)
    confidence <- max(1L, min(as.integer(conf_levels), confidence))
  }
  list(makes_id = makes_id, confidence = confidence)
}


#' Simulate Lineup Identification Data
#'
#' Generates simulated eyewitness lineup data based on signal detection theory
#' parameters. Useful for power analysis, method validation, and teaching.
#'
#' @param n_tp Integer. Number of target-present lineups (default = 100)
#' @param n_ta Integer. Number of target-absent lineups (default = 100)
#' @param d_prime Numeric. Discriminability (d') between target and lure
#'   distributions. Higher values = better memory. Typical range: 0.5 to 3.0
#'   (default = 1.5)
#' @param c_criterion Numeric scalar or vector of ordered decision criteria.
#'   The first value is always the identification/rejection threshold. With
#'   confidence ratings, a scalar generates \code{conf_levels} thresholds from
#'   \code{c_criterion} through \code{c_criterion + 2}; alternatively, supply
#'   exactly one strictly increasing threshold per confidence level. Lower
#'   values are more liberal (default = 0).
#' @param lineup_size Integer. Number of lineup members (default = 6)
#' @param conf_levels Integer. Number of confidence levels to simulate. If NULL,
#'   returns binary decision only (default = 5)
#' @param decision_rule Character. Decision strategy for lineup choices:
#'   \itemize{
#'     \item "max" - Independent observations, choose highest strength (default)
#'     \item "best_rest" - Best strength minus the mean of the other members
#'     \item "ensemble" - Best strength minus the mean of all lineup members
#'     \item "integration" - Sum memory strengths across all members
#'   }
#' @param foil_bias Numeric vector of length \code{lineup_size - 1}, giving the
#'   mean memory strength of each foil. Defaults to \code{NULL}, which places
#'   every foil at \code{N(0, 1)}. Supplying a vector makes the foils differ in
#'   how plausible they are, so that they are no longer interchangeable. Values
#'   are means on the same scale as \code{d_prime}: a foil at \code{0.5} is
#'   chosen more often than one at \code{-0.5}, and a strongly negative value
#'   produces a foil that is rarely or never chosen — a null foil.
#' @param include_response_time Logical. Whether to simulate response times
#'   correlated with memory strength (default = FALSE)
#' @param seed Integer. Random seed for reproducibility (default = NULL)
#'
#' @return A dataframe with columns:
#'   \itemize{
#'     \item participant_id: Unique ID for each trial
#'     \item target_present: Logical. TRUE if target in lineup
#'     \item lineup_member: Integer. Position of the lineup member actually
#'       chosen, or 0 for a rejection. Each foil is simulated separately and
#'       retains its own identity; foils are not treated as interchangeable
#'     \item suspect_pos: Integer. Position occupied by the suspect (1 by
#'       construction; the target in target-present lineups, the designated
#'       innocent suspect in target-absent lineups)
#'     \item identification: Character. "suspect", "filler", or "reject",
#'       derived from lineup_member and suspect_pos. See
#'       \code{\link{classify_lineup_choices}}
#'     \item confidence: Numeric (if conf_levels specified). Confidence rating
#'     \item response_time: Numeric (if include_response_time = TRUE). RT in ms
#'   }
#'
#' @section Foil identity:
#' Memory strengths are drawn independently for every lineup member, so the
#' foils are distinguishable and are not exchangeable. \code{lineup_member}
#' preserves which member was chosen; \code{identification} is a derived
#' summary of it. Analyses that need member-level information (for example the
#' effective-size correction in \code{\link{make_ppv_range}}) can therefore use
#' \code{lineup_member} directly rather than working from the collapsed labels.
#'
#' @details
#' This function implements signal detection models with multiple decision rules:
#' \itemize{
#'   \item Target distribution: Normal(d_prime, 1)
#'   \item Lure distribution: Normal(0, 1)
#'   \item Decision: Depends on decision_rule parameter
#' }
#'
#' \strong{Decision Rules:}
#' \itemize{
#'   \item \strong{MAX} (default): Independent observations model. Choose lineup member
#'     with highest memory strength (Clark, 2003).
#'   \item \strong{BEST-REST}: Best strength minus the average of the remaining
#'     members (Clark, 2003).
#'   \item \strong{Ensemble}: Best strength minus the average of all lineup members
#'     (Wixted et al., 2018). For lineup size \eqn{k}, this is
#'     \eqn{(k - 1) / k} times BEST-REST, so the two are equivalent model
#'     parameterizations when their criteria are rescaled by the same factor.
#'   \item \strong{Integration}: Sum memory strengths across all lineup members.
#'     Represents complete integration of evidence (Wixted et al., 2018).
#' }
#'
#' All rules use independent signals with target distribution
#' \eqn{N(d', 1)} and filler distribution \eqn{N(0, 1)}. Thus, this simulator
#' represents the equal-variance, zero-correlation special case of the lineup
#' models. It does not implement the shared-variance or unequal-variance
#' likelihood models fitted by Wixted et al. (2018).
#'
#' The simulation assumes:
#' \itemize{
#'   \item Fair lineups (all fillers equally similar to description)
#'   \item Perfect attention (no guessing without memory)
#'   \item Normal distributions for memory strength
#' }
#'
#' Response times (if simulated) are a heuristic convenience, not a fitted
#' reaction-time or drift-diffusion model:
#' \itemize{
#'   \item Faster for stronger memory signals
#'   \item A bounded noisy function of decision strength
#' }
#'
#' @references
#' Wixted, J. T., Vul, E., Mickes, L., & Wilson, B. M. (2018). Models of
#' lineup memory. \emph{Cognitive Psychology, 105}, 81--114.
#' \doi{10.1016/j.cogpsych.2018.06.001}
#'
#' Mickes, L., et al. (2024). pyWitness 1.0: A python eyewitness
#' identification analysis toolkit. \emph{Behavior Research Methods, 56},
#' 1533-1550.
#'
#' @examples
#' # Basic simulation: strong memory with MAX rule (default)
#' strong_memory <- simulate_lineup_data(
#'   n_tp = 200, n_ta = 200,
#'   d_prime = 2.0,
#'   conf_levels = 3,
#'   seed = 42
#' )
#'
#' # Ensemble decision rule
#' ensemble_data <- simulate_lineup_data(
#'   n_tp = 200, n_ta = 200,
#'   d_prime = 1.5,
#'   decision_rule = "ensemble",
#'   conf_levels = 5,
#'   seed = 42
#' )
#'
#' # Integration decision rule
#' integration_data <- simulate_lineup_data(
#'   n_tp = 200, n_ta = 200,
#'   d_prime = 1.5,
#'   decision_rule = "integration",
#'   conf_levels = 5,
#'   seed = 42
#' )
#'
#' # Compare ROC curves across decision rules
#' roc_max <- make_roc(strong_memory, lineup_size = 6)
#' roc_ensemble <- make_roc(ensemble_data, lineup_size = 6)
#'
#' \donttest{
#' # Power analysis: vary sample size (few simulations, to keep this quick;
#' # use hundreds in practice)
#' power_results <- simulate_power_analysis(
#'   sample_sizes = c(50, 100),
#'   d_prime = 1.5,
#'   n_simulations = 5,
#'   n_bootstrap = 50,
#'   seed = 1
#' )
#' }
#'
#' @importFrom stats rnorm
#' @export
simulate_lineup_data <- function(n_tp = 100,
                                  n_ta = 100,
                                  d_prime = 1.5,
                                  c_criterion = 0,
                                  lineup_size = 6,
                                  conf_levels = 5,
                                  decision_rule = c("max", "best_rest", "ensemble", "integration"),
                                  foil_bias = NULL,
                                  include_response_time = FALSE,
                                  seed = NULL) {

  if (!is.null(seed)) {
    restore_rng <- .local_seed(seed)
    on.exit(restore_rng(), add = TRUE)
  }

  # Match decision_rule argument
  decision_rule <- match.arg(decision_rule)

  # Validate inputs
  if (!is.numeric(n_tp) || length(n_tp) != 1L || is.na(n_tp) ||
      !is.finite(n_tp) || n_tp < 1L || n_tp != as.integer(n_tp) ||
      !is.numeric(n_ta) || length(n_ta) != 1L || is.na(n_ta) ||
      !is.finite(n_ta) || n_ta < 1L || n_ta != as.integer(n_ta)) {
    stop("n_tp and n_ta must be positive integers.", call. = FALSE)
  }
  n_tp <- as.integer(n_tp)
  n_ta <- as.integer(n_ta)
  if (!is.numeric(d_prime) || length(d_prime) != 1L || is.na(d_prime) ||
      !is.finite(d_prime) || d_prime < 0) {
    stop("d_prime must be a finite non-negative scalar.", call. = FALSE)
  }
  if (!is.numeric(lineup_size) || length(lineup_size) != 1L ||
      is.na(lineup_size) || !is.finite(lineup_size) || lineup_size < 2L ||
      lineup_size != as.integer(lineup_size)) {
    stop("lineup_size must be at least 2 and integer-valued.", call. = FALSE)
  }
  lineup_size <- as.integer(lineup_size)
  if (!is.logical(include_response_time) ||
      length(include_response_time) != 1L || is.na(include_response_time)) {
    stop("include_response_time must be TRUE or FALSE.", call. = FALSE)
  }
  # foil_bias shifts the mean strength of each foil, so that foils differ in how
  # plausible they are. NULL keeps every foil at N(0, 1), the previous
  # behaviour, so existing calls are unaffected.
  if (is.null(foil_bias)) {
    foil_bias <- rep(0, lineup_size - 1L)
  } else {
    if (!is.numeric(foil_bias) || anyNA(foil_bias) ||
        any(!is.finite(foil_bias))) {
      stop("foil_bias must be a finite numeric vector.", call. = FALSE)
    }
    if (length(foil_bias) != lineup_size - 1L) {
      stop("foil_bias must have one value per foil, i.e. lineup_size - 1 = ",
           lineup_size - 1L, " values.", call. = FALSE)
    }
    foil_bias <- as.numeric(foil_bias)
  }

  criteria <- .simulation_criteria(c_criterion, conf_levels)
  if (!is.null(conf_levels)) conf_levels <- as.integer(conf_levels)

  # Initialize results
  all_data <- list()

  # --- Target-Present Lineups ---
  for (i in 1:n_tp) {
    # Sample memory strength for each lineup member
    # Target: N(d', 1), Fillers: N(0, 1)
    target_strength <- rnorm(1, mean = d_prime, sd = 1)
    # foil_bias makes some foils systematically more plausible than others
    filler_strengths <- rnorm(lineup_size - 1, mean = foil_bias, sd = 1)
    all_strengths <- c(target_strength, filler_strengths)

    # Apply decision rule to compute decision variable
    decision_result <- .apply_decision_rule(
      all_strengths,
      decision_rule,
      lineup_size,
      d_prime
    )

    decision_value <- decision_result$decision_value
    max_position <- decision_result$chosen_position

    response <- .simulation_response(decision_value, criteria, conf_levels)
    makes_id <- response$makes_id
    conf <- response$confidence

    # Determine identification
    if (makes_id) {
      if (max_position == 1) {
        identification <- "suspect"
      } else {
        identification <- "filler"
      }
    } else {
      identification <- "reject"
    }

    # Simulate response time (if requested)
    if (include_response_time) {
      # RT based on memory strength (stronger = faster)
      # Base RT + inverse of strength + noise
      rt <- 5000 + (3000 / (1 + decision_value^2)) + rnorm(1, 0, 1000)
      rt <- max(rt, 500)  # Minimum 500ms
    } else {
      rt <- NA
    }

    all_data[[length(all_data) + 1]] <- data.frame(
      participant_id = i,
      target_present = TRUE,
      lineup_member = if (makes_id) as.integer(max_position) else 0L,
      suspect_pos = 1L,
      identification = identification,
      confidence = ifelse(!is.null(conf_levels), conf, NA),
      response_time = rt,
      stringsAsFactors = FALSE
    )
  }

  # --- Target-Absent Lineups ---
  for (i in 1:n_ta) {
    # Position 1 is the designated innocent suspect and carries no foil bias;
    # positions 2..k are the foils, which do.
    filler_strengths <- c(rnorm(1, mean = 0, sd = 1),
                          rnorm(lineup_size - 1, mean = foil_bias, sd = 1))

    # Apply decision rule to compute decision variable
    decision_result <- .apply_decision_rule(
      filler_strengths,
      decision_rule,
      lineup_size,
      d_prime
    )

    decision_value <- decision_result$decision_value
    max_position <- decision_result$chosen_position

    response <- .simulation_response(decision_value, criteria, conf_levels)
    makes_id <- response$makes_id
    conf <- response$confidence

    # Determine identification
    # In target-absent, position 1 is designated "innocent suspect"
    if (makes_id) {
      if (max_position == 1) {
        identification <- "suspect"
      } else {
        identification <- "filler"
      }
    } else {
      identification <- "reject"
    }

    # Simulate response time
    if (include_response_time) {
      rt <- 5000 + (3000 / (1 + decision_value^2)) + rnorm(1, 0, 1000)
      rt <- max(rt, 500)
    } else {
      rt <- NA
    }

    all_data[[length(all_data) + 1]] <- data.frame(
      participant_id = n_tp + i,
      target_present = FALSE,
      lineup_member = if (makes_id) as.integer(max_position) else 0L,
      suspect_pos = 1L,
      identification = identification,
      confidence = ifelse(!is.null(conf_levels), conf, NA),
      response_time = rt,
      stringsAsFactors = FALSE
    )
  }

  # Combine all trials
  result <- do.call(rbind, all_data)

  # Remove NA columns if not used
  if (all(is.na(result$confidence))) {
    result$confidence <- NULL
  }
  if (all(is.na(result$response_time))) {
    result$response_time <- NULL
  }

  # Shuffle rows
  result <- result[sample(nrow(result)), ]
  rownames(result) <- NULL

  # Add attributes for reproducibility
  attr(result, "simulation_params") <- list(
    n_tp = n_tp,
    n_ta = n_ta,
    d_prime = d_prime,
    c_criterion = c_criterion,
    criteria = criteria,
    lineup_size = lineup_size,
    conf_levels = conf_levels,
    decision_rule = decision_rule,
    foil_bias = foil_bias,
    seed = seed
  )

  class(result) <- c("simulated_lineup_data", "data.frame")
  result
}


#' Print Method for Simulated Lineup Data
#' @param x A simulated_lineup_data object
#' @param ... Additional arguments (ignored)
#' @export
print.simulated_lineup_data <- function(x, ...) {
  params <- attr(x, "simulation_params")

  cat("\n=== Simulated Lineup Data ===\n\n")
  cat("Simulation Parameters:\n")
  cat("  Target-present lineups:", params$n_tp, "\n")
  cat("  Target-absent lineups:", params$n_ta, "\n")
  cat("  d':", params$d_prime, "\n")
  cat("  Criteria:", paste(params$criteria, collapse = ", "), "\n")
  cat("  Lineup size:", params$lineup_size, "\n")
  if (!is.null(params$decision_rule)) {
    cat("  Decision rule:", params$decision_rule, "\n")
  }
  if (!is.null(params$foil_bias) && any(params$foil_bias != 0)) {
    cat("  Foil bias (mean strength per foil):",
        paste(format(params$foil_bias, digits = 2), collapse = ", "), "\n")
  }
  if (!is.null(params$conf_levels)) {
    cat("  Confidence levels:", params$conf_levels, "\n")
  }
  if (!is.null(params$seed)) {
    cat("  Random seed:", params$seed, "\n")
  }

  cat("\nData Summary:\n")
  cat("  Total trials:", nrow(x), "\n")

  # Count IDs by type
  tp_data <- x[x$target_present == TRUE, ]
  ta_data <- x[x$target_present == FALSE, ]

  cat("  Target-present:\n")
  cat("    Suspect IDs:", sum(tp_data$identification == "suspect"), "\n")
  cat("    Filler IDs:", sum(tp_data$identification == "filler"), "\n")
  cat("    Rejections:", sum(tp_data$identification == "reject"), "\n")

  cat("  Target-absent:\n")
  cat("    Suspect IDs:", sum(ta_data$identification == "suspect"), "\n")
  cat("    Filler IDs:", sum(ta_data$identification == "filler"), "\n")
  cat("    Rejections:", sum(ta_data$identification == "reject"), "\n")

  # Compute basic accuracy
  n_correct <- sum(tp_data$identification == "suspect")
  n_false <- sum(ta_data$identification == "suspect")
  n_filler_ta <- sum(ta_data$identification == "filler")
  n_false_total <- n_false + (n_filler_ta / params$lineup_size)
  accuracy <- n_correct / (n_correct + n_false_total)

  cat("\n  Suspect ID Accuracy:", round(accuracy, 3), "\n")

  # Foils are simulated individually, so the spread of choices across members
  # is informative in its own right: a member nobody chose is a null foil.
  if (!is.null(x$lineup_member)) {
    chosen <- x$lineup_member[x$lineup_member > 0L]
    counts <- table(factor(chosen, levels = seq_len(params$lineup_size)))
    cat("\n  Choices by lineup member (0 = rejection, excluded):\n")
    cat("    pos:", paste(format(seq_len(params$lineup_size), width = 4),
                          collapse = ""), "\n")
    cat("    cnt:", paste(format(as.integer(counts), width = 4),
                          collapse = ""), "\n")
    null_foils <- which(as.integer(counts) == 0L)
    if (length(null_foils) > 0L) {
      cat("    Null foils (never chosen):",
          paste(null_foils, collapse = ", "), "\n")
    }
  }

  cat("\nFirst 10 rows:\n")
  print(head(as.data.frame(x), 10))

  invisible(x)
}


#' Simulate Power Analysis for Lineup Studies
#'
#' Estimates statistical power to detect a difference in discriminability
#' between two lineup conditions, by simulating both conditions at a range of
#' sample sizes and recording how often the difference is detected.
#'
#' @param sample_sizes Integer vector of sizes to test (e.g., c(50, 100, 200)).
#'   A size n means n target-present \emph{and} n target-absent witnesses in
#'   each of the two simulated conditions: 2n witnesses per condition and 4n
#'   in the whole simulated experiment.
#' @param d_prime Numeric. Discriminability of the condition of interest.
#' @param d_prime_null Numeric. Discriminability of the comparison condition.
#'   Defaults to 0, which asks how often the procedure can be shown to
#'   discriminate better than chance. Set it to a non-zero value to ask the more
#'   usual question of how often two working procedures can be told apart.
#' @param n_simulations Integer. Number of simulated studies per sample size
#'   (default = 1000). Power is a proportion estimated from these, so its own
#'   standard error is roughly \code{sqrt(0.25 / n_simulations)}.
#' @param test_function Function of two dataframes returning a p-value. Defaults
#'   to a bootstrap comparison of partial AUC via \code{\link{compare_pauc}}.
#' @param alpha Numeric. Significance level (default = 0.05)
#' @param lineup_size Integer. Lineup size for both conditions (default = 6)
#' @param n_bootstrap Integer. Bootstrap resamples used by the default test
#'   (default = 200). This is the dominant cost: the default test is run
#'   \code{n_simulations} times for every sample size.
#' @param seed Optional integer seed for reproducibility.
#' @param verbose Logical. Report progress (default = TRUE).
#' @param ... Additional arguments passed to simulate_lineup_data(), for example
#'   \code{conf_levels} or \code{foil_bias}.
#'
#' @details
#' Power is the probability that a study of a given size detects an effect that
#' is really there. Estimating it by simulation means specifying the effect,
#' generating many studies of that size, applying the analysis you intend to
#' use, and counting how often it reaches significance.
#'
#' That requires a null to test against, which is what \code{d_prime_null}
#' supplies. Simulating a single condition and computing a statistic from it is
#' not a power analysis, because no hypothesis is being tested.
#'
#' @return A dataframe of class \code{power_analysis} with columns:
#'   \itemize{
#'     \item sample_size: n, the number of target-present (and, separately,
#'       target-absent) witnesses per condition
#'     \item n_total: Witnesses in the whole simulated experiment, 4n
#'     \item power: Proportion of simulated studies reaching significance
#'     \item power_se: Standard error of that proportion
#'     \item mean_effect: Mean observed difference in pAUC
#'     \item sd_effect: Standard deviation of the observed difference
#'     \item n_valid: Simulations that produced a usable test
#'     \item n_failed: Attempted simulations without a usable test
#'     \item n_attempted: All attempted simulations
#'   }
#' Power and its Monte Carlo standard error are conditional on usable tests.
#' Failures are retained in the attempted count and trigger a warning; compare
#' failure rates as well as conditional power when assessing a design.
#'
#' @seealso \code{\link{compare_pauc}}, \code{\link{simulate_lineup_data}}
#'
#' @examples
#' \donttest{
#' # Power to show discriminability better than chance
#' power_res <- simulate_power_analysis(
#'   sample_sizes = c(50, 100),
#'   d_prime = 1.5,
#'   n_simulations = 8,
#'   n_bootstrap = 50,
#'   conf_levels = 5,
#'   seed = 123
#' )
#' print(power_res)
#' }
#'
#' @export
simulate_power_analysis <- function(sample_sizes,
                                     d_prime,
                                     d_prime_null = 0,
                                     n_simulations = 1000,
                                     test_function = NULL,
                                     alpha = 0.05,
                                     lineup_size = 6,
                                     n_bootstrap = 200,
                                     seed = NULL,
                                     verbose = TRUE,
                                     ...) {

  if (!is.numeric(sample_sizes) || length(sample_sizes) < 1L ||
      anyNA(sample_sizes) || any(sample_sizes < 2)) {
    stop("sample_sizes must be a numeric vector of sizes of at least 2.",
         call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || alpha <= 0 || alpha >= 1) {
    stop("alpha must be strictly between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(d_prime_null) || length(d_prime_null) != 1L ||
      !is.finite(d_prime_null) || d_prime_null < 0) {
    stop("d_prime_null must be a finite non-negative scalar.", call. = FALSE)
  }
  if (!is.numeric(n_simulations) || length(n_simulations) != 1L ||
      !is.finite(n_simulations) || n_simulations < 1 ||
      n_simulations != floor(n_simulations)) {
    stop("n_simulations must be a positive integer.", call. = FALSE)
  }
  if (d_prime_null == d_prime) {
    warning("The generating d_prime values are equal; this evaluates the test's null rejection rate, which need not equal alpha in small or sparse samples.", call. = FALSE)
  }

  if (!is.null(seed)) {
    restore_rng <- .local_seed(seed)
    on.exit(restore_rng(), add = TRUE)
  }

  # The default test compares partial AUC between the two conditions and
  # returns its p-value. Any function of two dataframes returning a p-value can
  # be supplied instead.
  if (is.null(test_function)) {
    test_function <- function(data1, data2) {
      cmp <- tryCatch(
        compare_pauc(data1, data2, lineup_size = lineup_size,
                     n_bootstrap = n_bootstrap),
        error = function(e) NULL
      )
      if (is.null(cmp)) return(c(p = NA_real_, effect = NA_real_))
      c(p = cmp$p_value, effect = cmp$pauc1 - cmp$pauc2)
    }
  }

  results <- list()

  for (n in sample_sizes) {
    if (isTRUE(verbose)) {
      message("Simulating sample size ", n, " (", n_simulations, " studies)")
    }

    p_values <- rep(NA_real_, n_simulations)
    effects  <- rep(NA_real_, n_simulations)

    for (sim in seq_len(n_simulations)) {
      data_effect <- as.data.frame(simulate_lineup_data(
        n_tp = n, n_ta = n, d_prime = d_prime,
        lineup_size = lineup_size, ...
      ))
      data_null <- as.data.frame(simulate_lineup_data(
        n_tp = n, n_ta = n, d_prime = d_prime_null,
        lineup_size = lineup_size, ...
      ))

      out <- suppressWarnings(suppressMessages(
        test_function(data_effect, data_null)
      ))
      # A test_function may return a bare p-value or a named p/effect pair.
      if (length(out) >= 2L && !is.null(names(out))) {
        p_values[sim] <- out[["p"]]
        effects[sim]  <- out[["effect"]]
      } else {
        p_values[sim] <- out[[1]]
      }
      if (!is.na(p_values[sim]) && (!is.finite(p_values[sim]) ||
          p_values[sim] < 0 || p_values[sim] > 1)) {
        stop("test_function must return a p-value in [0, 1] or NA for a failed analysis.", call. = FALSE)
      }
    }

    valid <- !is.na(p_values)
    n_valid <- sum(valid)
    power <- if (n_valid > 0) mean(p_values[valid] < alpha) else NA_real_
    if (n_valid < n_simulations) {
      warning(n_simulations - n_valid, " of ", n_simulations,
              " analyses failed; power is conditional on successful analyses.", call. = FALSE)
    }

    results[[length(results) + 1]] <- data.frame(
      sample_size = n,
      n_total     = 4 * n,
      power       = power,
      # Binomial standard error of the estimated proportion, so readers can see
      # how much of any non-monotonicity is simulation noise.
      power_se    = if (n_valid > 0) sqrt(power * (1 - power) / n_valid) else NA_real_,
      mean_effect = mean(effects, na.rm = TRUE),
      sd_effect   = stats::sd(effects, na.rm = TRUE),
      n_valid     = n_valid,
      n_failed    = n_simulations - n_valid,
      n_attempted = n_simulations
    )
  }

  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL
  attr(result_df, "alpha") <- alpha
  attr(result_df, "d_prime") <- d_prime
  attr(result_df, "d_prime_null") <- d_prime_null
  attr(result_df, "n_simulations") <- n_simulations
  class(result_df) <- c("power_analysis", "data.frame")
  result_df
}


# Declare global variables used in ggplot2 aes()
utils::globalVariables(c("sample_size", "power", "power_se"))

#' Plot Power Analysis Results
#'
#' @param x A power_analysis object
#' @param ... Additional arguments (ignored)
#'
#' @return A ggplot object, returned invisibly. Because it is an ordinary
#'   ggplot, further layers and themes can be added to it.
#'
#' @export
plot.power_analysis <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting power analysis results")
  }

  d_prime <- attr(x, "d_prime")
  d_null  <- attr(x, "d_prime_null")
  n_sims  <- attr(x, "n_simulations")

  subtitle <- if (!is.null(d_prime)) {
    sprintf("Detecting d' = %.2f against d' = %.2f, %s simulated studies per point",
            d_prime, d_null, format(n_sims))
  } else NULL

  p <- ggplot2::ggplot(x, ggplot2::aes(x = sample_size, y = power))

  # Monte Carlo error matters here: without it a reader reads noise between
  # adjacent points as a real dip in power.
  if ("power_se" %in% names(x)) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = pmax(0, power - 1.96 * power_se),
                   ymax = pmin(1, power + 1.96 * power_se)),
      fill = "steelblue", alpha = 0.18
    )
  }

  p <- p +
    ggplot2::geom_hline(yintercept = 0.80, linetype = "dashed",
                        color = "red", linewidth = 0.8) +
    ggplot2::geom_line(linewidth = 1, color = "steelblue") +
    ggplot2::geom_point(size = 3, color = "steelblue") +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::labs(
      x = "Target-present (= target-absent) witnesses per condition",
      y = "Power",
      title = "Power to detect a difference in discriminability",
      subtitle = subtitle,
      caption = "Dashed line = 80% power. Band = 95% Monte Carlo interval."
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))

  print(p)
  invisible(p)
}
