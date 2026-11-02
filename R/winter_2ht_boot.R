#' Bootstrap Confidence Intervals for 2-HT Model Parameters
#'
#' Computes bootstrap confidence intervals for the parameters of the 2-HT model
#' by resampling the observed data with replacement.
#'
#' @param x A winter_2ht object from fit_winter_2ht(), OR count data/data frame
#'   as in fit_winter_2ht().
#' @param nboot Number of bootstrap samples. Default is 1000.
#' @param conf_level Confidence level for intervals. Default is 0.95.
#' @param lineup_size Lineup size (only needed if x is count data, not a winter_2ht object).
#' @param method Type of bootstrap CI: "percentile" (default) or "bca" (bias-corrected and accelerated).
#' @param parallel Logical indicating whether to use parallel processing. Default is FALSE.
#' @param ncpus Number of CPUs to use if parallel = TRUE. Default is 2.
#' @param seed Random seed for reproducibility. Default is NULL.
#' @param ... Additional arguments passed to fit_winter_2ht().
#'
#' @return An object of class "winter_2ht_boot" containing:
#'   \item{boot_estimates}{Matrix of bootstrap parameter estimates (nboot x 4)}
#'   \item{ci}{Matrix of confidence intervals for each parameter}
#'   \item{original_fit}{The original model fit}
#'   \item{nboot}{Number of bootstrap samples}
#'   \item{conf_level}{Confidence level used}
#'
#' @details
#' The bootstrap procedure resamples within TP and TA strata with replacement
#' to keep both experimental totals fixed. For each bootstrap sample, the 2-HT model is re-fitted,
#' and the distribution of parameter estimates across bootstrap samples is used to
#' construct confidence intervals.
#'
#' Two types of confidence intervals are available:
#' \itemize{
#'   \item \strong{percentile}: Uses quantiles of the bootstrap distribution
#'   \item \strong{bca}: Bias-corrected and accelerated intervals (Efron,
#'     1987). The bias correction counts bootstrap ties with the estimate as
#'     half, and the acceleration comes from the jackknife over individual
#'     observations, centered and scaled within TP/TA strata, using at most six
#'     distinct leave-one-out response-count fits.
#' }
#'
#' @references
#' Efron, B. (1987). Better bootstrap confidence intervals. \emph{Journal of
#' the American Statistical Association, 82}(397), 171-185.
#'
#' @examples
#' \donttest{
#' counts <- c(
#'   n_tp_suspect = 147, n_tp_filler = 94, n_tp_reject = 141,
#'   n_ta_suspect = 38, n_ta_filler = 138, n_ta_reject = 206
#' )
#' fit <- fit_winter_2ht(counts, lineup_size = 6)
#'
#' # Bootstrap CIs
#' boot_fit <- boot_winter_2ht(fit, nboot = 200, seed = 123)
#' print(boot_fit)
#' plot(boot_fit)
#' }
#'
#' @export
boot_winter_2ht <- function(x,
                            nboot = 1000,
                            conf_level = 0.95,
                            lineup_size = 6,
                            method = c("percentile", "bca"),
                            parallel = FALSE,
                            ncpus = 2,
                            seed = NULL,
                            ...) {

  method <- match.arg(method)

  # Set seed if provided
  if (!is.null(seed)) {
    restore_rng <- .local_seed(seed)
    on.exit(restore_rng(), add = TRUE)
  }

  # Get original fit if x is a winter_2ht object
  if (inherits(x, "winter_2ht")) {
    original_fit <- x
    # Reconstruct raw data for resampling
    raw_data <- .reconstruct_raw_data(x)
  } else {
    # Fit original model
    original_fit <- fit_winter_2ht(x, lineup_size = lineup_size, ...)
    raw_data <- .reconstruct_raw_data(original_fit)
  }

  n_total <- nrow(raw_data)
  if (!is.numeric(nboot) || length(nboot) != 1L || !is.finite(nboot) ||
      nboot < 2 || nboot != floor(nboot)) stop("nboot must be an integer of at least 2.", call. = FALSE)
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) stop("conf_level must be in (0, 1).", call. = FALSE)

  # Bootstrap function
  boot_func <- function(i) {
    # Resample with replacement
    strata <- split(seq_len(n_total), raw_data$target_present)
    boot_indices <- unlist(lapply(strata, .resample_values), use.names = FALSE)
    boot_data <- raw_data[boot_indices, ]

    # Extract counts from bootstrap sample
    boot_counts <- .extract_counts_from_df(
      boot_data,
      target_present = "target_present",
      identification = "identification"
    )

    # Fit model to bootstrap sample
    tryCatch({
      boot_fit <- suppressWarnings(fit_winter_2ht(
        boot_counts,
        lineup_size = original_fit$lineup_size,
        ...
      ))
      if (boot_fit$convergence != 0L) return(rep(NA_real_, 4))
      return(boot_fit$parameters)
    }, error = function(e) {
      return(rep(NA, 4))
    })
  }

  # Run bootstrap
  if (parallel) {
    # Parallel bootstrap
    cl <- parallel::makeCluster(ncpus)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterCall(cl, function(paths) {
      .libPaths(paths)
      loadNamespace("r4lineups")
      NULL
    }, .libPaths())
    parallel::clusterSetRNGStream(cl, iseed = if (is.null(seed))
      sample.int(.Machine$integer.max, 1L) else seed)

    # Export necessary functions and data
    parallel::clusterExport(cl, varlist = c("fit_winter_2ht", ".compute_2ht_probs",
                                           ".extract_counts_from_df", ".resample_values"),
                           envir = environment())

    boot_results <- parallel::parLapply(cl, 1:nboot, boot_func)
  } else {
    # Serial bootstrap
    boot_results <- lapply(1:nboot, function(i) {
      if (i %% 100 == 0) {
        message(sprintf("Bootstrap iteration %d/%d", i, nboot))
      }
      boot_func(i)
    })
  }

  # Convert to matrix
  boot_estimates <- do.call(rbind, boot_results)
  colnames(boot_estimates) <- c("dP", "dA", "b", "g")

  # Remove failed fits
  valid_rows <- complete.cases(boot_estimates)
  if (sum(valid_rows) < nboot) {
    warning(sprintf("%d bootstrap samples failed to converge and were excluded",
                    nboot - sum(valid_rows)))
  }

  # Compute confidence intervals: one row per parameter, Lower and Upper
  if (method == "percentile") {
    ci <- t(apply(boot_estimates, 2, function(x) {
      stats::quantile(x, probs = c((1 - conf_level)/2, 1 - (1 - conf_level)/2), na.rm = TRUE)
    }))
  } else if (method == "bca") {
    accel <- .jackknife_acceleration_2ht(original_fit, ...)
    ci <- .compute_bca_ci(boot_estimates, original_fit$parameters, conf_level,
                          accel)
  }

  dimnames(ci) <- list(colnames(boot_estimates), c("Lower", "Upper"))

  # Create result object
  result <- list(
    boot_estimates = boot_estimates,
    ci = ci,
    original_fit = original_fit,
    nboot = sum(valid_rows),
    nboot_requested = nboot,
    n_failed = sum(!valid_rows),
    conf_level = conf_level,
    method = method
  )

  class(result) <- "winter_2ht_boot"
  return(result)
}


#' Reconstruct Raw Data from winter_2ht Object
#'
#' @param x A winter_2ht object
#' @return Data frame with individual observations
#' @keywords internal
.reconstruct_raw_data <- function(x) {
  counts <- unlist(x$observed_counts)
  if (any(counts != floor(counts))) stop("Nonparametric bootstrap requires whole observed response counts.", call. = FALSE)

  # Target-present data
  tp_data <- data.frame(
    target_present = TRUE,
    identification = c(
      rep("suspect", x$observed_counts$tp[1]),
      rep("filler", x$observed_counts$tp[2]),
      rep("reject", x$observed_counts$tp[3])
    )
  )

  # Target-absent data
  ta_data <- data.frame(
    target_present = FALSE,
    identification = c(
      rep("suspect", x$observed_counts$ta[1]),
      rep("filler", x$observed_counts$ta[2]),
      rep("reject", x$observed_counts$ta[3])
    )
  )

  return(rbind(tp_data, ta_data))
}


#' Jackknife Acceleration for 2-HT BCa Intervals
#'
#' Leaving out one observation changes one of the six response counts by one,
#' so there are at most six distinct leave-one-out fits. Each is weighted by
#' the number of observations in its cell (Efron, 1987).
#'
#' @param original_fit A winter_2ht object
#' @param ... Additional arguments passed to fit_winter_2ht()
#'
#' @return Named vector of acceleration constants, one per parameter
#' @keywords internal
.jackknife_acceleration_2ht <- function(original_fit, ...) {
  counts <- c(
    n_tp_suspect = original_fit$observed_counts$tp[[1]],
    n_tp_filler = original_fit$observed_counts$tp[[2]],
    n_tp_reject = original_fit$observed_counts$tp[[3]],
    n_ta_suspect = original_fit$observed_counts$ta[[1]],
    n_ta_filler = original_fit$observed_counts$ta[[2]],
    n_ta_reject = original_fit$observed_counts$ta[[3]]
  )
  cells <- which(counts > 0)
  theta <- tryCatch(
    t(vapply(cells, function(j) {
      loo <- counts
      loo[j] <- loo[j] - 1
      fit_winter_2ht(loo, lineup_size = original_fit$lineup_size, ...)$parameters
    }, numeric(4))),
    error = function(e) NULL
  )
  params <- c("dP", "dA", "b", "g")
  if (is.null(theta)) {
    warning("Jackknife refits failed; BCa acceleration set to 0.", call. = FALSE)
    return(stats::setNames(rep(0, 4), params))
  }
  w <- counts[cells]
  accel <- vapply(seq_len(4), function(k) {
    th <- theta[, k]
    group <- ifelse(cells <= 3L, "TP", "TA")
    d <- numeric(length(th))
    for (g in unique(group)) {
      ix <- which(group == g)
      # Center within fixed-size strata; scale influences by the arm size.
      ns <- sum(w[ix])
      d[ix] <- (ns - 1) / ns * (sum(w[ix] * th[ix]) / ns - th[ix])
    }
    denom <- 6 * sum(w * d^2)^1.5
    if (!is.finite(denom) || denom <= 0) NaN else sum(w * d^3) / denom
  }, numeric(1))
  if (any(is.nan(accel))) {
    warning("Jackknife variance is zero for ", paste(params[is.nan(accel)],
            collapse = ", "), " (a boundary estimate?); BCa acceleration set ",
            "to 0 for that parameter.", call. = FALSE)
    accel[is.nan(accel)] <- 0
  }
  stats::setNames(accel, params)
}


#' Compute BCa Confidence Intervals
#'
#' @param boot_estimates Matrix of bootstrap estimates
#' @param original_est Vector of original parameter estimates
#' @param conf_level Confidence level
#' @param accel Vector of acceleration constants, one per parameter
#'
#' @return Matrix of confidence intervals
#' @keywords internal
.compute_bca_ci <- function(boot_estimates, original_est, conf_level,
                            accel = rep(0, ncol(boot_estimates))) {

  n_params <- ncol(boot_estimates)
  ci_matrix <- matrix(NA, nrow = n_params, ncol = 2)

  alpha <- (1 - conf_level) / 2

  for (i in 1:n_params) {
    # Bias correction. Ties with the estimate count half, and the proportion
    # is kept off 0 and 1 so that z0 stays finite.
    boot_i <- boot_estimates[!is.na(boot_estimates[, i]), i]
    B <- length(boot_i)
    p0 <- mean(boot_i < original_est[i]) + 0.5 * mean(boot_i == original_est[i])
    p0 <- min(max(p0, 1 / (B + 1)), B / (B + 1))
    z0 <- stats::qnorm(p0)
    a <- accel[[i]]

    # Adjusted quantiles
    z_alpha <- stats::qnorm(alpha)
    z_1alpha <- stats::qnorm(1 - alpha)

    p_lower <- stats::pnorm(z0 + (z0 + z_alpha) / (1 - a * (z0 + z_alpha)))
    p_upper <- stats::pnorm(z0 + (z0 + z_1alpha) / (1 - a * (z0 + z_1alpha)))

    ci_matrix[i, ] <- stats::quantile(boot_estimates[, i],
                                     probs = c(p_lower, p_upper),
                                     na.rm = TRUE)
  }

  return(ci_matrix)
}


#' Print Method for winter_2ht_boot Objects
#'
#' @param x A winter_2ht_boot object
#' @param ... Additional arguments (not used)
#'
#' @export
print.winter_2ht_boot <- function(x, ...) {
  cat("\nBootstrap Confidence Intervals for 2-HT Model\n")
  cat("==============================================\n\n")

  cat(sprintf("Number of bootstrap samples: %d\n", x$nboot))
  cat(sprintf("Confidence level: %.1f%%\n", x$conf_level * 100))
  cat(sprintf("CI method: %s\n\n", x$method))

  cat("Parameter Estimates with Bootstrap CIs:\n")
  result_table <- data.frame(
    Estimate = round(x$original_fit$parameters, 4),
    Boot_Lower = round(x$ci[, 1], 4),
    Boot_Upper = round(x$ci[, 2], 4),
    Boot_SE = round(apply(x$boot_estimates, 2, sd, na.rm = TRUE), 4)
  )
  rownames(result_table) <- c("dP", "dA", "b", "g")
  print(result_table)

  invisible(x)
}


#' Summary Method for winter_2ht_boot Objects
#'
#' @param object A winter_2ht_boot object
#' @param ... Additional arguments (not used)
#'
#' @export
summary.winter_2ht_boot <- function(object, ...) {
  print(object)

  cat("\nBootstrap Distribution Summary:\n")
  boot_summary <- apply(object$boot_estimates, 2, function(x) {
    c(Mean = mean(x, na.rm = TRUE),
      SD = sd(x, na.rm = TRUE),
      Min = min(x, na.rm = TRUE),
      Max = max(x, na.rm = TRUE))
  })
  print(round(t(boot_summary), 4))

  invisible(object)
}


#' Plot Bootstrap Distributions
#'
#' Creates histograms showing the bootstrap distributions of parameter estimates.
#'
#' @param x A winter_2ht_boot object
#' @param ... Additional arguments (not used)
#'
#' @export
plot.winter_2ht_boot <- function(x, ...) {

  # Prepare data for plotting
  boot_long <- data.frame(
    parameter = rep(colnames(x$boot_estimates), each = nrow(x$boot_estimates)),
    value = c(x$boot_estimates)
  )

  # Add original estimates and CIs for reference
  param_info <- data.frame(
    parameter = rownames(x$ci),
    original = x$original_fit$parameters,
    lower = x$ci[, 1],
    upper = x$ci[, 2]
  )

  # Create plot
  p <- ggplot2::ggplot(boot_long, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
    ggplot2::facet_wrap(~ parameter, scales = "free", ncol = 2) +
    ggplot2::geom_vline(
      data = param_info,
      ggplot2::aes(xintercept = original),
      color = "red",
      linetype = "dashed",
      size = 1
    ) +
    ggplot2::geom_vline(
      data = param_info,
      ggplot2::aes(xintercept = lower),
      color = "darkgreen",
      linetype = "dotted"
    ) +
    ggplot2::geom_vline(
      data = param_info,
      ggplot2::aes(xintercept = upper),
      color = "darkgreen",
      linetype = "dotted"
    ) +
    ggplot2::labs(
      title = sprintf("Bootstrap Distributions (n = %d)", x$nboot),
      subtitle = paste0("Red dashed = original estimate; Green dotted = ", 100 * x$conf_level, "% CI"),
      x = "Parameter Value",
      y = "Frequency"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 9)
    )

  return(p)
}
