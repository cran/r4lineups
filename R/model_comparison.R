# Declare global variables used in ggplot2 aes()
utils::globalVariables(c("Parameter", "Estimate", "Lower", "Upper"))

#' Compare Multiple Models for Lineup Identification Data
#'
#' Fits and compares multiple models (2-HT, EIG, Full ROC) to the same lineup
#' identification dataset, providing a side-by-side descriptive table.
#'
#' @param data A dataframe with columns: target_present, identification, confidence
#' @param models Character vector of models to fit. Options:
#'   \itemize{
#'     \item "2ht" or "winter": Winter et al. (2022) Two-High Threshold MPT model
#'     \item "eig": Expected Information Gain (Starns et al., 2023)
#'     \item "fullroc": Full ROC curve (Smith & Yang, 2020)
#'   }
#'   Default = c("2ht", "eig", "fullroc") fits all models.
#' @param lineup_size Integer. Number of people in the lineup (default = 6)
#' @param prior_guilt Numeric. Prior probability of guilt for EIG (default = 0.5)
#' @param confidence_bins Numeric vector of confidence bin edges (optional).
#'   Used for EIG and Full ROC if specified.
#' @param show_warnings Logical. Whether to show model fitting warnings (default = FALSE)
#' @param ... Additional arguments passed to individual model functions
#'
#' @return An object of class "model_comparison" containing:
#'   \itemize{
#'     \item comparison_table: Dataframe comparing model fits
#'     \item fitted_models: List of fitted model objects
#'     \item best_model: Always NULL. The included methods estimate different
#'       quantities and cannot be ranked by a common information criterion.
#'     \item data: The input data
#'     \item models_fit: Character vector of models successfully fit
#'   }
#'
#' @details
#' This function provides a unified interface for fitting and comparing multiple
#' eyewitness identification models. It automatically handles different data
#' requirements and output formats across models.
#'
#' **Models Compared:**
#' \itemize{
#'   \item **2-HT (Winter et al., 2022)**: Multinomial processing tree model
#'     with parameters for detection (dP, dA), bias (b), and guessing (g).
#'     Provides AIC/BIC for comparison only with other likelihood models fit
#'     to the same observations and outcome representation.
#'   \item **EIG (Starns et al., 2023)**: Information-theoretic measure of
#'     evidentiary value. Higher values indicate more diagnostic procedures.
#'   \item **Full ROC (Smith & Yang, 2020)**: Uses ALL responses to compute
#'     investigator discriminability. AUC ranges from 0 to 1; 0.5 represents
#'     chance under a fixed ordering.
#' }
#'
#' **Interpretation:**
#' \itemize{
#'   \item AIC/BIC describe the 2-HT likelihood fit but cannot be compared with
#'     EIG or full-ROC AUC
#'   \item Use EIG for comparing procedure diagnosticity (higher is better)
#'   \item Use Full ROC AUC for investigator discriminability (higher is better)
#' }
#'
#' @examples
#' data(lineup_example)
#' # Fit all models
#' comparison <- compare_models(lineup_example)
#' print(comparison)
#' summary(comparison)
#'
#' # Fit specific models
#' comparison <- compare_models(lineup_example, models = c("2ht", "eig"))
#'
#' # Access individual fitted models
#' comparison$fitted_models$`2ht`
#' comparison$fitted_models$eig
#'
#' @references
#' Winter, K., Menne, N. M., Bell, R., & Buchner, A. (2022). Experimental validation
#' of a multinomial processing tree model for analyzing eyewitness identification
#' decisions. \emph{Scientific Reports, 12}, 15571.
#'
#' Starns, J. J., Cohen, A. L., & Rotello, C. M. (2023). A complete method for
#' assessing the effectiveness of eyewitness identification procedures: Expected
#' information gain. \emph{Psychological Review, 130}(3), 677--719.
#'
#' Smith, A. M., Yang, Y., & Wells, G. L. (2020). Distinguishing between investigator
#' discriminability and eyewitness discriminability. \emph{Perspectives on
#' Psychological Science, 15}(3), 589-607.
#'
#' @export
compare_models <- function(data,
                           models = c("2ht", "eig", "fullroc"),
                           lineup_size = 6,
                           prior_guilt = 0.5,
                           confidence_bins = NULL,
                           show_warnings = FALSE,
                           ...) {

  # Validate data
  required_cols <- c("target_present", "identification", "confidence")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Standardize model names
  models <- tolower(models)
  models <- gsub("winter", "2ht", models)
  valid_models <- c("2ht", "eig", "fullroc")
  invalid_models <- setdiff(models, valid_models)
  if (length(invalid_models) > 0) {
    stop("Invalid model names: ", paste(invalid_models, collapse = ", "),
         "\nValid options: ", paste(valid_models, collapse = ", "))
  }

  # Initialize storage
  fitted_models <- list()
  comparison_table <- data.frame()
  models_fit <- character()

  # Fit 2-HT model
  if ("2ht" %in% models) {
    message("Fitting 2-HT model...")
    fitted_2ht <- tryCatch({
      if (show_warnings) {
        fit_winter_2ht(data, lineup_size = lineup_size,
                       target_present = "target_present",
                       identification = "identification", ...)
      } else {
        suppressWarnings(
          fit_winter_2ht(data, lineup_size = lineup_size,
                         target_present = "target_present",
                         identification = "identification", ...)
        )
      }
    }, error = function(e) {
      warning("Failed to fit 2-HT model: ", e$message)
      NULL
    })

    if (!is.null(fitted_2ht)) {
      fitted_models[["2ht"]] <- fitted_2ht
      models_fit <- c(models_fit, "2ht")

      # Add to comparison table
      comparison_table <- rbind(comparison_table, data.frame(
        Model = "2-HT (Winter et al., 2022)",
        Measure = "Log-likelihood",
        Value = round(fitted_2ht$loglik, 2),
        Interpretation = "Model fit quality",
        stringsAsFactors = FALSE
      ))
      comparison_table <- rbind(comparison_table, data.frame(
        Model = "2-HT (Winter et al., 2022)",
        Measure = "AIC",
        Value = round(fitted_2ht$aic, 2),
        Interpretation = "Lower is better",
        stringsAsFactors = FALSE
      ))
      comparison_table <- rbind(comparison_table, data.frame(
        Model = "2-HT (Winter et al., 2022)",
        Measure = "BIC",
        Value = round(fitted_2ht$bic, 2),
        Interpretation = "Lower is better",
        stringsAsFactors = FALSE
      ))
      comparison_table <- rbind(comparison_table, data.frame(
        Model = "2-HT (Winter et al., 2022)",
        Measure = "dP (detection presence)",
        Value = round(fitted_2ht$parameters["dP"], 3),
        Interpretation = "0 to 1",
        stringsAsFactors = FALSE
      ))
      comparison_table <- rbind(comparison_table, data.frame(
        Model = "2-HT (Winter et al., 2022)",
        Measure = "dA (detection absence)",
        Value = round(fitted_2ht$parameters["dA"], 3),
        Interpretation = "0 to 1",
        stringsAsFactors = FALSE
      ))
    }
  }

  # Fit EIG model
  if ("eig" %in% models) {
    message("Computing EIG...")
    fitted_eig <- tryCatch({
      if (show_warnings) {
        compute_eig(data, prior_guilt = prior_guilt,
                   confidence_bins = confidence_bins,
                   lineup_size = lineup_size)
      } else {
        suppressWarnings(
          compute_eig(data, prior_guilt = prior_guilt,
                     confidence_bins = confidence_bins,
                     lineup_size = lineup_size)
        )
      }
    }, error = function(e) {
      warning("Failed to compute EIG: ", e$message)
      NULL
    })

    if (!is.null(fitted_eig)) {
      fitted_models[["eig"]] <- fitted_eig
      models_fit <- c(models_fit, "eig")

      # Add to comparison table
      comparison_table <- rbind(comparison_table, data.frame(
        Model = "EIG (Starns et al., 2023)",
        Measure = "Expected Information Gain",
        Value = round(fitted_eig$eig, 4),
        Interpretation = "Higher is better (bits)",
        stringsAsFactors = FALSE
      ))
      comparison_table <- rbind(comparison_table, data.frame(
        Model = "EIG (Starns et al., 2023)",
        Measure = "Information Efficiency",
        Value = round(fitted_eig$eig / fitted_eig$prior_entropy * 100, 1),
        Interpretation = "Percentage (0-100%)",
        stringsAsFactors = FALSE
      ))
      comparison_table <- rbind(comparison_table, data.frame(
        Model = "EIG (Starns et al., 2023)",
        Measure = "Prior Entropy",
        Value = round(fitted_eig$prior_entropy, 4),
        Interpretation = "Maximum possible IG",
        stringsAsFactors = FALSE
      ))
    }
  }

  # Fit Full ROC
  if ("fullroc" %in% models) {
    message("Computing Full ROC...")
    fitted_fullroc <- tryCatch({
      if (show_warnings) {
        make_fullroc(data, conf_bins = confidence_bins,
                    lineup_size = lineup_size,
                    show_plot = FALSE, ...)
      } else {
        suppressWarnings(
          make_fullroc(data, conf_bins = confidence_bins,
                      lineup_size = lineup_size,
                      show_plot = FALSE, ...)
        )
      }
    }, error = function(e) {
      warning("Failed to compute Full ROC: ", e$message)
      NULL
    })

    if (!is.null(fitted_fullroc)) {
      fitted_models[["fullroc"]] <- fitted_fullroc
      models_fit <- c(models_fit, "fullroc")

      # Add to comparison table
      comparison_table <- rbind(comparison_table, data.frame(
        Model = "Full ROC (Smith & Yang, 2020)",
        Measure = "Full AUC",
        Value = round(fitted_fullroc$auc, 4),
        Interpretation = "Higher is better (0-1)",
        stringsAsFactors = FALSE
      ))
      comparison_table <- rbind(comparison_table, data.frame(
        Model = "Full ROC (Smith & Yang, 2020)",
        Measure = "Operating Points",
        Value = fitted_fullroc$summary$n_operating_points,
        Interpretation = "Number of decision criteria",
        stringsAsFactors = FALSE
      ))
      comparison_table <- rbind(comparison_table, data.frame(
        Model = "Full ROC (Smith & Yang, 2020)",
        Measure = "Max Hit Rate",
        Value = round(fitted_fullroc$summary$max_hit_rate, 3),
        Interpretation = "Cumulative (0-1)",
        stringsAsFactors = FALSE
      ))
    }
  }

  # These methods have different likelihoods/estimands; no common best model.
  best_model <- NULL

  # Create result object
  result <- list(
    comparison_table = comparison_table,
    fitted_models = fitted_models,
    best_model = best_model,
    data = data,
    models_fit = models_fit,
    lineup_size = lineup_size,
    prior_guilt = prior_guilt,
    confidence_bins = confidence_bins
  )

  class(result) <- c("model_comparison", "list")
  message("\nModel comparison complete!")
  return(result)
}


#' Print Method for model_comparison Objects
#'
#' @param x A model_comparison object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.model_comparison <- function(x, ...) {
  cat("\n=== Lineup Model Comparison ===\n\n")
  cat("Models fit:", paste(x$models_fit, collapse = ", "), "\n")
  cat("Sample size:", nrow(x$data), "\n")
  cat("Lineup size:", x$lineup_size, "\n\n")

  cat("Comparison Table:\n")
  print(x$comparison_table, row.names = FALSE)

  cat("\nMetrics describe different estimands and are not a single model-selection scale.\n")

  cat("\nAccess fitted models via: $fitted_models$<model_name>\n")
  cat("Available models:", paste(names(x$fitted_models), collapse = ", "), "\n")

  invisible(x)
}


#' Summary Method for model_comparison Objects
#'
#' @param object A model_comparison object
#' @param ... Additional arguments (ignored)
#'
#' @export
summary.model_comparison <- function(object, ...) {
  cat("\n=== Model Comparison Summary ===\n\n")

  # Overall info
  cat("Dataset:\n")
  cat("  Total observations:", nrow(object$data), "\n")
  cat("  Target-present:", sum(object$data$target_present), "\n")
  cat("  Target-absent:", sum(!object$data$target_present), "\n")
  cat("  Lineup size:", object$lineup_size, "\n\n")

  cat("Models fitted:", length(object$models_fit), "\n")
  cat("  ", paste(object$models_fit, collapse = ", "), "\n\n")

  # Model-specific summaries
  if ("2ht" %in% object$models_fit) {
    cat("--- 2-HT Model Summary ---\n")
    model_2ht <- object$fitted_models[["2ht"]]
    cat("  Parameters: dP =", round(model_2ht$parameters["dP"], 3),
        ", dA =", round(model_2ht$parameters["dA"], 3),
        ", b =", round(model_2ht$parameters["b"], 3),
        ", g =", round(model_2ht$parameters["g"], 3), "\n")
    cat("  AIC:", round(model_2ht$aic, 2), "\n")
    cat("  BIC:", round(model_2ht$bic, 2), "\n")
    if (model_2ht$convergence != 0) {
      cat("  WARNING: Model did not converge (code", model_2ht$convergence, ")\n")
    }
    cat("\n")
  }

  if ("eig" %in% object$models_fit) {
    cat("--- EIG Summary ---\n")
    model_eig <- object$fitted_models[["eig"]]
    cat("  EIG:", round(model_eig$eig, 4), "bits\n")
    cat("  Information efficiency:",
        round(model_eig$eig / model_eig$prior_entropy * 100, 1), "%\n")
    cat("  Number of response categories:", nrow(model_eig$response_data), "\n\n")
  }

  if ("fullroc" %in% object$models_fit) {
    cat("--- Full ROC Summary ---\n")
    model_fullroc <- object$fitted_models[["fullroc"]]
    cat("  Full AUC:", round(model_fullroc$auc, 4), "\n")
    cat("  Operating points:", model_fullroc$summary$n_operating_points, "\n")
    cat("  Ordering method:", model_fullroc$summary$order_method, "\n\n")
  }

  cat("---\n")
  cat("Interpretation Guidance:\n")
  cat("  - 2-HT: AIC/BIC are comparable only to other likelihood models fit to the same outcomes\n")
  cat("  - EIG: Higher values = more informative procedure\n")
  cat("  - Full ROC: Higher AUC = better investigator discriminability\n")
  cat("\nEach model provides different information; consider reporting more than one.\n")

  invisible(object)
}


#' Plot Side-by-Side Model Comparisons
#'
#' Creates a multi-panel visualization comparing fitted models.
#'
#' @param x A model_comparison object from compare_models()
#' @param which Character vector specifying which plots to create.
#'   Options: "2ht", "eig_ig", "eig_posteriors", "fullroc"
#'   Default = "all" creates all available plots.
#' @param ncol Integer. Number of columns for plot layout (default = 2)
#' @param ... Additional arguments passed to individual plotting functions
#'
#' @return A combined ggplot object (via patchwork or cowplot)
#'
#' @details
#' This function creates side-by-side visualizations of all fitted models
#' for easy comparison. The specific plots depend on which models were fit:
#' \itemize{
#'   \item **2-HT**: Parameter estimates with confidence intervals
#'   \item **EIG**: Information gain by response category
#'   \item **Full ROC**: ROC curve with AUC
#' }
#'
#' @export
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
plot.model_comparison <- function(x, which = "all", ncol = 2, ...) {

  if (!inherits(x, "model_comparison")) {
    stop("x must be a model_comparison object")
  }

  plots <- list()

  # Determine which plots to create
  if ("all" %in% which) {
    which <- x$models_fit
  }

  # 2-HT parameter plot
  if ("2ht" %in% which && "2ht" %in% x$models_fit) {
    model_2ht <- x$fitted_models[["2ht"]]
    param_df <- data.frame(
      Parameter = c("dP", "dA", "b", "g"),
      Estimate = model_2ht$parameters,
      SE = model_2ht$se,
      # The parameters are probabilities, so clip the Wald interval to [0, 1].
      # ylim() would instead drop any bar that crosses 0 or 1.
      Lower = pmax(0, model_2ht$parameters - 1.96 * model_2ht$se),
      Upper = pmin(1, model_2ht$parameters + 1.96 * model_2ht$se)
    )
    param_df$Parameter <- factor(param_df$Parameter,
                                  levels = c("dP", "dA", "b", "g"),
                                  labels = c("dP\n(presence)",
                                           "dA\n(absence)",
                                           "b\n(bias)",
                                           "g\n(guessing)"))

    p_2ht <- ggplot(param_df, aes(x = Parameter, y = Estimate)) +
      geom_point(size = 3, color = "darkblue") +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      coord_cartesian(ylim = c(0, 1)) +
      labs(title = "2-HT parameters",
           subtitle = paste0("AIC = ", round(model_2ht$aic, 1)),
           y = "Parameter estimate",
           x = NULL)

    plots[["2ht"]] <- p_2ht
  }

  # EIG information gain plot
  if ("eig" %in% which && "eig" %in% x$models_fit) {
    model_eig <- x$fitted_models[["eig"]]
    plots[["eig_ig"]] <- plot_eig(model_eig, max_responses = 10) +
      labs(title = "EIG by response", x = NULL, y = "Information gain (bits)")
  }

  # Full ROC plot
  if ("fullroc" %in% which && "fullroc" %in% x$models_fit) {
    model_fullroc <- x$fitted_models[["fullroc"]]
    # The AUC goes in the subtitle; plot_fullroc()'s large in-panel label
    # crowds a small panel.
    plots[["fullroc"]] <- plot_fullroc(model_fullroc, show_auc = FALSE, ...) +
      labs(title = "Full ROC", caption = NULL,
           subtitle = paste0("AUC = ", round(model_fullroc$auc, 3),
                             " (ordering: ", model_fullroc$summary$order_method,
                             ")"),
           x = "False alarm rate (innocent suspect)",
           y = "Hit rate (guilty suspect)")
  }

  # The standalone plots are sized to fill a figure on their own. Side by side
  # their titles, legends and captions overrun, so give every panel the same
  # compact theme. theme_bw() replaces each panel's theme, so the legend and
  # title settings come after it.
  plots <- lapply(plots, function(p) {
    p + theme_bw(base_size = 10) +
      theme(legend.position = "bottom",
            plot.title = element_text(size = 11, face = "bold"),
            plot.subtitle = element_text(size = 9))
  })

  # Arrange plots
  if (length(plots) == 0) {
    stop("No plots to create. Check that models were successfully fit.")
  }

  if (length(plots) == 1) {
    return(plots[[1]])
  }

  # Use grid.arrange for layout
  do.call(gridExtra::grid.arrange, c(plots, ncol = ncol))
}


#' Create Comprehensive Model Comparison Table
#'
#' Creates a formatted table comparing model fits for publication or reporting.
#'
#' @param comparison_obj A model_comparison object from compare_models()
#' @param format Character. Output format: "console" (default), "markdown", "latex"
#' @param digits Integer. Number of decimal places (default = 3)
#'
#' @return For \code{format = "console"}, the comparison dataframe. For
#'   \code{"markdown"} and \code{"latex"}, a \code{knitr_kable} object, which
#'   is a character vector with **one element per line**. Print it with
#'   \code{cat(x, sep = "\n")} inside a chunk using \code{results = "asis"};
#'   plain \code{cat(x)} joins the lines with spaces and the table collapses
#'   into a single paragraph.
#'
#' @details
#' Creates a publication-ready table with key statistics for each model:
#' \itemize{
#'   \item 2-HT: Parameters (dP, dA, b, g), AIC, BIC
#'   \item EIG: Information gain, efficiency
#'   \item Full ROC: AUC
#' }
#'
#' @examples
#' data(lineup_example)
#' comparison <- compare_models(lineup_example)
#' format_comparison_table(comparison)
#'
#' @export
format_comparison_table <- function(comparison_obj, format = "console", digits = 3) {

  if (!inherits(comparison_obj, "model_comparison")) {
    stop("comparison_obj must be a model_comparison object")
  }

  # Create wide-format table
  table_list <- list()

  if ("2ht" %in% comparison_obj$models_fit) {
    model_2ht <- comparison_obj$fitted_models[["2ht"]]
    table_list[["2-HT"]] <- data.frame(
      Model = "2-HT",
      dP = round(model_2ht$parameters["dP"], digits),
      dA = round(model_2ht$parameters["dA"], digits),
      b = round(model_2ht$parameters["b"], digits),
      g = round(model_2ht$parameters["g"], digits),
      AIC = round(model_2ht$aic, 1),
      BIC = round(model_2ht$bic, 1),
      stringsAsFactors = FALSE
    )
  }

  if ("eig" %in% comparison_obj$models_fit) {
    model_eig <- comparison_obj$fitted_models[["eig"]]
    table_list[["EIG"]] <- data.frame(
      Model = "EIG",
      EIG_bits = round(model_eig$eig, digits + 1),
      Efficiency_pct = round(model_eig$eig / model_eig$prior_entropy * 100, 1),
      N_categories = nrow(model_eig$response_data),
      stringsAsFactors = FALSE
    )
  }

  if ("fullroc" %in% comparison_obj$models_fit) {
    model_fullroc <- comparison_obj$fitted_models[["fullroc"]]
    table_list[["FullROC"]] <- data.frame(
      Model = "Full ROC",
      AUC = round(model_fullroc$auc, digits + 1),
      Operating_points = model_fullroc$summary$n_operating_points,
      stringsAsFactors = FALSE
    )
  }

  if (format == "console") {
    # Return original comparison table for console printing
    return(comparison_obj$comparison_table)
  } else if (format == "markdown") {
    # row.names = FALSE matters here: comparison_table inherits row names from
    # the named parameter vectors it is assembled from ("dP", "dA", ...), which
    # otherwise appear as a spurious unlabelled first column.
    if (requireNamespace("knitr", quietly = TRUE)) {
      return(knitr::kable(comparison_obj$comparison_table, format = "markdown",
                          row.names = FALSE))
    } else {
      warning("knitr package not available, returning dataframe")
      return(comparison_obj$comparison_table)
    }
  } else if (format == "latex") {
    if (requireNamespace("knitr", quietly = TRUE)) {
      return(knitr::kable(comparison_obj$comparison_table, format = "latex",
                          row.names = FALSE))
    } else {
      warning("knitr package not available, returning dataframe")
      return(comparison_obj$comparison_table)
    }
  }

  return(comparison_obj$comparison_table)
}
