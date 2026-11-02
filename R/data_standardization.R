#' Validate Lineup Identification Data
#'
#' Checks whether a data frame meets the requirements for r4lineups
#' confidence-based analyses. Ensures required columns exist with correct
#' types and valid values.
#'
#' @param data A data frame to validate
#' @param require_confidence Logical. Whether confidence column is required
#'   (default = TRUE). Set to FALSE for analyses that don't need confidence.
#' @param require_response_time Logical. Whether response_time is required
#'   (default = FALSE)
#' @param strict Logical. If TRUE, stops with error on validation failure.
#'   If FALSE, returns validation result with messages (default = FALSE)
#'
#' @return If strict = FALSE, returns a list with:
#'   \itemize{
#'     \item valid: Logical indicating if data passes validation
#'     \item messages: Character vector of validation messages/errors
#'     \item warnings: Character vector of warnings (non-fatal issues)
#'   }
#'   If strict = TRUE, either returns TRUE invisibly or stops with error.
#'
#' @details
#' **Required columns:**
#' \itemize{
#'   \item `target_present`: Logical (TRUE/FALSE) indicating if target in lineup
#'   \item `identification`: Character or factor with values "suspect", "filler",
#'     or "reject"
#'   \item `confidence`: Numeric (if require_confidence = TRUE). Higher = more
#'     confident
#' }
#'
#' **Optional columns:**
#' \itemize{
#'   \item `participant_id`: Unique identifier for each trial/participant
#'   \item `response_time`: Numeric response time in milliseconds
#' }
#'
#' **Validation checks:**
#' \itemize{
#'   \item All required columns present
#'   \item Correct data types
#'   \item No missing values in required columns
#'   \item Valid identification values
#'   \item Positive confidence values
#'   \item At least some target-present and target-absent trials
#' }
#'
#' @examples
#' # Valid data
#' valid_data <- data.frame(
#'   target_present = c(TRUE, TRUE, FALSE, FALSE),
#'   identification = c("suspect", "filler", "reject", "suspect"),
#'   confidence = c(5, 3, 2, 4)
#' )
#' validate_lineup_data(valid_data)
#'
#' # Invalid data (missing column)
#' invalid_data <- data.frame(
#'   target_present = c(TRUE, FALSE),
#'   identification = c("suspect", "filler")
#' )
#' result <- validate_lineup_data(invalid_data, strict = FALSE)
#' print(result$messages)
#'
#' # Strict validation (errors on failure)
#' validate_lineup_data(valid_data, strict = TRUE)
#'
#' @export
validate_lineup_data <- function(data,
                                   require_confidence = TRUE,
                                   require_response_time = FALSE,
                                   strict = FALSE) {

  messages <- character()
  warnings <- character()
  valid <- TRUE

  # Check if data is a data frame
  if (!is.data.frame(data)) {
    messages <- c(messages, "Data must be a data frame")
    valid <- FALSE
    if (strict) stop("Data must be a data frame")
  }

  # Check for required columns
  required_cols <- c("target_present", "identification")
  if (require_confidence) {
    required_cols <- c(required_cols, "confidence")
  }
  if (require_response_time) {
    required_cols <- c(required_cols, "response_time")
  }

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    msg <- paste0("Missing required columns: ", paste(missing_cols, collapse = ", "))
    messages <- c(messages, msg)
    valid <- FALSE
    if (strict) stop(msg)
  }

  # If basic structure is invalid, return early
  if (!valid && !strict) {
    return(list(
      valid = FALSE,
      messages = messages,
      warnings = warnings
    ))
  }

  # Check target_present column
  if ("target_present" %in% names(data)) {
    # Check if it's logical or can be converted
    if (is.logical(data$target_present)) {
      tp_vals <- data$target_present
    } else if (all(data$target_present %in% c(0, 1), na.rm = TRUE)) {
      tp_vals <- as.logical(data$target_present)
    } else {
      msg <- "target_present must be logical (TRUE/FALSE) or 0/1"
      messages <- c(messages, msg)
      valid <- FALSE
      if (strict) stop(msg)
      tp_vals <- NULL
    }

    if (!is.null(tp_vals)) {
      if (any(is.na(tp_vals))) {
        msg <- "target_present contains missing values"
        messages <- c(messages, msg)
        valid <- FALSE
        if (strict) stop(msg)
      }

      # Check for both TP and TA trials
      if (valid || !strict) {
        if (!any(tp_vals, na.rm = TRUE)) {
          warnings <- c(warnings, "No target-present trials found")
        }
        if (!any(!tp_vals, na.rm = TRUE)) {
          warnings <- c(warnings, "No target-absent trials found")
        }
      }
    }
  }

  # Check identification column
  if ("identification" %in% names(data)) {
    valid_ids <- c("suspect", "filler", "reject")

    # Convert to character if factor
    ids <- as.character(data$identification)

    if (any(is.na(ids))) {
      msg <- "identification contains missing values"
      messages <- c(messages, msg)
      valid <- FALSE
      if (strict) stop(msg)
    }

    invalid_ids <- setdiff(unique(ids[!is.na(ids)]), valid_ids)
    if (length(invalid_ids) > 0) {
      msg <- paste0("identification contains invalid values: ",
                    paste(invalid_ids, collapse = ", "),
                    ". Must be 'suspect', 'filler', or 'reject'")
      messages <- c(messages, msg)
      valid <- FALSE
      if (strict) stop(msg)
    }
  }

  # Check confidence column
  if (require_confidence && "confidence" %in% names(data)) {
    if (!is.numeric(data$confidence)) {
      msg <- "confidence must be numeric"
      messages <- c(messages, msg)
      valid <- FALSE
      if (strict) stop(msg)
    } else {
      # Only check further if confidence is numeric
      if (any(!is.finite(data$confidence))) {
        msg <- "confidence contains missing or non-finite values"
        messages <- c(messages, msg)
        valid <- FALSE
        if (strict) stop(msg)
      }

      if (valid || !strict) {
        if (any(data$confidence < 0, na.rm = TRUE)) {
          msg <- "confidence contains negative values"
          messages <- c(messages, msg)
          valid <- FALSE
          if (strict) stop(msg)
        }

        # Check if confidence has reasonable range
        conf_range <- range(data$confidence, na.rm = TRUE)
        if (conf_range[2] - conf_range[1] < 1) {
          warnings <- c(warnings, "confidence has very limited range (might indicate data issue)")
        }
      }
    }
  }

  # Check response_time column
  if (require_response_time && "response_time" %in% names(data)) {
    if (!is.numeric(data$response_time)) {
      msg <- "response_time must be numeric"
      messages <- c(messages, msg)
      valid <- FALSE
      if (strict) stop(msg)
    }

    if (is.numeric(data$response_time) && any(!is.finite(data$response_time))) {
      msg <- "response_time contains missing or non-finite values"
      messages <- c(messages, msg)
      valid <- FALSE
      if (strict) stop(msg)
    }

    if (is.numeric(data$response_time) && any(data$response_time <= 0, na.rm = TRUE)) {
      msg <- "response_time contains non-positive values"
      messages <- c(messages, msg)
      valid <- FALSE
      if (strict) stop(msg)
    }
  }

  # Check for minimum sample size
  if (valid || !strict) {
    if (nrow(data) < 10) {
      warnings <- c(warnings, paste0("Very small sample size (n=", nrow(data),
                                     "). Results may be unreliable."))
    }
  }

  # Return result
  if (strict) {
    if (length(warnings) > 0) {
      for (w in warnings) {
        warning(w)
      }
    }
    return(invisible(TRUE))
  } else {
    return(list(
      valid = valid,
      messages = if (length(messages) > 0) messages else "Data is valid",
      warnings = warnings
    ))
  }
}


#' Standardize Lineup Identification Data
#'
#' Converts lineup data from various formats into the standardized r4lineups
#' format. Handles common data issues and ensures compatibility with all
#' confidence-based analyses.
#'
#' @param data A data frame with lineup identification data
#' @param target_present_col Name of column indicating target presence
#'   (default = "target_present"). Can also be "tp", "culprit_present", etc.
#' @param identification_col Name of column with identification responses
#'   (default = "identification"). Can also be "response", "choice", etc.
#' @param confidence_col Name of column with confidence ratings
#'   (default = "confidence"). Can also be "conf", "certainty", etc.
#' @param response_time_col Name of column with response times (optional)
#' @param participant_id_col Name of column with participant IDs (optional)
#' @param recode_identification Named character vector for recoding
#'   identification values. E.g., c("target" = "suspect", "foil" = "filler")
#' @param validate Logical. Whether to validate the result (default = TRUE)
#'
#' @return A standardized data frame with class "lineup_data" containing:
#'   \itemize{
#'     \item target_present: Logical
#'     \item identification: Character ("suspect", "filler", "reject")
#'     \item confidence: Numeric (if present)
#'     \item participant_id: Original or generated (if present)
#'     \item response_time: Numeric (if present)
#'   }
#'   followed by every other column of \code{data} unchanged, including
#'   \code{suspect_pos} and \code{lineup_member}, which some analyses use.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Renames columns to standard names
#'   \item Converts data types as needed
#'   \item Recodes identification values to standard terms
#'   \item Adds participant IDs if missing
#'   \item Validates the result (if validate = TRUE)
#'   \item Adds S3 class "lineup_data"
#' }
#'
#' **Common recoding patterns:**
#' \itemize{
#'   \item "target" → "suspect"
#'   \item "foil" / "distractor" → "filler"
#'   \item "none" / "not present" / "no choice" → "reject"
#' }
#'
#' @examples
#' # Data with non-standard column names
#' raw_data <- data.frame(
#'   tp = c(1, 1, 0, 0),
#'   response = c("target", "foil", "none", "target"),
#'   conf = c(5, 3, 2, 4)
#' )
#'
#' # Standardize
#' std_data <- standardize_lineup_data(
#'   raw_data,
#'   target_present_col = "tp",
#'   identification_col = "response",
#'   confidence_col = "conf",
#'   recode_identification = c(
#'     "target" = "suspect",
#'     "foil" = "filler",
#'     "none" = "reject"
#'   )
#' )
#' print(std_data)
#'
#' @export
standardize_lineup_data <- function(data,
                                     target_present_col = "target_present",
                                     identification_col = "identification",
                                     confidence_col = "confidence",
                                     response_time_col = NULL,
                                     participant_id_col = NULL,
                                     recode_identification = NULL,
                                     validate = TRUE) {

  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  # Start with empty list to build result
  result_list <- list()

  # --- Process target_present ---
  if (!target_present_col %in% names(data)) {
    # Try common alternatives
    alternatives <- c("tp", "culprit_present", "target", "guilty")
    found <- alternatives[alternatives %in% names(data)]
    if (length(found) > 0) {
      target_present_col <- found[1]
      message("Using '", target_present_col, "' as target_present column")
    } else {
      stop("Cannot find target_present column. Tried: ", target_present_col,
           ", ", paste(alternatives, collapse = ", "))
    }
  }

  result_list$target_present <- as.logical(data[[target_present_col]])

  # --- Process identification ---
  if (!identification_col %in% names(data)) {
    # Try common alternatives
    alternatives <- c("response", "choice", "decision", "pick")
    found <- alternatives[alternatives %in% names(data)]
    if (length(found) > 0) {
      identification_col <- found[1]
      message("Using '", identification_col, "' as identification column")
    } else {
      stop("Cannot find identification column. Tried: ", identification_col,
           ", ", paste(alternatives, collapse = ", "))
    }
  }

  result_list$identification <- as.character(data[[identification_col]])
  used <- c(target_present_col, identification_col)

  # Apply recoding if provided
  if (!is.null(recode_identification)) {
    for (old_val in names(recode_identification)) {
      new_val <- recode_identification[old_val]
      result_list$identification[result_list$identification == old_val] <- new_val
    }
  }

  # Apply default recoding for common variations
  default_recode <- c(
    "target" = "suspect",
    "culprit" = "suspect",
    "perpetrator" = "suspect",
    "foil" = "filler",
    "distractor" = "filler",
    "lure" = "filler",
    "none" = "reject",
    "not present" = "reject",
    "no choice" = "reject",
    "no_choice" = "reject",
    "not_present" = "reject"
  )

  for (old_val in names(default_recode)) {
    new_val <- default_recode[old_val]
    result_list$identification[tolower(result_list$identification) == tolower(old_val)] <- new_val
  }

  # --- Process confidence (optional) ---
  if (confidence_col %in% names(data)) {
    result_list$confidence <- as.numeric(data[[confidence_col]])
    used <- c(used, confidence_col)
  } else {
    # Try common alternatives
    alternatives <- c("conf", "certainty", "cert", "rating")
    found <- alternatives[alternatives %in% names(data)]
    if (length(found) > 0) {
      result_list$confidence <- as.numeric(data[[found[1]]])
      used <- c(used, found[1])
      message("Using '", found[1], "' as confidence column")
    }
  }

  # --- Process participant_id (optional) ---
  if (!is.null(participant_id_col) && participant_id_col %in% names(data)) {
    result_list$participant_id <- data[[participant_id_col]]
    used <- c(used, participant_id_col)
  } else {
    # Try common alternatives
    alternatives <- c("participant_id", "participant", "id", "subject_id", "subj")
    found <- alternatives[alternatives %in% names(data)]
    if (length(found) > 0) {
      result_list$participant_id <- data[[found[1]]]
      used <- c(used, found[1])
    } else {
      # Generate sequential IDs
      result_list$participant_id <- seq_len(nrow(data))
    }
  }

  # --- Process response_time (optional) ---
  if (!is.null(response_time_col) && response_time_col %in% names(data)) {
    result_list$response_time <- as.numeric(data[[response_time_col]])
    used <- c(used, response_time_col)
  } else {
    # Try common alternatives
    alternatives <- c("response_time", "rt", "time", "latency")
    found <- alternatives[alternatives %in% names(data)]
    if (length(found) > 0) {
      result_list$response_time <- as.numeric(data[[found[1]]])
      used <- c(used, found[1])
    }
  }

  # Convert list to data frame
  result <- as.data.frame(result_list, stringsAsFactors = FALSE)

  # Carry every other column through unchanged: suspect_pos and lineup_member
  # decide the innocent-suspect design and the effective-size corrections.
  extra <- setdiff(names(data), c(used, names(result)))
  for (col in extra) {
    result[[col]] <- data[[col]]
  }

  # Add class
  class(result) <- c("lineup_data", "data.frame")

  # Validate if requested
  if (validate) {
    validation <- validate_lineup_data(
      result,
      require_confidence = "confidence" %in% names(result),
      require_response_time = FALSE,
      strict = FALSE
    )

    if (!validation$valid) {
      warning("Standardized data failed validation:\n",
              paste(validation$messages, collapse = "\n"))
    }

    if (length(validation$warnings) > 0) {
      for (w in validation$warnings) {
        warning(w)
      }
    }
  }

  return(result)
}


#' Print Method for Lineup Data
#'
#' @param x A lineup_data object
#' @param ... Additional arguments (ignored)
#' @export
print.lineup_data <- function(x, ...) {
  cat("\n=== Standardized Lineup Data ===\n\n")
  cat("Format: r4lineups standard format\n")
  cat("Rows:", nrow(x), "\n")
  cat("Columns:", paste(names(x), collapse = ", "), "\n\n")

  # Summary stats
  cat("Target-present trials:", sum(x$target_present), "\n")
  cat("Target-absent trials:", sum(!x$target_present), "\n\n")

  cat("Identifications:\n")
  id_table <- table(x$identification)
  for (id_type in names(id_table)) {
    cat(sprintf("  %-10s: %d\n", id_type, id_table[id_type]))
  }

  if ("confidence" %in% names(x)) {
    cat("\nConfidence range:", min(x$confidence), "to", max(x$confidence), "\n")
  }

  cat("\nFirst 10 rows:\n")
  print(head(as.data.frame(x), 10))

  invisible(x)
}


#' Create Example Lineup Data
#'
#' Generate example lineup identification data in standardized format.
#' Useful for testing, demonstrations, and learning r4lineups functions.
#'
#' @param n_trials Integer. Number of trials to generate (default = 100)
#' @param prop_target_present Numeric. Proportion of target-present lineups
#'   (default = 0.5)
#' @param include_confidence Logical. Include confidence ratings (default = TRUE)
#' @param include_response_time Logical. Include response times (default = FALSE)
#' @param seed Integer. Random seed for reproducibility (default = NULL)
#'
#' @return A lineup_data object with realistic example data
#'
#' @examples
#' # Basic example data
#' example_data <- create_example_lineup_data(n_trials = 200, seed = 123)
#' print(example_data)
#'
#' # Use in analysis
#' roc <- make_roc(example_data, lineup_size = 6)
#'
#' @export
create_example_lineup_data <- function(n_trials = 100,
                                        prop_target_present = 0.5,
                                        include_confidence = TRUE,
                                        include_response_time = FALSE,
                                        seed = NULL) {

  restore_rng <- .local_seed(seed)
  on.exit(restore_rng(), add = TRUE)

  # Generate target presence
  target_present <- sample(
    c(TRUE, FALSE),
    size = n_trials,
    replace = TRUE,
    prob = c(prop_target_present, 1 - prop_target_present)
  )

  # Generate identifications (simplified model)
  identification <- character(n_trials)
  for (i in 1:n_trials) {
    if (target_present[i]) {
      # Target-present: higher chance of suspect ID
      identification[i] <- sample(
        c("suspect", "filler", "reject"),
        size = 1,
        prob = c(0.6, 0.3, 0.1)
      )
    } else {
      # Target-absent: lower chance of suspect ID
      identification[i] <- sample(
        c("suspect", "filler", "reject"),
        size = 1,
        prob = c(0.1, 0.4, 0.5)
      )
    }
  }

  # Create data frame
  result <- data.frame(
    participant_id = seq_len(n_trials),
    target_present = target_present,
    identification = identification,
    stringsAsFactors = FALSE
  )

  # Add confidence if requested
  if (include_confidence) {
    # Higher confidence for suspect IDs (simplified)
    result$confidence <- ifelse(
      result$identification == "suspect",
      sample(3:5, n_trials, replace = TRUE, prob = c(0.2, 0.3, 0.5)),
      sample(1:5, n_trials, replace = TRUE)
    )
  }

  # Add response time if requested
  if (include_response_time) {
    # Faster for higher confidence (simplified)
    if (include_confidence) {
      base_rt <- 10000 - (result$confidence - 1) * 1500
    } else {
      base_rt <- rep(7000, n_trials)
    }
    result$response_time <- pmax(1000, base_rt + rnorm(n_trials, 0, 2000))
  }

  class(result) <- c("lineup_data", "data.frame")
  return(result)
}
