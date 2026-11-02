#' @keywords internal
#' @importFrom stats density
#' @importFrom utils globalVariables
utils::globalVariables(c(
  # ggplot2 aesthetics used in plotting functions
  "Dim1",
  "Dim2",
  "accuracy",
  "anri",
  "base_rate",
  "ci_lower",
  "ci_upper",
  "condition",
  "conf_numeric",
  "confidence",
  "correct_id_rate",
  "correction",
  "criterion",
  "cumulative_false_alarm_rate",
  "cumulative_hit_rate",
  "delta",
  "difference",
  "distance",
  "dpp",
  "effective_size",
  "error_rate",
  "evidence_direction",
  "evidence_label",
  "expected",
  "expected_utility",
  "false_id_rate",
  "fsmax",
  "foil_name",
  "group",
  "highlighted",
  "identification",
  "information_gain",
  "label",
  "model",
  "mean_confidence_plot",
  "n",
  "n_total",
  "observed",
  "posterior",
  "posterior_guilty",
  "pearson_chisq",
  "ppv",
  "ppv_nominal",
  "ppv_none",
  "prior",
  "prior_a",
  "procedure",
  "response",
  "response_cat",
  "response_label",
  "se",
  "source",
  "similarity",
  "ss",
  "stats_label",
  "cell",
  "count",
  "converged",
  "false_alarm_rate",
  "hit_rate",
  "target_present",
  "z",
  # multid plots
  "component",
  "end",
  "estimate",
  "label_y",
  "kind",
  "pos",
  "start",
  "threshold",
  "value"
))

# Determine how innocent-suspect identifications are represented in a
# target-absent dataset. With a designated innocent suspect, the explicit
# suspect rows are the innocent-suspect responses and filler rows must not be
# added to them. Without one, filler choices are divided by nominal lineup size.
#
# `innocent_suspect = "auto"` treats the lineups as having a designated suspect
# when any target-absent row is a suspect ID or the data carry a non-missing
# `suspect_pos` column for target-absent rows. A designated suspect who is never
# chosen is still designated, so zero suspect IDs alone is not evidence that
# there was none. Resolved values ("designated", "estimated_from_fillers") pass
# through unchanged, so a bootstrap can resolve once on the full data and reuse
# the result in every replicate.
.innocent_suspect_method <- function(ta_data, innocent_suspect = "auto") {
  if (!is.character(innocent_suspect) || length(innocent_suspect) != 1L ||
      !innocent_suspect %in% c("auto", "designated", "estimated",
                               "estimated_from_fillers")) {
    stop("innocent_suspect must be one of \"auto\", \"designated\" or ",
         "\"estimated\".", call. = FALSE)
  }
  if (innocent_suspect == "designated") return("designated")
  if (innocent_suspect %in% c("estimated", "estimated_from_fillers")) {
    if (any(ta_data$identification == "suspect", na.rm = TRUE)) {
      stop("Estimated innocent-suspect IDs require target-absent choices coded as filler IDs, not suspect IDs. Recode the undesignated data explicitly before requesting estimated mode.", call. = FALSE)
    }
    return("estimated_from_fillers")
  }
  has_suspect_id <- any(ta_data$identification == "suspect", na.rm = TRUE)
  has_suspect_pos <- "suspect_pos" %in% names(ta_data) &&
    any(!is.na(ta_data$suspect_pos))
  if (has_suspect_id || has_suspect_pos) {
    return("designated")
  }
  message("No target-absent suspect IDs and no suspect_pos column; treating ",
          "the lineups as having no designated innocent suspect and estimating ",
          "innocent-suspect IDs as filler IDs / lineup_size. Set ",
          "innocent_suspect = \"designated\" if a suspect was designated but ",
          "never chosen.")
  "estimated_from_fillers"
}

# Resolve the innocent-suspect design once on a full dataset, so that resampled
# or subsetted analyses all use the same design.
.resolve_innocent_suspect <- function(data, innocent_suspect = "auto") {
  .innocent_suspect_method(
    data[!as.logical(data$target_present), , drop = FALSE], innocent_suspect
  )
}

# Probability of guilt given a suspect ID. With `base_rate = NULL` this is the
# count ratio that Mickes (2015) defines for CAC accuracy, which equals the
# posterior at the design's own TP:TA ratio. With a base rate it is computed
# from the two arms' rates, so it no longer depends on how many trials each arm
# had.
.suspect_id_ppv <- function(n_guilty, n_innocent, n_tp, n_ta, base_rate = NULL) {
  if (is.null(base_rate)) {
    n <- n_guilty + n_innocent
    return(if (n > 0) n_guilty / n else NA_real_)
  }
  g <- if (n_tp > 0) base_rate * n_guilty / n_tp else 0
  i <- if (n_ta > 0) (1 - base_rate) * n_innocent / n_ta else 0
  if (g + i > 0) g / (g + i) else NA_real_
}

.check_base_rate <- function(base_rate) {
  if (!is.null(base_rate) &&
      (!is.numeric(base_rate) || length(base_rate) != 1L || is.na(base_rate) ||
       base_rate <= 0 || base_rate >= 1)) {
    stop("base_rate must be NULL or a single number strictly between 0 and 1.",
         call. = FALSE)
  }
  invisible(base_rate)
}

# Count-based accuracy mixes witness performance with the design's TP:TA split.
# Say so when the arms differ, and point to the rate-based alternative.
.unbalanced_note <- function(n_tp, n_ta) {
  if (n_tp != n_ta) {
    message("Target-present and target-absent samples differ in size (",
            n_tp, " and ", n_ta, "). Accuracy computed from counts, as in ",
            "Mickes (2015), then reflects that split as well as witness ",
            "performance. Set base_rate = 0.5 to compute it from the hit and ",
            "false-ID rates instead.")
  }
  invisible(NULL)
}

# Confidence criteria for a suspect-ID ROC, highest first. Only responses that
# can move an operating point define a criterion: target-present suspect IDs,
# plus target-absent suspect IDs (designated) or filler IDs (estimated). The
# confidence of rejections and target-present filler IDs would only add
# duplicate points, which reweight averages over points such as DPP. With no
# such responses the single highest confidence gives one (0, 0) point.
.roc_thresholds <- function(tp_data, ta_data, method, thresholds = NULL) {
  if (!is.null(thresholds)) {
    if (!is.numeric(thresholds) || length(thresholds) == 0L ||
        anyNA(thresholds)) {
      stop("thresholds must be a non-empty numeric vector.", call. = FALSE)
    }
    return(sort(unique(thresholds), decreasing = TRUE))
  }
  ta_type <- if (method == "designated") "suspect" else "filler"
  conf <- c(tp_data$confidence[tp_data$identification == "suspect"],
            ta_data$confidence[ta_data$identification == ta_type])
  conf <- conf[!is.na(conf)]
  if (length(conf) == 0L) {
    conf <- max(c(tp_data$confidence, ta_data$confidence), na.rm = TRUE)
  }
  sort(unique(conf), decreasing = TRUE)
}

# A confidence rule that really accepts no observed relevant identification,
# regardless of a caller's restricted criterion grid. Inf also handles rounding
# at very large finite confidence values.
.reject_all_criterion <- function(tp_data, ta_data, method, thresholds = NULL) {
  ta_type <- if (method == "designated") "suspect" else "filler"
  conf <- c(tp_data$confidence[tp_data$identification == "suspect"],
            ta_data$confidence[ta_data$identification == ta_type], thresholds)
  if (!length(conf)) conf <- c(tp_data$confidence, ta_data$confidence)
  top <- max(conf)
  cutoff <- top + 1
  if (!is.finite(cutoff) || cutoff <= top) Inf else cutoff
}

.validate_bins <- function(values, breaks, label = "confidence_bins") {
  if (is.null(breaks)) return(invisible(TRUE))
  if (!is.numeric(breaks) || length(breaks) < 2L || anyNA(breaks) ||
      is.unsorted(breaks, strictly = TRUE)) {
    stop(label, " must contain strictly increasing numeric boundaries.", call. = FALSE)
  }
  if (anyNA(values) || any(!is.finite(values)) ||
      any(values < breaks[1L] | values > utils::tail(breaks, 1L))) {
    stop(label, " must cover every observation; subset the data explicitly before analysis if observations are to be excluded.", call. = FALSE)
  }
  invisible(TRUE)
}

# sample(x, ...) treats a single positive integer as 1:x. Bootstrap draws must
# always sample observed elements, including when a stratum has one member.
.resample_values <- function(x) {
  x[sample.int(length(x), length(x), replace = TRUE)]
}

.innocent_suspect_count <- function(ta_data, include, lineup_size,
                                    method = .innocent_suspect_method(ta_data)) {
  if (!is.numeric(lineup_size) || length(lineup_size) != 1L ||
      is.na(lineup_size) || lineup_size < 1) {
    stop("lineup_size must be a positive number.", call. = FALSE)
  }
  include <- as.logical(include)
  if (length(include) != nrow(ta_data) || anyNA(include)) {
    stop("Internal inclusion mask does not match target-absent data.", call. = FALSE)
  }
  if (method == "designated") {
    sum(include & ta_data$identification == "suspect", na.rm = TRUE)
  } else {
    sum(include & ta_data$identification == "filler", na.rm = TRUE) / lineup_size
  }
}

# Shared validation for analytical entry points. This intentionally avoids the
# sample-size warnings produced by strict validation.
.validate_lineup_analysis <- function(data, require_confidence = TRUE,
                                      require_response_time = FALSE,
                                      lineup_size = NULL) {
  if (!is.null(lineup_size) && (!is.numeric(lineup_size) ||
      length(lineup_size) != 1L || !is.finite(lineup_size) ||
      lineup_size < 1 || lineup_size != floor(lineup_size))) {
    stop("lineup_size must be a positive integer.", call. = FALSE)
  }
  checked <- validate_lineup_data(
    data,
    require_confidence = require_confidence,
    require_response_time = require_response_time,
    strict = FALSE
  )
  if (!isTRUE(checked$valid)) {
    stop(paste(checked$messages, collapse = "; "), call. = FALSE)
  }
  tp <- as.logical(data$target_present)
  if (!any(tp) || !any(!tp)) {
    stop("Data must include both target-present and target-absent trials.",
         call. = FALSE)
  }
  invisible(TRUE)
}

.adjust_innocent_response_counts <- function(counts, lineup_size, method) {
  counts <- as.numeric(counts) |> stats::setNames(names(counts))
  if (method == "designated") return(counts)
  adjusted <- numeric()
  add_count <- function(name, value) {
    current <- adjusted[name]
    if (length(current) == 0L || is.na(current)) current <- 0
    adjusted[name] <<- current + value
  }
  for (response in names(counts)) {
    count <- unname(counts[[response]])
    if (response == "filler" || startsWith(response, "filler_")) {
      suspect_response <- sub("^filler", "suspect", response)
      add_count(suspect_response, count / lineup_size)
      add_count(response, count * (lineup_size - 1) / lineup_size)
    } else {
      add_count(response, count)
    }
  }
  adjusted
}

.local_seed <- function(seed) {
  if (is.null(seed)) return(function() invisible(NULL))
  seed_existed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (seed_existed) get(".Random.seed", envir = .GlobalEnv) else NULL
  set.seed(seed)
  function() {
    if (seed_existed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
    invisible(NULL)
  }
}
