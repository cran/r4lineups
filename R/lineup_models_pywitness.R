.pywitness_audited_revision <- "e726dcfc09423d0e0ff7f46c8e3a711040293eba"
.pywitness_repository <- "https://github.com/lmickes/pyWitness.git"
.pywitness_archive <- paste0(
  "https://github.com/lmickes/pyWitness/archive/",
  .pywitness_audited_revision,
  ".zip"
)
.pywitness_default_env <- "r4lineups-pywitness"

.pywitness_revision_matches <- function(version) {
  is.character(version) && length(version) == 1L && !is.na(version) &&
    grepl(substr(.pywitness_audited_revision, 1L, 9L), version, fixed = TRUE)
}

#' Install the Audited pyWitness Engine
#'
#' Explicitly creates or reuses an isolated Python virtual environment and
#' installs the audited pyWitness Git revision used by
#' \code{\link{fit_lineup_models}}. This function is never called when the
#' package is loaded or checked.
#'
#' @param envname Name or path of the virtual environment.
#' @param python Optional path to the Python executable used to create a new
#'   virtual environment.
#' @param pip_options Optional character vector passed to pip.
#' @param force Logical. Reinstall the pinned revision if already present.
#' @return Invisibly returns the environment name.
#'
#' @details
#' The installation is pinned to Git revision
#' \code{e726dcfc09423d0e0ff7f46c8e3a711040293eba}. pyWitness is GPL-3
#' software maintained separately from r4lineups; no pyWitness source or
#' documentation is bundled in this package. The immutable commit archive is
#' used so installation does not require a local Git executable.
#'
#' @examples
#' \dontrun{
#' install_pywitness()
#' check_pywitness_deps(initialize = TRUE)
#' }
#'
#' @references
#' Mickes, L., Seale-Carlisle, T. M., Chen, X., & Boogert, S. (2024).
#' pyWitness 1.0: A Python eyewitness identification analysis toolkit.
#' \emph{Behavior Research Methods, 56}, 1533--1550.
#' @export
install_pywitness <- function(envname = .pywitness_default_env,
                              python = NULL,
                              pip_options = NULL,
                              force = FALSE) {
  if (!is.character(envname) || length(envname) != 1L || is.na(envname) ||
      !nzchar(envname)) {
    stop("envname must be one non-empty character value.", call. = FALSE)
  }
  if (!is.null(python) &&
      (!is.character(python) || length(python) != 1L || is.na(python) ||
       !nzchar(python))) {
    stop("python must be NULL or one non-empty path.", call. = FALSE)
  }
  if (!is.logical(force) || length(force) != 1L || is.na(force)) {
    stop("force must be TRUE or FALSE.", call. = FALSE)
  }

  if (!reticulate::virtualenv_exists(envname)) {
    args <- list(envname = envname)
    if (!is.null(python)) args$python <- python
    do.call(reticulate::virtualenv_create, args)
  }
  previous_scm_version <- Sys.getenv(
    "SETUPTOOLS_SCM_PRETEND_VERSION_FOR_PYWITNESS", unset = NA_character_
  )
  Sys.setenv(
    SETUPTOOLS_SCM_PRETEND_VERSION_FOR_PYWITNESS = paste0(
      "0.1.dev0+g", substr(.pywitness_audited_revision, 1L, 9L)
    )
  )
  on.exit({
    if (is.na(previous_scm_version)) {
      Sys.unsetenv("SETUPTOOLS_SCM_PRETEND_VERSION_FOR_PYWITNESS")
    } else {
      Sys.setenv(
        SETUPTOOLS_SCM_PRETEND_VERSION_FOR_PYWITNESS = previous_scm_version
      )
    }
  }, add = TRUE)
  reticulate::py_install(
    packages = .pywitness_archive,
    envname = envname,
    method = "virtualenv",
    pip = TRUE,
    pip_options = pip_options,
    ignore_installed = isTRUE(force)
  )
  message("Installed the audited pyWitness revision in '", envname, "'.")
  invisible(envname)
}

#' Check Availability of the pyWitness Engine
#'
#' Reports whether Python and pyWitness are available without initializing
#' Python by default.
#'
#' @param envname Name or path of the intended virtual environment.
#' @param initialize Logical. If \code{TRUE}, select \code{envname}, initialize
#'   Python, and import pyWitness. The default performs no initialization.
#' @param verbose Logical. Print a concise status report.
#' @return A one-row tibble with environment and engine status, including
#'   whether the imported version encodes the audited revision when
#'   \code{initialize = TRUE}.
#'
#' @examples
#' check_pywitness_deps()
#' \dontrun{
#' check_pywitness_deps(initialize = TRUE)
#' }
#' @export
check_pywitness_deps <- function(envname = .pywitness_default_env,
                                 initialize = FALSE,
                                 verbose = TRUE) {
  if (!is.character(envname) || length(envname) != 1L || is.na(envname) ||
      !nzchar(envname)) {
    stop("envname must be one non-empty character value.", call. = FALSE)
  }
  if (!is.logical(initialize) || length(initialize) != 1L ||
      is.na(initialize) || !is.logical(verbose) || length(verbose) != 1L ||
      is.na(verbose)) {
    stop("initialize and verbose must each be TRUE or FALSE.", call. = FALSE)
  }

  env_exists <- reticulate::virtualenv_exists(envname)
  python_configured <- reticulate::py_available(initialize = FALSE)
  installed <- FALSE
  version <- NA_character_
  revision_matches <- NA
  python <- NA_character_

  if (isTRUE(initialize)) {
    if (!env_exists) {
      stop("The pyWitness environment does not exist. Run install_pywitness().",
           call. = FALSE)
    }
    reticulate::use_virtualenv(envname, required = TRUE)
    python_configured <- reticulate::py_available(initialize = TRUE)
    installed <- reticulate::py_module_available("pyWitness")
    if (installed) {
      module <- reticulate::import("pyWitness", convert = TRUE)
      version <- as.character(module$version)
      revision_matches <- .pywitness_revision_matches(version)
      python <- reticulate::py_config()$python
    }
  } else if (python_configured) {
    installed <- reticulate::py_module_available("pyWitness")
    python <- reticulate::py_config()$python
  }

  result <- tibble::tibble(
    environment = envname,
    environment_exists = env_exists,
    python_configured = python_configured,
    installed = installed,
    version = version,
    audited_revision_matches = revision_matches,
    python = python,
    audited_revision = .pywitness_audited_revision
  )
  if (verbose) {
    cat("pyWitness environment:", envname, "\n")
    cat("Environment exists:", env_exists, "\n")
    cat("Python initialized:", python_configured, "\n")
    cat("pyWitness available:", installed, "\n")
    if (!is.na(version)) cat("pyWitness version:", version, "\n")
    if (!is.na(revision_matches)) {
      cat("Audited revision matches:", revision_matches, "\n")
    }
    cat("Audited revision:", .pywitness_audited_revision, "\n")
    if (!initialize && !python_configured) {
      cat("No Python process was initialized. Use initialize = TRUE to import ",
          "the isolated engine.\n", sep = "")
    }
  }
  invisible(result)
}

.validate_lineup_model_control <- function(control) {
  defaults <- list(
    maxiter = 5000L,
    method = "Nelder-Mead",
    integration_sigma = 8,
    chi2_variance = "expected",
    objective = "corrected",
    verbose = FALSE
  )
  if (is.null(control)) return(defaults)
  if (!is.list(control) || is.null(names(control)) ||
      any(!names(control) %in% names(defaults))) {
    stop("control must be a named list containing only maxiter, method, ",
         "integration_sigma, chi2_variance, objective, or verbose.",
         call. = FALSE)
  }
  out <- utils::modifyList(defaults, control)
  if (!is.numeric(out$maxiter) || length(out$maxiter) != 1L ||
      is.na(out$maxiter) || out$maxiter < 1L ||
      out$maxiter != as.integer(out$maxiter)) {
    stop("control$maxiter must be a positive integer.", call. = FALSE)
  }
  if (!is.character(out$method) || length(out$method) != 1L ||
      is.na(out$method) || !nzchar(out$method)) {
    stop("control$method must be one non-empty character value.",
         call. = FALSE)
  }
  if (!is.numeric(out$integration_sigma) ||
      length(out$integration_sigma) != 1L ||
      is.na(out$integration_sigma) || !is.finite(out$integration_sigma) ||
      out$integration_sigma <= 0) {
    stop("control$integration_sigma must be positive and finite.",
         call. = FALSE)
  }
  out$chi2_variance <- match.arg(out$chi2_variance,
                                 c("expected", "observed"))
  out$objective <- match.arg(out$objective, c("corrected", "pywitness"))
  if (!is.logical(out$verbose) || length(out$verbose) != 1L ||
      is.na(out$verbose)) {
    stop("control$verbose must be TRUE or FALSE.", call. = FALSE)
  }
  out$maxiter <- as.integer(out$maxiter)
  out
}

.prepare_lineup_model_data <- function(data, lineup_size, confidence_bins) {
  if (!is.data.frame(data)) stop("data must be a data frame.", call. = FALSE)
  required <- c("target_present", "identification", "confidence")
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop("data is missing required columns: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  grouping_columns <- intersect(c("condition", "procedure"), names(data))
  for (column in grouping_columns) {
    values <- unique(stats::na.omit(data[[column]]))
    if (length(values) > 1L) {
      stop("Fit one ", column, " per call; subset data before fitting.",
           call. = FALSE)
    }
  }
  if (!is.numeric(lineup_size) || length(lineup_size) != 1L ||
      is.na(lineup_size) || !is.finite(lineup_size) || lineup_size < 2L ||
      lineup_size != as.integer(lineup_size)) {
    stop("lineup_size must be an integer of at least 2.", call. = FALSE)
  }
  lineup_size <- as.integer(lineup_size)
  tp <- data$target_present
  if (is.numeric(tp) && all(tp %in% c(0, 1))) tp <- as.logical(tp)
  if (!is.logical(tp) || anyNA(tp) || !any(tp) || !any(!tp)) {
    stop("target_present must be complete logical data containing both ",
         "target-present and target-absent trials.", call. = FALSE)
  }
  identification <- as.character(data$identification)
  if (anyNA(identification) ||
      any(!identification %in% c("suspect", "filler", "reject"))) {
    stop("identification must contain only suspect, filler, or reject.",
         call. = FALSE)
  }
  confidence <- data$confidence
  if (!is.numeric(confidence) || anyNA(confidence) ||
      any(!is.finite(confidence))) {
    stop("confidence must be complete, finite numeric data.", call. = FALSE)
  }

  if (is.null(confidence_bins)) {
    confidence_values <- sort(unique(confidence))
    confidence_bin <- match(confidence, confidence_values)
    bin_specification <- confidence_values
  } else {
    if (!is.numeric(confidence_bins) || length(confidence_bins) < 2L ||
        anyNA(confidence_bins) || any(!is.finite(confidence_bins)) ||
        is.unsorted(confidence_bins, strictly = TRUE)) {
      stop("confidence_bins must be finite, strictly increasing bin edges.",
           call. = FALSE)
    }
    confidence_bin <- cut(
      confidence, breaks = confidence_bins, labels = FALSE,
      include.lowest = TRUE, right = TRUE
    )
    if (anyNA(confidence_bin)) {
      stop("confidence_bins must cover every confidence value.",
           call. = FALSE)
    }
    confidence_values <- seq_len(length(confidence_bins) - 1L)
    if (any(tabulate(confidence_bin, nbins = length(confidence_values)) == 0L)) {
      stop("confidence_bins cannot create an entirely empty confidence bin; ",
           "combine or remove empty bins.", call. = FALSE)
    }
    bin_specification <- confidence_bins
  }

  ta_suspect_collapsed <- sum(!tp & identification == "suspect")
  engine_response <- identification
  engine_response[!tp & engine_response == "suspect"] <- "filler"

  required_cells <- data.frame(
    target_present = c(FALSE, FALSE, TRUE, TRUE, TRUE),
    identification = c("filler", "reject", "filler", "suspect", "reject"),
    stringsAsFactors = FALSE
  )
  present <- vapply(seq_len(nrow(required_cells)), function(i) {
    any(tp == required_cells$target_present[[i]] &
          engine_response == required_cells$identification[[i]])
  }, logical(1))
  if (any(!present)) {
    labels <- paste0(
      ifelse(required_cells$target_present[!present], "TP", "TA"),
      " ", required_cells$identification[!present]
    )
    stop(
      "pyWitness requires each aggregate response category to be present; ",
      "missing: ", paste(labels, collapse = ", "), ".",
      call. = FALSE
    )
  }

  prepared <- data.frame(
    target_present = tp,
    identification = engine_response,
    confidence = as.integer(confidence_bin),
    stringsAsFactors = FALSE
  )
  list(
    data = prepared,
    confidence_levels = as.integer(confidence_values),
    confidence_bins = bin_specification,
    lineup_size = lineup_size,
    ta_suspect_collapsed = ta_suspect_collapsed,
    n_tp = sum(tp),
    n_ta = sum(!tp)
  )
}

.lineup_model_cells <- function(prepared) {
  confidence <- prepared$confidence_levels
  data <- prepared$data
  count <- function(tp, response, level) {
    sum(data$target_present == tp &
          data$identification == response &
          data$confidence == level)
  }
  cells <- do.call(rbind, lapply(confidence, function(level) {
    data.frame(
      target_present = c(FALSE, TRUE, TRUE),
      response = c("filler", "suspect", "filler"),
      confidence = level,
      observed = c(
        count(FALSE, "filler", level),
        count(TRUE, "suspect", level),
        count(TRUE, "filler", level)
      ),
      stringsAsFactors = FALSE
    )
  }))
  rbind(
    cells,
    data.frame(
      target_present = c(FALSE, TRUE),
      response = c("reject", "reject"),
      confidence = NA_integer_,
      observed = c(
        sum(!data$target_present & data$identification == "reject"),
        sum(data$target_present & data$identification == "reject")
      ),
      stringsAsFactors = FALSE
    )
  )
}

.pywitness_data_frame <- function(prepared) {
  data <- prepared$data
  data.frame(
    participantId = seq_len(nrow(data)),
    lineupSize = prepared$lineup_size,
    targetLineup = ifelse(data$target_present,
                          "targetPresent", "targetAbsent"),
    responseType = ifelse(
      data$identification == "suspect", "suspectId",
      ifelse(data$identification == "filler", "fillerId", "rejectId")
    ),
    confidence = data$confidence,
    stringsAsFactors = FALSE
  )
}

.lineup_model_class <- function(model, shared_variance = "estimated") {
  if (identical(model, "independent") && identical(shared_variance, "zero")) {
    return("ModelFitIndependentObservationSimple")
  }
  switch(
    model,
    independent = "ModelFitIndependentObservation",
    ensemble = "ModelFitEnsemble",
    best_rest = "ModelFitBestRest",
    integration = "ModelFitIntegration"
  )
}

.model_starts <- function(starts, model) {
  if (is.null(starts)) return(NULL)
  if (is.numeric(starts)) return(starts)
  if (!is.list(starts) || is.null(names(starts))) {
    stop("starts must be a named numeric vector or a named list by model.",
         call. = FALSE)
  }
  model_starts <- starts[[model]]
  if (is.null(model_starts)) return(NULL)
  model_starts
}

.validate_lineup_model_starts <- function(starts, models) {
  if (is.null(starts)) return(invisible(NULL))
  validate_vector <- function(values) {
    if (!is.numeric(values) || is.null(names(values)) ||
        any(!nzchar(names(values))) || anyDuplicated(names(values)) ||
        anyNA(values) || any(!is.finite(values))) {
      stop("Starting values must be a uniquely named finite numeric vector.",
           call. = FALSE)
    }
    if (any(grepl("Sigma$", names(values)) & values < 0)) {
      stop("Sigma starting values cannot be negative.", call. = FALSE)
    }
    common <- c(
      "lureMean", "lureSigma", "targetMean", "targetSigma",
      "lureBetweenSigma", "targetBetweenSigma"
    )
    unknown <- names(values)[
      !names(values) %in% common & !grepl("^c[0-9]+$", names(values))
    ]
    if (length(unknown)) {
      stop("Unknown pyWitness starting parameters: ",
           paste(unknown, collapse = ", "), call. = FALSE)
    }
    criteria_names <- grep("^c[0-9]+$", names(values), value = TRUE)
    if (length(criteria_names) > 1L) {
      criteria_names <- criteria_names[order(as.integer(
        sub("^c", "", criteria_names)
      ))]
      if (is.unsorted(unname(values[criteria_names]), strictly = TRUE)) {
        stop("Criterion starting values must be strictly increasing.",
             call. = FALSE)
      }
    }
  }
  if (is.numeric(starts)) {
    validate_vector(starts)
  } else {
    if (!is.list(starts) || is.null(names(starts)) ||
        any(!nzchar(names(starts))) || anyDuplicated(names(starts))) {
      stop("starts must be a named numeric vector or a named list by model.",
           call. = FALSE)
    }
    unknown <- setdiff(names(starts), models)
    if (length(unknown)) {
      stop("starts contains entries for models not being fitted: ",
           paste(unknown, collapse = ", "), call. = FALSE)
    }
    lapply(starts, validate_vector)
  }
  invisible(NULL)
}

.apply_pywitness_starts <- function(fit, starts, model) {
  values <- .model_starts(starts, model)
  if (is.null(values)) return(invisible(NULL))
  if (!is.numeric(values) || is.null(names(values)) || any(!nzchar(names(values))) ||
      anyNA(values) || any(!is.finite(values))) {
    stop("Starting values must be a named finite numeric vector.",
         call. = FALSE)
  }
  parameter_names <- as.character(reticulate::py_to_r(fit$parameterNames))
  unknown <- setdiff(names(values), parameter_names)
  if (length(unknown)) {
    stop("Unknown pyWitness starting parameters for ", model, ": ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  if (any(grepl("Sigma$", names(values)) & values < 0)) {
    stop("Sigma starting values cannot be negative.", call. = FALSE)
  }
  for (name in names(values)) fit[[name]]$value <- unname(values[[name]])
  criteria_names <- grep("^c[0-9]+$", parameter_names, value = TRUE)
  criteria <- vapply(criteria_names, function(name) {
    as.numeric(fit[[name]]$value)
  }, numeric(1))
  if (length(criteria) > 1L && is.unsorted(criteria, strictly = TRUE)) {
    stop("Criterion starting values must be strictly increasing.",
         call. = FALSE)
  }
  invisible(NULL)
}

.extract_pywitness_parameters <- function(fit, model) {
  parameter_names <- as.character(reticulate::py_to_r(fit$parameterNames))
  do.call(rbind, lapply(parameter_names, function(name) {
    parameter <- fit[[name]]
    linked <- tryCatch({
      other <- parameter$other
      if (is.null(other)) NA_character_ else as.character(other$name)
    }, error = function(e) NA_character_)
    data.frame(
      model = model,
      parameter = name,
      estimate = as.numeric(parameter$value),
      fixed = isTRUE(as.logical(parameter$fixed)),
      linked_to = linked,
      stringsAsFactors = FALSE
    )
  }))
}

.extract_pywitness_cells <- function(fit, observed, prepared, model) {
  prediction <- reticulate::py_to_r(
    fit$calculateFrequenciesForAllCriteria()
  )
  if (!is.list(prediction) || length(prediction) != 6L) {
    prediction <- as.list(prediction)
  }
  ta_reject <- as.numeric(prediction[[1L]])
  ta_filler <- as.numeric(prediction[[3L]])
  tp_reject <- as.numeric(prediction[[4L]])
  tp_suspect <- as.numeric(prediction[[5L]])
  tp_filler <- as.numeric(prediction[[6L]])
  n_conf <- length(prepared$confidence_levels)
  if (any(lengths(list(ta_filler, tp_suspect, tp_filler)) != n_conf)) {
    stop("pyWitness returned an unexpected number of confidence cells.",
         call. = FALSE)
  }
  expected <- c(rbind(ta_filler, tp_suspect, tp_filler),
                ta_reject, tp_reject)
  out <- observed
  out$model <- model
  out$expected <- as.numeric(expected)
  out[, c("model", "target_present", "response", "confidence",
          "observed", "expected")]
}

.loglik_at_expected <- function(cells) {
  total_loglik <- 0
  for (tp in c(FALSE, TRUE)) {
    group <- cells[cells$target_present == tp, , drop = FALSE]
    observed <- group$observed
    expected <- pmax(group$expected, 0)
    total_expected <- sum(expected)
    if (!is.finite(total_expected) || total_expected <= 0) return(NA_real_)
    probability <- expected / total_expected
    if (any(probability <= 0 & observed > 0)) return(-Inf)
    positive <- observed > 0
    total_loglik <- total_loglik +
      lgamma(sum(observed) + 1) - sum(lgamma(observed + 1)) +
      sum(observed[positive] * log(probability[positive]))
  }
  total_loglik
}

.pywitness_fit_one <- function(pyw, processed, model, prepared, observed,
                               variance, shared_variance, starts, control) {
  if (identical(control$chi2_variance, "observed") && any(observed$observed <= 0)) {
    stop("Observed-denominator chi-squared requires positive observed cells; use expected denominators for sparse cells.", call. = FALSE)
  }
  constructor <- pyw[[.lineup_model_class(model, shared_variance)]]
  fit <- constructor(
    processed,
    integrationSigma = as.numeric(control$integration_sigma),
    chi2Var = control$chi2_variance
  )
  # objective = "pywitness" leaves the engine untouched, to reproduce its own
  # results (e.g. published fits); the default corrects its rejection cells.
  if (identical(control$objective, "corrected")) {
    adapter <- new.env(parent = baseenv())
    reticulate::source_python(system.file("python", "chi_square_adapter.py",
                                         package = "r4lineups"), envir = adapter)
    fit <- adapter$correct_chi_square_objective(fit)
  }
  if (variance == "equal") fit$setEqualVariance() else fit$setUnequalVariance()

  if (model %in% c("ensemble", "best_rest") ||
      shared_variance == "zero") {
    fit$targetBetweenSigma$value <- 0
    fit$targetBetweenSigma$fixed <- TRUE
  } else {
    fit$targetBetweenSigma$fixed <- FALSE
  }
  .apply_pywitness_starts(fit, starts, model)

  if (control$verbose) {
    fit$fit(maxiter = control$maxiter, method = control$method)
  } else {
    reticulate::py_capture_output(
      fit$fit(maxiter = control$maxiter, method = control$method),
      type = "stdout"
    )
  }

  status <- as.character(fit$fitStatus)
  converged <- grepl("success|converg", status, ignore.case = TRUE)
  cells <- .extract_pywitness_cells(fit, observed, prepared, model)
  parameters <- .extract_pywitness_parameters(fit, model)
  pearson <- as.numeric(fit$chi2)
  df <- as.integer(fit$numberDegreesOfFreedom)
  p_value <- if (df > 0 && is.finite(pearson)) {
    stats::pchisq(pearson, df = df, lower.tail = FALSE)
  } else {
    NA_real_
  }
  comparison <- data.frame(
    model = model,
    converged = converged,
    status = status,
    iterations = as.integer(fit$numberIterations),
    n_parameters = as.integer(fit$numberFreeParameters),
    pearson_chisq = pearson,
    df = df,
    p_value = p_value,
    loglik_at_estimate = .loglik_at_expected(cells),
    AIC = NA_real_,
    BIC = NA_real_,
    error = NA_character_,
    stringsAsFactors = FALSE
  )
  list(comparison = comparison, parameters = parameters, cells = cells)
}

#' Fit Competing Wixted Lineup-Memory Models
#'
#' Fits confidence-binned Independent Observations, Ensemble/BEST-Rest, and
#' Integration models through an explicitly installed pyWitness engine.
#'
#' @param data A data frame with \code{target_present}, \code{identification},
#'   and numeric \code{confidence} columns.
#' @param models Character vector containing \code{"independent"},
#'   \code{"ensemble"}, \code{"integration"}, and optionally
#'   \code{"best_rest"}. \code{"max"} aliases \code{"independent"}.
#' @param lineup_size Integer lineup size.
#' @param confidence_bins Optional strictly increasing numeric bin edges. When
#'   \code{NULL}, each observed confidence value is an ordered bin.
#' @param variance Either \code{"equal"} or \code{"unequal"}.
#' @param shared_variance Either \code{"estimated"} or \code{"zero"}. Shared
#'   variance cancels from Ensemble and BEST-Rest and is fixed at zero there.
#' @param starts Optional named numeric starting vector applied to all models,
#'   or a named list of such vectors indexed by model.
#' @param control Named list controlling \code{maxiter}, optimizer
#'   \code{method}, numerical \code{integration_sigma},
#'   \code{chi2_variance}, \code{objective}, and \code{verbose}.
#'   \code{objective = "corrected"} (the default) applies the per-fit
#'   correction described in Details; \code{objective = "pywitness"} leaves the
#'   engine's own objective and optimizer untouched, which reproduces results
#'   from pyWitness itself, including published fits.
#' @param envname Name or path of the isolated pyWitness virtual environment.
#'
#' @return An R-native \code{"lineup_model_comparison"} object containing
#'   comparison statistics, parameters, observed and expected cells,
#'   diagnostics, specifications, and engine metadata. It contains no live
#'   Python objects.
#'
#' @details
#' This interface is restricted to fair simultaneous lineups. Explicit
#' target-absent suspect IDs are combined with other target-absent choices
#' because all innocent members share one lure distribution. The upstream
#' processor requires at least one observation in each aggregate TP/TA response
#' category; zero cells within particular confidence bins remain supported.
#' If the input includes a \code{condition} or \code{procedure} column, it must
#' contain only one non-missing value; otherwise the data must be subset before
#' fitting.
#'
#' By default the bridge corrects the pinned engine's reversed observed/expected
#' rejection-cell variance denominators in memory for each fit, without changing
#' its installed environment; \code{control = list(objective = "pywitness")}
#' turns the correction off. With the default \code{chi2_variance = "expected"},
#' every cell therefore uses the Pearson denominator. With \code{"observed"},
#' every cell uses the observed denominator (the Neyman discrepancy), requiring
#' positive observed cells. Thus, \code{loglik_at_estimate} is a multinomial likelihood
#' evaluated at the minimum-Pearson estimate, not a maximized likelihood. AIC
#' and BIC are returned as \code{NA}; computing them from a non-likelihood
#' optimum would be invalid.
#'
#' BEST-Rest and Ensemble obey
#' \deqn{DV_{ensemble} = (k - 1)DV_{best-rest}/k.}
#' They fit identically after rescaling criteria and must not be counted as
#' independent model-selection evidence. Integration is included as a
#' historically important comparator, not a recommended default.
#'
#' @examples
#' \dontrun{
#' install_pywitness()
#' fits <- fit_lineup_models(
#'   lineup_example,
#'   confidence_bins = c(0, 60, 80, 100)
#' )
#' print(fits)
#' plot(fits, type = "fit")
#' }
#'
#' @references
#' Wixted, J. T., Vul, E., Mickes, L., & Wilson, B. M. (2018). Models of
#' lineup memory. \emph{Cognitive Psychology, 105}, 81--114.
#' \doi{10.1016/j.cogpsych.2018.06.001}
#'
#' Mickes, L., Seale-Carlisle, T. M., Chen, X., & Boogert, S. (2024).
#' pyWitness 1.0: A Python eyewitness identification analysis toolkit.
#' \emph{Behavior Research Methods, 56}, 1533--1550.
#' @export
fit_lineup_models <- function(
    data,
    models = c("independent", "ensemble", "integration"),
    lineup_size = 6L,
    confidence_bins = NULL,
    variance = c("equal", "unequal"),
    shared_variance = c("estimated", "zero"),
    starts = NULL,
    control = NULL,
    envname = .pywitness_default_env) {
  variance <- match.arg(variance)
  shared_variance <- match.arg(shared_variance)
  if (!is.character(models) || !length(models) || anyNA(models)) {
    stop("models must be a non-empty character vector.", call. = FALSE)
  }
  models[models == "max"] <- "independent"
  valid_models <- c("independent", "ensemble", "best_rest", "integration")
  invalid <- setdiff(models, valid_models)
  if (length(invalid)) {
    stop("Unknown models: ", paste(invalid, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(models)) {
    stop("models cannot contain duplicates.", call. = FALSE)
  }
  .validate_lineup_model_starts(starts, models)
  if (!is.character(envname) || length(envname) != 1L || is.na(envname) ||
      !nzchar(envname)) {
    stop("envname must be one non-empty character value.", call. = FALSE)
  }

  control <- .validate_lineup_model_control(control)
  prepared <- .prepare_lineup_model_data(
    data, lineup_size = lineup_size, confidence_bins = confidence_bins
  )
  observed <- .lineup_model_cells(prepared)

  if (!reticulate::virtualenv_exists(envname)) {
    stop("The isolated pyWitness environment '", envname,
         "' does not exist. Run install_pywitness() explicitly.",
         call. = FALSE)
  }
  reticulate::use_virtualenv(envname, required = TRUE)
  if (!reticulate::py_module_available("pyWitness")) {
    stop("pyWitness is not installed in '", envname,
         "'. Run install_pywitness() explicitly.", call. = FALSE)
  }
  pyw <- reticulate::import("pyWitness", convert = TRUE)
  engine_version <- as.character(pyw$version)
  if (!.pywitness_revision_matches(engine_version)) {
    stop(
      "The installed pyWitness version ('", engine_version,
      "') does not match audited revision ", .pywitness_audited_revision,
      ". Run install_pywitness(force = TRUE).",
      call. = FALSE
    )
  }

  engine_data <- .pywitness_data_frame(prepared)
  path <- tempfile("r4lineups-pywitness-", fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(engine_data, path, row.names = FALSE, na = "")
  data_raw <- NULL
  if (control$verbose) {
    data_raw <- pyw$DataRaw(path)
  } else {
    reticulate::py_capture_output(
      data_raw <- pyw$DataRaw(path),
      type = "stdout"
    )
  }
  if (control$verbose) {
    processed <- data_raw$process()
  } else {
    processed <- NULL
    reticulate::py_capture_output(
      processed <- data_raw$process(),
      type = "stdout"
    )
  }

  fitted <- lapply(models, function(model) {
    tryCatch(
      .pywitness_fit_one(
        pyw = pyw, processed = processed, model = model,
        prepared = prepared, observed = observed, variance = variance,
        shared_variance = shared_variance, starts = starts, control = control
      ),
      error = function(e) {
        list(
          comparison = data.frame(
            model = model, converged = FALSE, status = "error",
            iterations = NA_integer_, n_parameters = NA_integer_,
            pearson_chisq = NA_real_, df = NA_integer_, p_value = NA_real_,
            loglik_at_estimate = NA_real_, AIC = NA_real_, BIC = NA_real_,
            error = conditionMessage(e), stringsAsFactors = FALSE
          ),
          parameters = NULL,
          cells = NULL
        )
      }
    )
  })
  comparison <- do.call(rbind, lapply(fitted, function(x) x$comparison))
  parameter_list <- Filter(Negate(is.null),
                           lapply(fitted, function(x) x$parameters))
  cell_list <- Filter(Negate(is.null),
                      lapply(fitted, function(x) x$cells))
  parameters <- if (length(parameter_list)) do.call(rbind, parameter_list) else NULL
  cells <- if (length(cell_list)) do.call(rbind, cell_list) else NULL

  duplicated_parameterization <- all(c("ensemble", "best_rest") %in% models)
  if (duplicated_parameterization) {
    warning("BEST-Rest and Ensemble are scale-equivalent parameterizations; ",
            "do not treat both as independent model-selection evidence.",
            call. = FALSE)
  }
  if (any(!comparison$converged)) {
    warning("One or more lineup-memory fits did not converge or failed; ",
            "inspect x$comparison and x$diagnostics.", call. = FALSE)
  }

  result <- list(
    comparison = comparison,
    parameters = parameters,
    observed = transform(observed, count = observed)[,
      c("target_present", "response", "confidence", "count"),
      drop = FALSE
    ],
    expected = if (is.null(cells)) NULL else transform(
      cells, count = expected
    )[, c("model", "target_present", "response", "confidence", "count"),
       drop = FALSE],
    cells = cells,
    diagnostics = comparison[, c("model", "converged", "status",
                                  "iterations", "error"), drop = FALSE],
    specification = list(
      lineup_size = prepared$lineup_size,
      confidence_bins = prepared$confidence_bins,
      confidence_levels = prepared$confidence_levels,
      variance = variance,
      shared_variance = shared_variance,
      n_tp = prepared$n_tp,
      n_ta = prepared$n_ta,
      target_absent_suspect_ids_collapsed = prepared$ta_suspect_collapsed,
      fair_simultaneous_lineup = TRUE,
      best_rest_ensemble_duplicate = duplicated_parameterization,
      estimator = if (identical(control$objective, "pywitness")) "pyWitness chi-squared objective (uncorrected)"
                  else if (control$chi2_variance == "expected") "minimum Pearson chi-squared" else "minimum Neyman chi-squared",
      chi2_variance = control$chi2_variance
    ),
    engine = list(
      name = "pyWitness",
      version = engine_version,
      repository = .pywitness_repository,
      audited_revision = .pywitness_audited_revision,
      installation_source = .pywitness_archive,
      environment = envname,
      objective_correction = if (identical(control$objective, "corrected"))
        "Rejection-cell variance denominators corrected per fit; installed engine unchanged"
      else "None: pyWitness objective and optimizer used as installed",
      durable_python_object = FALSE
    ),
    information_criteria = list(
      available = FALSE,
      reason = paste(
        "The audited engine minimizes Pearson chi-squared rather than",
        "maximizing likelihood; AIC and BIC are therefore not reported."
      )
    )
  )
  class(result) <- "lineup_model_comparison"
  result
}

#' @export
print.lineup_model_comparison <- function(x, digits = 3, ...) {
  cat("Wixted Lineup-Memory Model Comparison\n")
  cat("Engine:", x$engine$name, x$engine$version, "\n")
  cat("Estimator:", x$specification$estimator, "\n")
  cat("Lineup size:", x$specification$lineup_size,
      "| TP:", x$specification$n_tp,
      "| TA:", x$specification$n_ta, "\n\n")
  table <- x$comparison[, c("model", "converged", "pearson_chisq",
                            "df", "p_value", "loglik_at_estimate")]
  numeric <- vapply(table, is.numeric, logical(1))
  table[numeric] <- lapply(table[numeric], round, digits = digits)
  print(table, row.names = FALSE)
  cat("\nAIC/BIC unavailable:", x$information_criteria$reason, "\n")
  if (isTRUE(x$specification$best_rest_ensemble_duplicate)) {
    cat("BEST-Rest and Ensemble are equivalent after criterion rescaling.\n")
  }
  invisible(x)
}

#' @export
summary.lineup_model_comparison <- function(object, ...) {
  print(object, ...)
  cat("\nParameter estimates:\n")
  if (is.null(object$parameters) || !nrow(object$parameters)) {
    cat("  No successful fits.\n")
  } else {
    print(object$parameters, row.names = FALSE)
  }
  cat("\nDiagnostics:\n")
  print(object$diagnostics, row.names = FALSE)
  invisible(list(
    comparison = object$comparison,
    parameters = object$parameters,
    diagnostics = object$diagnostics,
    specification = object$specification,
    engine = object$engine
  ))
}

.lineup_model_roc <- function(cells, lineup_size, n_tp, n_ta) {
  if (is.null(cells) || !nrow(cells)) return(data.frame())
  models <- unique(cells$model)
  do.call(rbind, lapply(models, function(model) {
    model_cells <- cells[cells$model == model, , drop = FALSE]
    levels <- sort(unique(stats::na.omit(model_cells$confidence)),
                   decreasing = TRUE)
    make_curve <- function(value, source) {
      hit <- vapply(levels, function(level) {
        sum(value[
          model_cells$target_present &
            model_cells$response == "suspect" &
            model_cells$confidence >= level
        ])
      }, numeric(1)) / n_tp
      false_alarm <- vapply(levels, function(level) {
        sum(value[
          !model_cells$target_present &
            model_cells$response == "filler" &
            model_cells$confidence >= level
        ])
      }, numeric(1)) / lineup_size / n_ta
      data.frame(
        model = model,
        source = source,
        confidence = c(Inf, levels),
        false_alarm_rate = c(0, false_alarm),
        hit_rate = c(0, hit),
        stringsAsFactors = FALSE
      )
    }
    rbind(
      make_curve(model_cells$observed, "Observed"),
      make_curve(model_cells$expected, "Expected")
    )
  }))
}

#' Plot a Wixted Lineup-Memory Model Comparison
#'
#' @param x A \code{"lineup_model_comparison"} object.
#' @param type Plot type: Pearson comparison, fitted response cells, or ROC.
#' @param ... Additional arguments (currently ignored).
#' @return A ggplot object.
#' @export
plot.lineup_model_comparison <- function(
    x, type = c("comparison", "fit", "roc"), ...) {
  type <- match.arg(type)
  if (type == "comparison") {
    return(
      ggplot2::ggplot(
        x$comparison,
        ggplot2::aes(x = model, y = pearson_chisq, fill = converged)
      ) +
        ggplot2::geom_col() +
        ggplot2::labs(
          x = "Model", y = "Pearson chi-squared", fill = "Converged",
          title = "Wixted lineup-memory model comparison"
        ) +
        ggplot2::theme_minimal(base_size = 12)
    )
  }
  if (is.null(x$cells) || !nrow(x$cells)) {
    stop("No successful fitted cells are available to plot.", call. = FALSE)
  }
  if (type == "fit") {
    plot_data <- rbind(
      transform(x$cells, source = "Observed", count = observed),
      transform(x$cells, source = "Expected", count = expected)
    )
    plot_data$cell <- ifelse(
      plot_data$response == "reject",
      paste(ifelse(plot_data$target_present, "TP", "TA"), "reject"),
      paste(
        ifelse(plot_data$target_present, "TP", "TA"),
        plot_data$response, plot_data$confidence, sep = ":"
      )
    )
    return(
      ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = cell, y = count, fill = source)
      ) +
        ggplot2::geom_col(position = "dodge") +
        ggplot2::facet_wrap(~model, scales = "free_y") +
        ggplot2::labs(
          x = "Response cell", y = "Frequency", fill = NULL,
          title = "Observed and expected lineup-response frequencies"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )
    )
  }

  roc <- .lineup_model_roc(
    x$cells,
    lineup_size = x$specification$lineup_size,
    n_tp = x$specification$n_tp,
    n_ta = x$specification$n_ta
  )
  ggplot2::ggplot(
    roc,
    ggplot2::aes(
      x = false_alarm_rate, y = hit_rate,
      colour = model, linetype = source
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = "False ID rate", y = "Correct ID rate",
      colour = "Model", linetype = NULL,
      title = "Observed and fitted lineup ROC"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}
