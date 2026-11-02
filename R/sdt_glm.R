#' Fit an SDT GLM for Old/New Recognition
#'
#' Fits a single-level GLM (binomial link) to estimate SDT metrics from
#' old/new recognition responses. With centered item coding, the coefficient
#' on item status corresponds to d' (probit) or ln(OR) (logit).
#'
#' @param data Data frame containing trial-level responses.
#' @param is_old Column name indicating whether the item is old (1) or new (0).
#' @param said_old Column name indicating whether the participant said "old" (1) or "new" (0).
#' @param covariates Optional character vector of covariate column names.
#' @param link Link function for the binomial model ("probit" or "logit").
#' @param center_isold Logical. If TRUE, recodes is_old to -0.5 / +0.5.
#' @param interactions Logical. If TRUE, include is_old interactions with covariates.
#'
#' @return A \code{glm} object with SDT attributes.
#' @examples
#' set.seed(123)
#' n <- 200
#' is_old <- rep(c(0, 1), each = n / 2)
#' # d' of about 1: higher hit rate for old items
#' said_old <- rbinom(n, 1, ifelse(is_old == 1, 0.7, 0.3))
#' df <- data.frame(is_old = is_old, said_old = said_old)
#' fit <- fit_sdt_glm(df, is_old = "is_old", said_old = "said_old")
#' extract_sdt_metrics(fit)
#' @export
fit_sdt_glm <- function(data,
                        is_old,
                        said_old,
                        covariates = NULL,
                        link = "probit",
                        center_isold = TRUE,
                        interactions = FALSE) {
  link <- match.arg(link, c("probit", "logit"))
  if (!is.data.frame(data)) {
    stop("data must be a data.frame.", call. = FALSE)
  }
  if (!is.character(is_old) || length(is_old) != 1) {
    stop("is_old must be a single column name.", call. = FALSE)
  }
  if (!is.character(said_old) || length(said_old) != 1) {
    stop("said_old must be a single column name.", call. = FALSE)
  }
  if (!is.null(covariates) && !is.character(covariates)) {
    stop("covariates must be NULL or a character vector.", call. = FALSE)
  }

  if (!all(c(is_old, said_old, covariates) %in% names(data))) {
    stop("All response and covariate column names must exist in data.", call. = FALSE)
  }
  df <- data
  df[[is_old]] <- .as_binary_sdt(df[[is_old]], is_old)
  df[[said_old]] <- .as_binary_sdt(df[[said_old]], said_old)

  df$.is_old_c <- if (center_isold) df[[is_old]] - 0.5 else df[[is_old]]
  df$.said_old <- df[[said_old]]

  terms <- c(".is_old_c")
  if (!is.null(covariates) && length(covariates) > 0) {
    terms <- c(terms, covariates)
    if (isTRUE(interactions)) {
      inter_terms <- paste(".is_old_c", covariates, sep = ":", collapse = " + ")
      terms <- c(terms, inter_terms)
    }
  }
  rhs <- paste(terms, collapse = " + ")
  form <- stats::as.formula(paste(".said_old ~", rhs))

  fit <- stats::glm(form, data = df, family = stats::binomial(link = link))
  attr(fit, "sdt_is_old_term") <- ".is_old_c"
  attr(fit, "sdt_centered") <- center_isold
  attr(fit, "sdt_link") <- link
  attr(fit, "sdt_is_old_col") <- is_old
  attr(fit, "sdt_said_old_col") <- said_old
  fit
}

#' Fit an SDT GLMM for Old/New Recognition
#'
#' Fits a multilevel GLMM (binomial link) to estimate SDT metrics from
#' old/new recognition responses, using random effects for participants and
#' optionally items.
#'
#' @param data Data frame containing trial-level responses.
#' @param is_old Column name indicating whether the item is old (1) or new (0).
#' @param said_old Column name indicating whether the participant said "old" (1) or "new" (0).
#' @param subject_id Column name identifying participants.
#' @param covariates Optional character vector of covariate column names.
#' @param item_id Optional column name for item-level random intercepts.
#' @param link Link function for the binomial model ("probit" or "logit").
#' @param center_isold Logical. If TRUE, recodes is_old to -0.5 / +0.5.
#' @param random_slope Logical. If TRUE, include random slope for is_old by subject.
#' @param interactions Logical. If TRUE, include is_old interactions with covariates.
#'
#' @return A \code{glmerMod} object with SDT attributes.
#' @examples
#' \donttest{
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   set.seed(123)
#'   n_subj <- 20
#'   n_trial <- 20
#'   df <- expand.grid(subject = seq_len(n_subj), trial = seq_len(n_trial))
#'   df$is_old <- rep(c(0, 1), length.out = nrow(df))
#'   df$said_old <- rbinom(nrow(df), 1, ifelse(df$is_old == 1, 0.7, 0.3))
#'   fit <- fit_sdt_glmm(df, is_old = "is_old", said_old = "said_old",
#'                       subject_id = "subject", random_slope = FALSE)
#'   extract_sdt_metrics(fit)
#' }
#' }
#' @export
fit_sdt_glmm <- function(data,
                         is_old,
                         said_old,
                         subject_id,
                         covariates = NULL,
                         item_id = NULL,
                         link = "probit",
                         center_isold = TRUE,
                         random_slope = TRUE,
                         interactions = FALSE) {
  link <- match.arg(link, c("probit", "logit"))
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' is required for fit_sdt_glmm().", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("data must be a data.frame.", call. = FALSE)
  }
  if (!is.character(is_old) || length(is_old) != 1) {
    stop("is_old must be a single column name.", call. = FALSE)
  }
  if (!is.character(said_old) || length(said_old) != 1) {
    stop("said_old must be a single column name.", call. = FALSE)
  }
  if (!is.character(subject_id) || length(subject_id) != 1) {
    stop("subject_id must be a single column name.", call. = FALSE)
  }
  if (!is.null(item_id) && (!is.character(item_id) || length(item_id) != 1)) {
    stop("item_id must be NULL or a single column name.", call. = FALSE)
  }
  if (!is.null(covariates) && !is.character(covariates)) {
    stop("covariates must be NULL or a character vector.", call. = FALSE)
  }

  required_names <- c(is_old, said_old, subject_id, item_id, covariates)
  if (!all(required_names %in% names(data))) {
    stop("All response, grouping, item, and covariate column names must exist in data.",
         call. = FALSE)
  }
  df <- data
  df[[is_old]] <- .as_binary_sdt(df[[is_old]], is_old)
  df[[said_old]] <- .as_binary_sdt(df[[said_old]], said_old)

  df$.is_old_c <- if (center_isold) df[[is_old]] - 0.5 else df[[is_old]]
  df$.said_old <- df[[said_old]]
  df$.subject_id <- df[[subject_id]]
  if (!is.null(item_id)) {
    df$.item_id <- df[[item_id]]
  }

  terms <- c(".is_old_c")
  if (!is.null(covariates) && length(covariates) > 0) {
    terms <- c(terms, covariates)
    if (isTRUE(interactions)) {
      inter_terms <- paste(".is_old_c", covariates, sep = ":", collapse = " + ")
      terms <- c(terms, inter_terms)
    }
  }
  rhs <- paste(terms, collapse = " + ")

  subj_re <- if (isTRUE(random_slope)) {
    "(1 + .is_old_c | .subject_id)"
  } else {
    "(1 | .subject_id)"
  }
  re_terms <- subj_re
  if (!is.null(item_id)) {
    re_terms <- paste(re_terms, "+ (1 | .item_id)")
  }

  form <- stats::as.formula(paste(".said_old ~", rhs, "+", re_terms))

  fit <- lme4::glmer(form, data = df, family = stats::binomial(link = link))
  attr(fit, "sdt_is_old_term") <- ".is_old_c"
  attr(fit, "sdt_centered") <- center_isold
  attr(fit, "sdt_link") <- link
  attr(fit, "sdt_is_old_col") <- is_old
  attr(fit, "sdt_said_old_col") <- said_old
  fit
}

.as_binary_sdt <- function(x, name) {
  if (is.logical(x)) return(as.integer(x))
  if (!is.numeric(x) || anyNA(x) || !all(x %in% c(0, 1))) {
    stop(name, " must contain only logical or numeric 0/1 values.", call. = FALSE)
  }
  as.numeric(x)
}

#' Extract SDT Metrics from a GLM/GLMM
#'
#' Extracts d' (probit) or ln(OR) (logit) from SDT GLM/GLMM fits.
#' Criterion is reported only when is_old was centered to -0.5/+0.5.
#' With covariates these quantities are evaluated at zero/reference covariates;
#' interactions can change them at other covariate values. GLMM coefficients
#' are conditional latent-scale effects, not marginal population SDT measures.
#' A logit coefficient is log odds ratio rather than normal-theory d-prime.
#'
#' @param model A glm or glmerMod object produced by fit_sdt_glm/fit_sdt_glmm.
#' @param link Optional. If NULL, inferred from the model family.
#'
#' @return A list containing d' or ln(OR), criterion (if available), and metadata.
#' @examples
#' set.seed(123)
#' n <- 200
#' is_old <- rep(c(0, 1), each = n / 2)
#' said_old <- rbinom(n, 1, ifelse(is_old == 1, 0.7, 0.3))
#' df <- data.frame(is_old = is_old, said_old = said_old)
#' fit <- fit_sdt_glm(df, is_old = "is_old", said_old = "said_old")
#' metrics <- extract_sdt_metrics(fit)
#' metrics$estimate  # d' under the probit link
#' @export
extract_sdt_metrics <- function(model, link = NULL) {
  if (is.null(link)) {
    link <- stats::family(model)$link
  }
  link <- match.arg(link, c("probit", "logit"))

  if (inherits(model, "glmerMod")) {
    coef_vec <- lme4::fixef(model)
  } else {
    coef_vec <- stats::coef(model)
  }
  term <- attr(model, "sdt_is_old_term")
  if (is.null(term) || !term %in% names(coef_vec)) {
    candidates <- grep("is_old", names(coef_vec), value = TRUE)
    if (length(candidates) > 0) {
      term <- candidates[1]
    } else {
      stop("Could not identify is_old coefficient in model.", call. = FALSE)
    }
  }

  slope <- unname(coef_vec[[term]])
  intercept <- unname(coef_vec[["(Intercept)"]])

  centered <- isTRUE(attr(model, "sdt_centered"))
  criterion <- if (centered) -intercept else NA_real_

  if (link == "probit") {
    list(
      metric = "dprime",
      estimate = slope,
      criterion = criterion,
      link = link,
      centered = centered
    )
  } else {
    list(
      metric = "ln_or",
      estimate = slope,
      criterion = criterion,
      link = link,
      centered = centered
    )
  }
}
