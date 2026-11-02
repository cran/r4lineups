#' Restricted Independent-Observations/MAX Model for Lineup Counts
#'
#' Fits the equal-variance, independent-signal, single-criterion MAX model to
#' aggregate simultaneous-lineup counts using minimum-Pearson chi-squared
#' estimation. Estimates d' (sensitivity) and lambda (criterion) simultaneously,
#' supporting one or two conditions and optional cross-condition constraints.
#'
#' @param n_hit Number of correct suspect identifications in target-present (TP)
#'   lineups (condition 1).
#' @param n_tp_choose Total number of participants who chose any lineup member in
#'   TP lineups (condition 1). Must be >= n_hit.
#' @param n_fa Number of participants who chose any lineup member in target-absent
#'   (TA) lineups (condition 1).
#' @param N_tp Total number of participants in TP lineups (condition 1).
#' @param N_ta Total number of participants in TA lineups (condition 1).
#' @param n_hit_2,n_tp_choose_2,n_fa_2,N_tp_2,N_ta_2 Corresponding counts for an
#'   optional second condition. All five must be supplied together or not at all.
#' @param n Lineup size (number of members). Default 6.
#' @param constrain_d Logical. If \code{TRUE} and two conditions are supplied,
#'   constrains d' to be equal across conditions. Default \code{FALSE}.
#' @param constrain_c Logical. If \code{TRUE} and two conditions are supplied,
#'   constrains lambda to be equal across conditions. Default \code{FALSE}.
#' @param start Named numeric vector of starting values for the optimizer.
#'   If \code{NULL} (default), reasonable starting values are chosen automatically.
#'   Names should be \code{"d1"}, \code{"lambda1"} (and \code{"d2"},
#'   \code{"lambda2"} for two conditions).
#' @param nboot Number of parametric bootstrap replicates for confidence intervals.
#'   Default 0 (no bootstrap). Set to e.g. 1000 to obtain bootstrap CIs.
#' @param ci_level Confidence level for bootstrap CIs. Default 0.95.
#' @param seed Optional integer seed for reproducible bootstrap.
#'
#' @return An object of class \code{"max_sdt_fit"} containing:
#' \describe{
#'   \item{dprime_1, lambda_1}{Estimated d' and criterion for condition 1.}
#'   \item{dprime_2, lambda_2}{Estimated d' and criterion for condition 2 (if supplied).}
#'   \item{chisq}{Chi-squared goodness-of-fit statistic.}
#'   \item{df}{Degrees of freedom for the GoF test.}
#'   \item{p_value}{P-value for the GoF test.}
#'   \item{observed}{Named vector of observed counts.}
#'   \item{predicted}{Named vector of predicted counts.}
#'   \item{convergence}{Convergence code from \code{optim()} (0 = success).}
#'   \item{constrain_d, constrain_c, n_conditions, n}{Input metadata.}
#'   \item{boot_ci}{Bootstrap confidence intervals (if nboot > 0).}
#'   \item{boot_params}{Full bootstrap parameter matrix (if nboot > 0). Rows for
#'     replicates that failed to converge are \code{NA}, so summarise with
#'     \code{na.rm = TRUE}.}
#'   \item{boot_n_used, boot_n_failed}{Number of bootstrap replicates that
#'     converged and that did not (if nboot > 0).}
#' }
#'
#' @details
#' This is a restricted Independent Observations/MAX model for simultaneous,
#' fair lineups. Fillers are IID \eqn{N(0, 1)}, the culprit signal is
#' \eqn{N(d', 1)}, memory signals are independent, and a single criterion is
#' applied to the largest signal. Target-absent \code{n_fa} therefore means a
#' choice of any lineup member, not only a designated innocent suspect.
#'
#' For a lineup of \eqn{n} members with culprit signal strength \eqn{d'} and
#' response criterion \eqn{\lambda}:
#' \deqn{P(\text{correct ID}) = \int_{\lambda}^{\infty} \phi(x - d') \Phi(x)^{n-1}\,dx}
#' \deqn{P(\text{choose anyone} \mid \text{TP}) = 1 - \Phi(\lambda - d')\Phi(\lambda)^{n-1}}
#' \deqn{P(\text{choose anyone} \mid \text{TA}) = 1 - \Phi(\lambda)^n}
#'
#' Parameters are estimated by minimizing the Pearson chi-squared discrepancy
#' between the mutually exclusive response cells: target-present correct ID,
#' filler ID and rejection; and target-absent choice and rejection. This is not
#' maximum-likelihood estimation and is not the multi-criterion likelihood model
#' of Wixted et al. (2018). Nested comparisons (equal d' or equal lambda across
#' conditions) are supported via \code{\link{compare_max_sdt}}.
#'
#' Estimates are conditional on the model assumptions. Correlated signals,
#' unequal variances, confidence criteria, sequential presentation, unfair
#' lineups, and the Ensemble and Integration decision variables are outside the
#' scope of this function.
#'
#' Parametric bootstrap CIs are obtained by simulating data from the fitted model
#' and re-fitting. Replicates that fail to converge are dropped and counted in
#' \code{boot_n_failed}.
#'
#' @section Provenance:
#' The implementation follows R scripts written by Andrew M. Smith (Iowa State
#' University) for the analyses in Reisberg and Pezdek (2024), which fit what he
#' describes as \dQuote{the MAX SDT model (sometimes called the
#' compound-decision model)}. The default example counts are the Profile-lineup
#' condition from that work. The constrained fits (\code{constrain_d},
#' \code{constrain_c}) and the nested chi-square comparison in
#' \code{\link{compare_max_sdt}} follow the procedure described there.
#'
#' @section Relation to Smith's scripts:
#' Estimates from this function will \strong{not} match those scripts exactly,
#' and the reason is the set of cells entering the discrepancy function.
#'
#' Smith's scripts fit culprit identifications, \emph{total} target-present
#' choices and \emph{total} target-absent choices, together with the rejection
#' counts. Culprit identifications therefore enter twice — once on their own and
#' again inside total target-present choices — so the cells do not partition the
#' sample, and the filler count is never fitted directly.
#'
#' This function uses five mutually exclusive cells per condition: correct
#' identification, filler identification, target-present rejection,
#' target-absent choice, target-absent rejection. A Pearson chi-square is
#' defined for a partition, so this form keeps the statistic and its degrees of
#' freedom interpretable, and it fits the observed counts better on that
#' criterion.
#'
#' On the Profile-lineup counts used in the examples below, the two give
#' d' = 1.94 (this function) against d' = 1.78 (Smith's script) for the first
#' condition. Neither is a coding error; they minimise different discrepancies.
#'
#' @references
#' Duncan, M. (2006). \emph{A signal detection model of compound decision
#' tasks}. DRDC Toronto TR 2006-256.
#'
#' Reisberg, D., & Pezdek, K. (2024). Data analysed with the MAX SDT model in
#' scripts supplied by A. M. Smith (personal communication, 4 April 2024).
#'
#' Kaesler, M., Dunn, J. C., Ransom, K., & Semmler, C. (2020). Do sequential
#' lineups impair underlying discriminability? \emph{Cognitive Research:
#' Principles and Implications, 5}, 35. \doi{10.1186/s41235-020-00234-5}
#'
#' Wixted, J. T., Vul, E., Mickes, L., & Wilson, B. M. (2018). Models of lineup
#' memory. \emph{Cognitive Psychology, 105}, 81--114.
#' \doi{10.1016/j.cogpsych.2018.06.001}
#'
#' @seealso \code{\link{compare_max_sdt}}, \code{\link{estimate_msdt_params}},
#'   \code{\link{sdt_compare}}
#'
#' @examples
#' # Single condition
#' fit1 <- fit_max_sdt(
#'   n_hit = 69, n_tp_choose = 82, n_fa = 64,
#'   N_tp = 96, N_ta = 106, n = 6
#' )
#' print(fit1)
#'
#' \donttest{
#' # Two conditions, free parameters (each two-condition fit takes a few seconds)
#' fit_free <- fit_max_sdt(
#'   n_hit = 69, n_tp_choose = 82, n_fa = 64,
#'   N_tp = 96, N_ta = 106,
#'   n_hit_2 = 68, n_tp_choose_2 = 78, n_fa_2 = 42,
#'   N_tp_2 = 96, N_ta_2 = 96, n = 6
#' )
#'
#' # Two conditions, equal d' (constrained)
#' fit_eqd <- fit_max_sdt(
#'   n_hit = 69, n_tp_choose = 82, n_fa = 64,
#'   N_tp = 96, N_ta = 106,
#'   n_hit_2 = 68, n_tp_choose_2 = 78, n_fa_2 = 42,
#'   N_tp_2 = 96, N_ta_2 = 96, n = 6,
#'   constrain_d = TRUE
#' )
#'
#' # Compare models
#' cmp <- compare_max_sdt(fit_free, fit_eqd)
#' print(cmp)
#' }
#'
#' @importFrom stats optim integrate pnorm dnorm qchisq pchisq quantile rbinom
#' @export
fit_max_sdt <- function(n_hit, n_tp_choose, n_fa,
                         N_tp, N_ta,
                         n_hit_2     = NULL,
                         n_tp_choose_2 = NULL,
                         n_fa_2      = NULL,
                         N_tp_2      = NULL,
                         N_ta_2      = NULL,
                         n           = 6,
                         constrain_d = FALSE,
                         constrain_c = FALSE,
                         start       = NULL,
                         nboot       = 0,
                         ci_level    = 0.95,
                         seed        = NULL) {

  # --- input validation ---
  .max_sdt_check_count <- function(val, nm) {
    if (!is.numeric(val) || length(val) != 1 || val < 0)
      stop(sprintf("%s must be a non-negative numeric scalar.", nm), call. = FALSE)
  }
  for (nm in c("n_hit","n_tp_choose","n_fa","N_tp","N_ta"))
    .max_sdt_check_count(get(nm), nm)
  if (n_hit > n_tp_choose)
    stop("n_hit cannot exceed n_tp_choose.", call. = FALSE)
  if (n_tp_choose > N_tp)
    stop("n_tp_choose cannot exceed N_tp.", call. = FALSE)
  if (n_fa > N_ta)
    stop("n_fa cannot exceed N_ta.", call. = FALSE)

  cond2_args <- list(n_hit_2, n_tp_choose_2, n_fa_2, N_tp_2, N_ta_2)
  cond2_nms  <- c("n_hit_2","n_tp_choose_2","n_fa_2","N_tp_2","N_ta_2")
  cond2_null <- vapply(cond2_args, is.null, logical(1))
  if (!all(cond2_null) && any(cond2_null))
    stop("All five condition-2 arguments must be supplied together.", call. = FALSE)
  two_cond <- !all(cond2_null)

  if (two_cond) {
    for (i in seq_along(cond2_args))
      .max_sdt_check_count(cond2_args[[i]], cond2_nms[i])
    if (n_hit_2 > n_tp_choose_2)
      stop("n_hit_2 cannot exceed n_tp_choose_2.", call. = FALSE)
    if (n_tp_choose_2 > N_tp_2)
      stop("n_tp_choose_2 cannot exceed N_tp_2.", call. = FALSE)
    if (n_fa_2 > N_ta_2)
      stop("n_fa_2 cannot exceed N_ta_2.", call. = FALSE)
  }

  if (!two_cond && (constrain_d || constrain_c))
    warning("constrain_d and constrain_c only apply with two conditions.", call. = FALSE)

  if (!is.numeric(n) || n < 2)
    stop("n must be an integer >= 2.", call. = FALSE)
  if (!is.numeric(nboot) || nboot < 0)
    stop("nboot must be a non-negative integer.", call. = FALSE)
  if (!is.numeric(ci_level) || ci_level <= 0 || ci_level >= 1)
    stop("ci_level must be in (0, 1).", call. = FALSE)

  # --- core probability functions ---
  hit_prob <- function(d, lambda) {
    integrand <- function(x) dnorm(x, mean = d, sd = 1) * pnorm(x)^(n - 1)
    integrate(integrand, lambda, Inf, rel.tol = 1e-6)$value
  }
  tpc_prob <- function(d, lambda) {
    1 - pnorm(lambda - d) * pnorm(lambda)^(n - 1)
  }
  fa_prob <- function(lambda) {
    1 - pnorm(lambda)^n
  }

  # --- chi-squared objective ---
  # Cells are a proper multinomial partition of each lineup type:
  #   TP: correct ID | filler ID (= chose - hit) | reject
  #   TA: false alarm | reject
  # (Using n_tp_choose itself as a cell double-counts hits, which are nested in it.)
  obs1 <- c(n_hit       = n_hit,
            n_filler_tp = n_tp_choose - n_hit,
            n_reject_tp = N_tp - n_tp_choose,
            n_fa        = n_fa,
            n_reject_ta = N_ta - n_fa)

  if (two_cond) {
    obs2 <- c(n_hit       = n_hit_2,
              n_filler_tp = n_tp_choose_2 - n_hit_2,
              n_reject_tp = N_tp_2 - n_tp_choose_2,
              n_fa        = n_fa_2,
              n_reject_ta = N_ta_2 - n_fa_2)
  }

  predicted_from_params <- function(d, lambda, N_tp_i, N_ta_i) {
    ph  <- hit_prob(d, lambda)
    pt  <- tpc_prob(d, lambda)
    pf  <- fa_prob(lambda)
    c(n_hit       = ph * N_tp_i,
      n_filler_tp = (pt - ph) * N_tp_i,
      n_reject_tp = (1 - pt) * N_tp_i,
      n_fa        = pf * N_ta_i,
      n_reject_ta = (1 - pf) * N_ta_i)
  }

  chisq_one <- function(obs, pred) {
    # Floor the expected counts rather than excluding low-expected cells.
    # Excluding cells (the old `pred > 0.5` rule) let the optimiser hide on a flat
    # plateau at low lambda, where reject predictions collapse to ~0 and dropped
    # out of the objective -- producing a spurious boundary fit.
    pred <- pmax(pred, 1e-8)
    sum((obs - pred)^2 / pred)
  }

  # parameter layout depends on constraints
  if (!two_cond) {
    # 2 free parameters: d1, lambda1
    obj <- function(parms) {
      d1 <- parms[1]; lam1 <- parms[2]
      pred <- predicted_from_params(d1, lam1, N_tp, N_ta)
      chisq_one(obs1, pred)
    }
    np <- 2
    lower <- c(0, -5)
    upper <- c(10,  5)
    if (is.null(start)) start <- c(1, 0)
    names(start) <- c("d1", "lambda1")
  } else if (!constrain_d && !constrain_c) {
    # 4 free: d1, d2, lambda1, lambda2
    obj <- function(parms) {
      d1 <- parms[1]; d2 <- parms[2]
      lam1 <- parms[3]; lam2 <- parms[4]
      pred1 <- predicted_from_params(d1, lam1, N_tp,   N_ta)
      pred2 <- predicted_from_params(d2, lam2, N_tp_2, N_ta_2)
      chisq_one(obs1, pred1) + chisq_one(obs2, pred2)
    }
    np <- 4
    lower <- c(0, 0, -5, -5)
    upper <- c(10, 10, 5, 5)
    if (is.null(start)) start <- c(1, 1, 0, 0)
    names(start) <- c("d1", "d2", "lambda1", "lambda2")
  } else if (constrain_d && !constrain_c) {
    # 3: d (shared), lambda1, lambda2
    obj <- function(parms) {
      d <- parms[1]; lam1 <- parms[2]; lam2 <- parms[3]
      pred1 <- predicted_from_params(d, lam1, N_tp,   N_ta)
      pred2 <- predicted_from_params(d, lam2, N_tp_2, N_ta_2)
      chisq_one(obs1, pred1) + chisq_one(obs2, pred2)
    }
    np <- 3
    lower <- c(0, -5, -5)
    upper <- c(10, 5, 5)
    if (is.null(start)) start <- c(1, 0, 0)
    names(start) <- c("d", "lambda1", "lambda2")
  } else if (!constrain_d && constrain_c) {
    # 3: d1, d2, lambda (shared)
    obj <- function(parms) {
      d1 <- parms[1]; d2 <- parms[2]; lam <- parms[3]
      pred1 <- predicted_from_params(d1, lam, N_tp,   N_ta)
      pred2 <- predicted_from_params(d2, lam, N_tp_2, N_ta_2)
      chisq_one(obs1, pred1) + chisq_one(obs2, pred2)
    }
    np <- 3
    lower <- c(0, 0, -5)
    upper <- c(10, 10, 5)
    if (is.null(start)) start <- c(1, 1, 0)
    names(start) <- c("d1", "d2", "lambda")
  } else {
    # 2: d (shared), lambda (shared)
    obj <- function(parms) {
      d <- parms[1]; lam <- parms[2]
      pred1 <- predicted_from_params(d, lam, N_tp,   N_ta)
      pred2 <- predicted_from_params(d, lam, N_tp_2, N_ta_2)
      chisq_one(obs1, pred1) + chisq_one(obs2, pred2)
    }
    np <- 2
    lower <- c(0, -5)
    upper <- c(10,  5)
    if (is.null(start)) start <- c(1, 0)
    names(start) <- c("d", "lambda")
  }

  # Deterministic multistart. The chi-squared surface is multimodal (a flat plateau
  # at low lambda, a spike near lambda = 0), so a single start can converge to a
  # non-global / boundary solution. Run L-BFGS-B from a grid spanning the box --
  # finer on the criterion (lambda) dimensions, which carry the multimodality --
  # then polish the best point with Nelder-Mead. No RNG, so the fit is reproducible.
  axis_for <- function(lo, hi) {
    if (lo < 0) seq(-2, 2, by = 1) else c(0.5, 1.5, 3)
  }
  start_grid <- as.matrix(expand.grid(Map(axis_for, lower, upper)))
  candidates <- c(list(start), lapply(seq_len(nrow(start_grid)),
                                      function(r) start_grid[r, ]))
  opt <- NULL
  for (s in candidates) {
    s <- pmin(upper, pmax(lower, as.numeric(s)))
    names(s) <- names(start)
    cand <- tryCatch(
      optim(par = s, fn = obj, method = "L-BFGS-B", lower = lower, upper = upper),
      error = function(e) NULL
    )
    if (!is.null(cand) && (is.null(opt) || cand$value < opt$value)) opt <- cand
  }
  if (is.null(opt))
    stop("Optimisation failed from all starting values.", call. = FALSE)
  # Polish; report convergence honestly (do not mask a non-converged result).
  # A polish that converges without being worse than the best start is the
  # converged solution: at a flat minimum the best L-BFGS-B start can stop with
  # a line-search code although no start does better.
  opt_nm <- tryCatch(
    optim(par = opt$par, fn = obj, method = "Nelder-Mead",
          control = list(maxit = 5000)),
    error = function(e) NULL
  )
  if (!is.null(opt_nm)) {
    tol <- 1e-8 * (1 + abs(opt$value))
    if (opt_nm$value < opt$value ||
        (opt_nm$convergence == 0L && opt_nm$value <= opt$value + tol)) {
      opt <- opt_nm
    }
  }

  phat <- opt$par
  chisq_val <- opt$value

  # GoF df = (independent cells) - (free params). Each condition contributes a
  # 3-category TP multinomial (2 df) + a 2-category TA multinomial (1 df) = 3,
  # because the row totals N_tp and N_ta are fixed. The raw cell count (5 or 10)
  # would over-state df by one per fixed total.
  indep_cells <- 3L * (if (two_cond) 2L else 1L)
  df_gof  <- indep_cells - np
  p_gof   <- pchisq(chisq_val, df = df_gof, lower.tail = FALSE)

  # reconstruct estimates by name
  extract_estimates <- function(parms) {
    nms <- names(parms)
    d1  <- if ("d1" %in% nms) parms["d1"] else if ("d" %in% nms) parms["d"] else NA_real_
    d2  <- if ("d2" %in% nms) parms["d2"] else if ("d" %in% nms) parms["d"] else NA_real_
    l1  <- if ("lambda1" %in% nms) parms["lambda1"] else if ("lambda" %in% nms) parms["lambda"] else NA_real_
    l2  <- if ("lambda2" %in% nms) parms["lambda2"] else if ("lambda" %in% nms) parms["lambda"] else NA_real_
    list(d1 = unname(d1), d2 = unname(d2), l1 = unname(l1), l2 = unname(l2))
  }

  est <- extract_estimates(phat)
  pred1 <- predicted_from_params(est$d1, est$l1, N_tp, N_ta)
  obs_vec  <- obs1
  pred_vec <- pred1
  if (two_cond) {
    pred2    <- predicted_from_params(est$d2, est$l2, N_tp_2, N_ta_2)
    obs_vec  <- c(obs1,  obs2)
    pred_vec <- c(pred1, pred2)
    names(obs_vec)  <- c(paste0(names(obs1),  "_cond1"), paste0(names(obs2),  "_cond2"))
    names(pred_vec) <- c(paste0(names(pred1), "_cond1"), paste0(names(pred2), "_cond2"))
  }

  result <- list(
    dprime_1       = est$d1,
    lambda_1       = est$l1,
    dprime_2       = if (two_cond) est$d2 else NULL,
    lambda_2       = if (two_cond) est$l2 else NULL,
    chisq          = chisq_val,
    df             = df_gof,
    p_value        = p_gof,
    observed       = obs_vec,
    predicted      = pred_vec,
    convergence    = opt$convergence,
    constrain_d    = constrain_d,
    constrain_c    = constrain_c,
    n_conditions   = if (two_cond) 2L else 1L,
    n              = n,
    n_params       = np,
    boot_ci        = NULL,
    boot_params    = NULL
  )

  # --- bootstrap ---
  if (nboot > 0) {
    restore_rng <- .local_seed(seed)
    on.exit(restore_rng(), add = TRUE)
    boot_mat <- matrix(NA_real_, nrow = nboot, ncol = np,
                       dimnames = list(NULL, names(phat)))
    sim_counts <- function(d, lam, N_tp_i, N_ta_i) {
      ph <- hit_prob(d, lam)
      pt <- tpc_prob(d, lam)
      pf <- fa_prob(lam)
      n_tp_c <- rbinom(1, N_tp_i, pt)
      n_hi_c <- rbinom(1, n_tp_c, ifelse(pt > 0, ph / pt, 0))
      n_fa_c <- rbinom(1, N_ta_i, pf)
      list(n_hit = n_hi_c, n_tp_choose = n_tp_c, n_fa = n_fa_c)
    }
    for (b in seq_len(nboot)) {
      s1 <- sim_counts(est$d1, est$l1, N_tp, N_ta)
      b_start <- phat + rnorm(np, 0, 0.05)
      b_start <- pmax(lower, pmin(upper, b_start))
      if (two_cond) {
        s2 <- sim_counts(est$d2, est$l2, N_tp_2, N_ta_2)
        b_obj <- .max_sdt_make_obj(
          n_hit = s1$n_hit, n_tp_choose = s1$n_tp_choose, n_fa = s1$n_fa,
          N_tp = N_tp, N_ta = N_ta,
          n_hit_2 = s2$n_hit, n_tp_choose_2 = s2$n_tp_choose, n_fa_2 = s2$n_fa,
          N_tp_2 = N_tp_2, N_ta_2 = N_ta_2,
          n = n, constrain_d = constrain_d, constrain_c = constrain_c,
          hit_prob = hit_prob, tpc_prob = tpc_prob, fa_prob = fa_prob
        )
      } else {
        b_obj <- .max_sdt_make_obj(
          n_hit = s1$n_hit, n_tp_choose = s1$n_tp_choose, n_fa = s1$n_fa,
          N_tp = N_tp, N_ta = N_ta,
          n_hit_2 = NULL, n_tp_choose_2 = NULL, n_fa_2 = NULL,
          N_tp_2 = NULL, N_ta_2 = NULL,
          n = n, constrain_d = FALSE, constrain_c = FALSE,
          hit_prob = hit_prob, tpc_prob = tpc_prob, fa_prob = fa_prob
        )
      }
      b_opt <- tryCatch(
        optim(par = b_start, fn = b_obj,
              method = "L-BFGS-B", lower = lower, upper = upper),
        error = function(e) list(par = rep(NA_real_, np), convergence = 99L)
      )
      if (b_opt$convergence == 0) boot_mat[b, ] <- b_opt$par
    }
    good <- complete.cases(boot_mat)
    n_failed <- sum(!good)
    if (n_failed == nboot) {
      stop("Every bootstrap replicate failed to converge; no intervals can be ",
           "formed. The fit itself may be poorly determined.", call. = FALSE)
    }
    tail_p <- (1 - ci_level) / 2
    result$boot_ci <- apply(boot_mat[good, , drop = FALSE], 2,
                            quantile, probs = c(tail_p, 1 - tail_p),
                            na.rm = TRUE)
    result$boot_params <- boot_mat
    # Replicates that failed to converge are left as NA rows. Record how many,
    # so that the count is visible rather than silently absorbed: a user taking
    # sd() over boot_params otherwise gets NA with no explanation.
    result$boot_n_failed <- n_failed
    result$boot_n_used <- sum(good)
    rownames(result$boot_ci) <- c("lower", "upper")
    if (n_failed > 0) {
      warning(n_failed, " of ", nboot, " bootstrap replicates failed to ",
              "converge and were dropped. Intervals rest on ", sum(good),
              " replicates and may be unreliable.", call. = FALSE)
    }
  }

  class(result) <- "max_sdt_fit"
  result
}

# Internal helper: re-build objective for bootstrap resampling
.max_sdt_make_obj <- function(n_hit, n_tp_choose, n_fa, N_tp, N_ta,
                               n_hit_2, n_tp_choose_2, n_fa_2, N_tp_2, N_ta_2,
                               n, constrain_d, constrain_c,
                               hit_prob, tpc_prob, fa_prob) {
  two_cond <- !is.null(n_hit_2)
  obs1 <- c(n_hit = n_hit, n_filler_tp = n_tp_choose - n_hit,
            n_reject_tp = N_tp - n_tp_choose,
            n_fa = n_fa, n_reject_ta = N_ta - n_fa)
  if (two_cond)
    obs2 <- c(n_hit = n_hit_2, n_filler_tp = n_tp_choose_2 - n_hit_2,
              n_reject_tp = N_tp_2 - n_tp_choose_2,
              n_fa = n_fa_2, n_reject_ta = N_ta_2 - n_fa_2)

  pred_f <- function(d, lam, Ntp, Nta) {
    ph <- hit_prob(d, lam); pt <- tpc_prob(d, lam); pf <- fa_prob(lam)
    c(n_hit = ph*Ntp, n_filler_tp = (pt-ph)*Ntp, n_reject_tp = (1-pt)*Ntp,
      n_fa = pf*Nta, n_reject_ta = (1-pf)*Nta)
  }
  csq <- function(obs, pred) {
    pred <- pmax(pred, 1e-8)
    sum((obs-pred)^2/pred)
  }

  if (!two_cond) {
    function(parms) csq(obs1, pred_f(parms[1], parms[2], N_tp, N_ta))
  } else if (!constrain_d && !constrain_c) {
    function(parms)
      csq(obs1, pred_f(parms[1], parms[3], N_tp,   N_ta)) +
      csq(obs2, pred_f(parms[2], parms[4], N_tp_2, N_ta_2))
  } else if (constrain_d && !constrain_c) {
    function(parms)
      csq(obs1, pred_f(parms[1], parms[2], N_tp,   N_ta)) +
      csq(obs2, pred_f(parms[1], parms[3], N_tp_2, N_ta_2))
  } else if (!constrain_d && constrain_c) {
    function(parms)
      csq(obs1, pred_f(parms[1], parms[3], N_tp,   N_ta)) +
      csq(obs2, pred_f(parms[2], parms[3], N_tp_2, N_ta_2))
  } else {
    function(parms)
      csq(obs1, pred_f(parms[1], parms[2], N_tp,   N_ta)) +
      csq(obs2, pred_f(parms[1], parms[2], N_tp_2, N_ta_2))
  }
}

#' @export
print.max_sdt_fit <- function(x, digits = 3, ...) {
  nc <- x$n_conditions
  cat("Restricted Independent-Observations/MAX SDT Model\n")
  cat(sprintf("  Lineup size: %d | Conditions: %d | Free parameters: %d\n",
              x$n, nc, x$n_params))
  cat(sprintf("  Convergence: %s\n",
              if (x$convergence == 0) "OK" else paste("code", x$convergence)))
  cat("\nParameter estimates:\n")
  if (nc == 1L) {
    cat(sprintf("  d' = %.3f | lambda = %.3f\n", x$dprime_1, x$lambda_1))
  } else {
    cat(sprintf("  Condition 1: d' = %.3f | lambda = %.3f\n",
                x$dprime_1, x$lambda_1))
    cat(sprintf("  Condition 2: d' = %.3f | lambda = %.3f\n",
                x$dprime_2, x$lambda_2))
    cat(sprintf("  (constrain_d = %s, constrain_c = %s)\n",
                x$constrain_d, x$constrain_c))
  }
  cat(sprintf("\nGoodness of fit: chi-sq(%.0f) = %.3f, p = %.4f\n",
              x$df, x$chisq, x$p_value))
  cat("\nObserved vs predicted:\n")
  tbl <- rbind(Observed = round(x$observed, 1),
               Predicted = round(x$predicted, 1))
  print(tbl)
  if (!is.null(x$boot_ci)) {
    cat("\nBootstrap CIs:\n")
    print(round(x$boot_ci, digits))
  }
  invisible(x)
}

#' @export
plot.max_sdt_fit <- function(x, ...) {
  obs  <- x$observed
  pred <- x$predicted
  nm   <- names(obs)
  df   <- data.frame(
    cell  = rep(factor(nm, levels = nm), 2),
    count = c(obs, pred),
    type  = rep(c("Observed", "Predicted"), each = length(obs))
  )
  ggplot2::ggplot(df, ggplot2::aes(x = cell, y = count, fill = type)) +
    ggplot2::geom_col(position = "dodge", alpha = 0.8) +
    ggplot2::scale_fill_manual(values = c(Observed = "#72b7b2",
                                          Predicted = "#e15759")) +
    ggplot2::labs(x = "Cell", y = "Count", fill = NULL,
                  title = "Restricted MAX SDT: Observed vs Predicted frequencies") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
}

#' Compare Two Nested MAX SDT Models
#'
#' Performs a chi-squared difference test comparing a less constrained (free)
#' MAX SDT model to a more constrained (nested) model. Both models must be fitted
#' to the same data.
#'
#' @param free A \code{"max_sdt_fit"} object: the less constrained model.
#' @param constrained A \code{"max_sdt_fit"} object: the more constrained model.
#'   Must have fewer free parameters than \code{free}.
#'
#' @return An object of class \code{"max_sdt_compare"} with the test result.
#'
#' @details
#' The chi-squared difference statistic is:
#' \deqn{\Delta\chi^2 = \chi^2_{\text{constrained}} - \chi^2_{\text{free}}}
#' with degrees of freedom equal to the difference in the number of free parameters.
#' A significant result indicates that the constraint worsens fit.
#' The fits must use identical counts and lineup sizes, must have converged, and
#' the constrained model must impose every constraint in the free model plus at
#' least one additional constraint.
#'
#' @seealso \code{\link{fit_max_sdt}}
#' @examples
#' \donttest{
#' # Each two-condition fit takes a few seconds
#' fit_free <- fit_max_sdt(
#'   n_hit=69, n_tp_choose=82, n_fa=64, N_tp=96, N_ta=106,
#'   n_hit_2=68, n_tp_choose_2=78, n_fa_2=42, N_tp_2=96, N_ta_2=96
#' )
#' fit_eqd <- fit_max_sdt(
#'   n_hit=69, n_tp_choose=82, n_fa=64, N_tp=96, N_ta=106,
#'   n_hit_2=68, n_tp_choose_2=78, n_fa_2=42, N_tp_2=96, N_ta_2=96,
#'   constrain_d = TRUE
#' )
#' compare_max_sdt(fit_free, fit_eqd)
#' }
#' @export
compare_max_sdt <- function(free, constrained) {
  if (!inherits(free, "max_sdt_fit") || !inherits(constrained, "max_sdt_fit"))
    stop("Both arguments must be 'max_sdt_fit' objects.", call. = FALSE)
  if (!identical(free$n, constrained$n) ||
      !identical(free$n_conditions, constrained$n_conditions) ||
      !isTRUE(all.equal(free$observed, constrained$observed,
                        check.attributes = TRUE))) {
    stop("Models must be fitted to identical counts and lineup sizes.",
         call. = FALSE)
  }
  if (free$convergence != 0L || constrained$convergence != 0L) {
    stop("Both models must have converged before they can be compared ",
         "(optim convergence codes: free ", free$convergence,
         ", constrained ", constrained$convergence, ").", call. = FALSE)
  }
  if (free$n_params <= constrained$n_params)
    stop("'free' must have more free parameters than 'constrained'.", call. = FALSE)
  constraints_free <- c(d = isTRUE(free$constrain_d),
                        c = isTRUE(free$constrain_c))
  constraints_constrained <- c(d = isTRUE(constrained$constrain_d),
                               c = isTRUE(constrained$constrain_c))
  if (any(constraints_free & !constraints_constrained)) {
    stop("'constrained' is not nested within 'free'.", call. = FALSE)
  }

  delta_chisq <- constrained$chisq - free$chisq
  test_df     <- abs(free$n_params - constrained$n_params)
  if (delta_chisq < -1e-5) {
    stop("The constrained fit has a materially smaller chi-squared value; ",
         "check optimizer convergence.", call. = FALSE)
  }
  delta_chisq <- max(delta_chisq, 0)
  p_val       <- pchisq(delta_chisq, df = test_df, lower.tail = FALSE)

  structure(
    list(
      delta_chisq    = delta_chisq,
      test_df        = test_df,
      p_value        = p_val,
      chisq_free     = free$chisq,
      chisq_const    = constrained$chisq,
      df_free        = free$n_params,
      df_const       = constrained$n_params,
      constrain_d    = constrained$constrain_d,
      constrain_c    = constrained$constrain_c
    ),
    class = "max_sdt_compare"
  )
}

#' @export
print.max_sdt_compare <- function(x, digits = 4, ...) {
  cat("MAX SDT Model Comparison (chi-squared difference test)\n")
  cat(sprintf("  Free model:       chi-sq = %.3f (%d free params)\n",
              x$chisq_free, x$df_free))
  cat(sprintf("  Constrained model: chi-sq = %.3f (%d free params)\n",
              x$chisq_const, x$df_const))
  constraints <- c(
    if (x$constrain_d) "d' equal" else NULL,
    if (x$constrain_c) "lambda equal" else NULL
  )
  if (length(constraints))
    cat(sprintf("  Constraint: %s\n", paste(constraints, collapse = ", ")))
  cat(sprintf("  Delta chi-sq(%.0f) = %.3f, p = %.4f\n",
              x$test_df, x$delta_chisq, x$p_value))
  cat(sprintf("  Interpretation: constraint %s fit.\n",
              if (x$p_value < 0.05) "significantly worsens" else "does not significantly worsen"))
  invisible(x)
}

utils::globalVariables(c("cell", "count", "type"))
