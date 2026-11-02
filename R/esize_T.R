#' Effective Size (Tredoux, 1998)
#'
#' Function for generating Tredoux's transformed index-of-diversity
#' effective size.
#'
#' @param lineup_tab A table of lineup choices
#' @param unbiased Logical. If \code{FALSE} (the default) the plug-in estimator
#'   is returned, as in Tredoux (1998). If \code{TRUE}, the small-sample bias
#'   correction described below is applied. The default is unchanged so that
#'   existing analyses and published values are unaffected.
#' @details Tredoux (1998) proposed using Agresti and Agresti's (1978)
#'          index of diversity for lineup choices,
#'          \deqn{I = 1 - \frac{\sum_i o_i^2}{N^2},}
#'          where \eqn{o_i} is the observed count for lineup member \eqn{i}
#'          and \eqn{N} is the total number of choices. The index is then
#'          transformed back to an effective-size scale:
#'          \deqn{E' = \frac{1}{1-I} = \frac{N^2}{\sum_i o_i^2}
#'          = \frac{1}{\sum_i p_i^2}.}
#'          This formula is algebraically equivalent to a reciprocal
#'          concentration form.
#'
#' @section Small-sample bias:
#' The expression above is a \emph{plug-in} estimator: sample proportions are
#' substituted for the unknown \eqn{p_i}. The plug-in concentration
#' \eqn{\sum_i \hat p_i^2} is biased upward. Its reciprocal \eqn{E'} is biased
#' \strong{downward} for fair and near-fair lineups, but the direction is not
#' guaranteed: when one member draws most choices the bias can be upward (two
#' members, \eqn{p = (.9, .1)}, \eqn{N = 10}: mean 1.230 against a true 1.220,
#' by exact enumeration). For \eqn{N} choices spread evenly over \eqn{k}
#' members the bias is approximately
#' \deqn{E[\hat{E}'] \approx \frac{Nk}{N + k - 1}, \qquad
#'       \mathrm{bias} = -\frac{k(k-1)}{N + k - 1}.}
#' A perfectly fair six-person lineup assessed with 50 choices therefore returns
#' about 5.47 rather than 6, and with 20 choices about 4.87. The bias is
#' depends on both nominal size and the choice distribution; no single sample
#' size makes it negligible for every lineup.
#'
#' With \code{unbiased = TRUE} the function substitutes the unbiased estimator
#' of the concentration, \eqn{\sum_i o_i(o_i - 1) / (N(N-1))}, and returns its
#' reciprocal,
#' \deqn{\hat{E}'_{\mathrm{corr}} = \frac{N(N-1)}{\sum_i o_i(o_i - 1)}.}
#' The reciprocal of an unbiased estimate is not itself unbiased. For
#' near-fair lineups this can reduce bias, with increased variability; in concentrated
#' distributions it can overshoot (mean 1.269 in the two-member example).
#' When fewer than two choices are available the correction is undefined and
#' returns \code{NA} with a warning. When every choice is on a different member,
#' the corrected reciprocal is \code{Inf}, retained with a warning. If
#' \eqn{N \le k}, that event can have positive probability, so the corrected
#' estimator can have an infinite sampling mean. It is not uniformly superior.
#'
#' Which to use depends on what is being reported. For a description of the
#' choice distribution actually observed, the plug-in value is the quantity of
#' interest. For inference about the lineup that produced those choices, the
#' framing of Tredoux (1998), the corrected form usually has less bias when
#' choices are spread across members. Neither is uniformly better.
#' Note that the other functions in this family (\code{\link{esize_T_boot}},
#' \code{\link{esize_T_ci_n}}, \code{\link{i_esize_T}}) use the plug-in form.
#' @references Malpass, R. S. (1981). Effective size and defendant bias in eyewitness
#'                  identification lineups. \emph{Law and Human Behavior, 5}(4), 299-309.
#'
#'             Agresti, A., & Agresti, B. F. (1978). Statistical analysis of
#'                  qualitative variation. \emph{Sociological Methodology, 9}, 204-237.
#'
#'             Malpass, R. S., Tredoux, C., & McQuiston-Surrett, D. (2007).
#'                  Lineup construction and lineup fairness. In R. Lindsay, D. F. Ross, J. D. Read, & M. P. Toglia (Eds.), Handbook of Eyewitness Psychology, Vol. 2: Memory for people (pp. 155-178). Mahwah, NJ: Lawrence Erlbaum Associates.
#'
#'             Tredoux, C. G. (1998). Statistical inference on measures of lineup
#'                  fairness. \emph{Law and Human Behavior, 22(2)}, 217-237.
#'
#'             Tredoux, C. (1999). Statistical considerations when determining measures
#'                  of lineup size and lineup bias. \emph{Applied Cognitive Psychology, 13}, S9-S26.
#'
#'             Wells, G. L., Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'                  empirically assessing the fairness of a lineup. \emph{Law and Human Behavior, 3}(4), 285-293.
#'@examples
#'#Data:
#'lineup_vec <- round(runif(100, 1, 6))
#'lineup_tab <- table(lineup_vec)
#'
#'#Call:
#'e <- esize_T(lineup_tab)
#'
#'@export

esize_T <- function(lineup_tab, unbiased = FALSE){
  counts <- as.numeric(lineup_tab)
  if (length(counts) == 0L || anyNA(counts) || any(!is.finite(counts)) ||
      any(counts < 0) || sum(counts) <= 0) {
    stop("lineup_tab must contain finite non-negative counts with a positive total.",
         call. = FALSE)
  }
  if (!is.logical(unbiased) || length(unbiased) != 1L || is.na(unbiased)) {
    stop("unbiased must be TRUE or FALSE.", call. = FALSE)
  }

  if (!unbiased) {
    return(sum(counts)^2 / sum(counts^2))
  }

  # Unbiased estimator of sum(p^2): the probability that two draws without
  # replacement fall on the same member. Undefined with fewer than two choices,
  # and zero when every choice fell on a different member -- in which case the
  # reciprocal is infinite. Do not substitute a different estimator.
  if (any(counts != floor(counts))) {
    stop("The small-sample correction requires whole observation counts.", call. = FALSE)
  }
  n <- sum(counts)
  if (n < 2) {
    warning("Fewer than two choices; the bias correction is undefined; returning NA.", call. = FALSE)
    return(NA_real_)
  }
  concentration <- sum(counts * (counts - 1))
  if (concentration <= 0) {
    warning("Every choice fell on a different lineup member, so the corrected ",
            "estimate is unbounded; returning Inf.",
            call. = FALSE)
    return(Inf)
  }
  n * (n - 1) / concentration
}
