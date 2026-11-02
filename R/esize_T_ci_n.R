#' Effective Size with Confidence Intervals from Normal Theory (Tredoux, 1998)
#'
#' Computes effective size E' (Tredoux, 1998) together with a confidence interval
#' derived from the normal-theory variance formula in Tredoux (1998).
#'
#' @param lineup_tab A table of lineup choices.
#' @param alpha Confidence level as a decimal (e.g., 0.95 for a 95-percent CI).
#' @details
#'   The variance of the diversity index I is estimated via Tredoux's (1998)
#'   closed-form expression:
#'   \deqn{\mathrm{Var}(I) \approx \frac{4}{N}\left(\sum_i p_i^3 - \left(\sum_i p_i^2\right)^2\right).}
#'   Normal quantiles are applied to I and the resulting bounds are back-transformed
#'   to the E' scale via \eqn{E' = 1/(1-I)}. For small samples or skewed distributions,
#'   all interval methods require qualification. In particular, plug-in bootstrap
#'   intervals also fail at exact uniform fairness; see \code{\link{esize_boot_dist}}.
#'   Bayesian credible intervals describe a stated posterior rather than
#'   guaranteeing fixed-parameter frequentist coverage.
#'
#'   Note that this concerns the \emph{interval}. The point estimate itself is
#'   also biased downward near uniform choice distributions at small samples,
#'   although the direction is not universal, since \eqn{E'} is a plug-in
#'   estimator; see the \dQuote{Small-sample bias} section of
#'   \code{\link{esize_T}}. This function uses the plug-in form.
#'   This first-order interval is unreliable in small samples and at uniform or
#'   concentrated boundary distributions. A zero estimated variance yields a
#'   degenerate interval, not evidence of zero population uncertainty. An ordinary
#'   bootstrap is also not a general solution to these boundary problems.
#'@references Malpass, R. S. (1981). Effective size and defendant bias in
#'            eyewitness identification lineups. \emph{Law and Human Behavior, 5}(4), 299-309.
#'
#'            Malpass, R. S., Tredoux, C., & McQuiston-Surrett, D. (2007). Lineup
#'            construction and lineup fairness. In R. Lindsay, D. F. Ross, J. D. Read,
#'            & M. P. Toglia (Eds.), \emph{Handbook of Eyewitness Psychology, Vol. 2: Memory for
#'            people} (pp. 155-178). Mahwah, NJ: Lawrence Erlbaum Associates.
#'
#'            Tredoux, C. G. (1998). Statistical inference on measures of lineup fairness.
#'            \emph{Law and Human Behavior, 22}(2), 217-237.
#'
#'            Tredoux, C. (1999). Statistical considerations when determining measures of
#'            lineup size and lineup bias. \emph{Applied Cognitive Psychology}, 13, S9-S26.
#'
#'            Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'            empirically assessing the fairness of a lineup. \emph{Law and Human Behavior,
#'            3}(4), 285-293.
#'@examples
#'#Data:
#'lineup_vec <- round(runif(100, 1, 6))
#'lineup_tab <- table(lineup_vec)
#'
#'#Call:
#'e_ci <- esize_T_ci_n(lineup_tab, .95)
#'
#'@importFrom stats qnorm
#'@export

esize_T_ci_n <- function(lineup_tab, alpha){
    datacheck3(lineup_tab, length(lineup_tab))
    if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) ||
        alpha <= 0 || alpha >= 1) {
      stop("alpha must be a confidence level strictly between 0 and 1.", call. = FALSE)
    }
    N = sum(lineup_tab)
    a <- 4/N
    b <- sum((lineup_tab/N)^3)
    c <- (sum((lineup_tab/N)^2)^2)
    var_i <- a*(b-c)
    if (var_i <= .Machine$double.eps) warning("The first-order diversity variance is zero; the degenerate normal interval does not establish zero population uncertainty.", call. = FALSE)
    var_i <- max(var_i, 0)
    sd_i <- sqrt(var_i)
    ci_low <- i_esize_T(lineup_tab) + qnorm((1-alpha)/2)*sd_i
    ci_high <- i_esize_T(lineup_tab) + qnorm(alpha+((1-alpha)/2))*sd_i
    e_ci <- list(ci_low = 1/(1-ci_low), ci_high = 1/(1-ci_high))
    return(e_ci)
}
