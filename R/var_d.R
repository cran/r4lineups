#' Asymptotic Variance of Log Diagnosticity
#'
#'Computes the large-sample variance of the log diagnosticity ratio for a lineup pair.
#'@details Returns 1/h - 1/N_TP + 1/f - 1/N_TA for positive suspect-ID
#'  counts h and f. This is on the log-ratio scale, as in Tredoux (1998),
#'  not the raw ratio scale. A delta-method variance for the raw ratio is
#'  the squared diagnosticity ratio times this value.
#'@param lineup_pres A numeric vector of lineup choices for a lineup in which
#'                   the target was present
#'@param lineup_abs A numeric vector of lineup choices for a lineup in which
#'                   the target was absent
#'@param pos_pres A scalar, representing target position in TP lineup. Must be declared by user
#'@param pos_abs A scalar, representing target position in TA lineup. Must be declared by user
#'@param k1 Number of targets in TP lineup. Must be specified by user (scalar).
#'@param k2 Number of targets in TA lineup. Must be specified by user (scalar).
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
#'lineup_pres <- round(runif(100, 1, 6))
#'lineup_abs <- round(runif(100, 1, 6))
#'pos_pres <- 3
#'pos_abs <- 4
#'
#'#Call:
#'var_d <- var_diag_ratio(lineup_pres, lineup_abs, pos_pres, pos_abs, 6, 6)
#'var_d <- var_diag_ratio(lineup_pres, lineup_abs, 3, 4, 6, 6)
#'
#'@export

var_diag_ratio <- function(lineup_pres, lineup_abs, pos_pres, pos_abs, k1, k2){

    lineup_pres <- typecheck(lineup_pres)
    lineup_abs <- typecheck(lineup_abs)
    datacheck2(lineup_pres, lineup_abs, k1, k2)
    if (length(pos_pres) != 1L || length(pos_abs) != 1L ||
        !is.numeric(pos_pres) || !is.numeric(pos_abs) ||
        !is.finite(pos_pres) || !is.finite(pos_abs) ||
        pos_pres != floor(pos_pres) || pos_abs != floor(pos_abs) ||
        pos_pres < 1 || pos_pres > k1 || pos_abs < 1 || pos_abs > k2) {
      stop("Suspect positions must each be a single position within its lineup.", call. = FALSE)
    }
    if (!any(lineup_pres == pos_pres) || !any(lineup_abs == pos_abs)) {
      return(Inf)
    }

    a <- sum(lineup_pres  != pos_pres)
    b <- sum(lineup_pres == pos_pres)*(length(lineup_pres))
    c <- sum(lineup_abs  != pos_abs)
    d <- sum(lineup_abs == pos_abs)*(length(lineup_abs))
    e <- (a/b)+(c/d)
    return(e)
}
