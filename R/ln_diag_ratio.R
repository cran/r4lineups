#' Ln of Diagnosticity Ratio
#'
#' Computes ln of diagnosticity ratio: ln(d)
#' @param linedf A dataframe of parameters for computing diagnosticity ratio
#' @param correction Logical. Apply Tredoux's small-cell correction (default
#'   \code{TRUE}). Set to \code{FALSE} for the uncorrected log relative risk
#'   used by the homogeneity procedure, which requires positive suspect-ID counts.
#' @details The correction adds 0.5 to each suspect-identification count and
#'   to its corresponding sample total before computing the ratio:
#'   \deqn{d = \frac{(n_{11}+0.5)/(n_{11}+n_{21}+0.5)}{(n_{12}+0.5)/(n_{12}+n_{22}+0.5)}}
#'   \eqn{\ln(d)} is then returned.  The correction avoids undefined log ratios when a
#'   cell count is zero and stabilises variance estimates in small samples.
#'
#'   \strong{To get linedf, use the diag_param helper function}
#'
#'   \emph{diag_param} returns a dataframe containing the following:
#'
#'   \itemize{
#'   \item \emph{n11}: Number of mock witnesses who identified the suspect in the target
#'        present condition
#'
#'   \item \emph{n21}: Number of mock witnesses who did not identify the suspect in the
#'       target present condition
#'
#'   \item \emph{n12}: Number of mock witnesses who identified the suspect in the target
#'       absent condition
#'
#'   \item \emph{n22}: Number of mock witnesses who did not identify the suspect in the
#'       target absent condition
#'       }
#'@references Malpass, R. S. (1981). Effective size and defendant bias in
#'            eyewitness identification lineups. \emph{Law and Human Behavior, 5}(4), 299-309.
#'
#'            Malpass, R. S., Tredoux, C., & McQuiston-Surrett, D. (2007). Lineup
#'            construction and lineup fairness. In R. Lindsay, D. F. Ross, J. D. Read,
#'            & M. P. Toglia (Eds.), \emph{Handbook of Eyewitness Psychology, Vol. 2: Memory for
#'            people} (pp. 155-178). Mahwah, NJ: Lawrence Erlbaum Associates.
#'
#'            Tredoux, C. G. (1998). Statistical inference on measures of lineup fairness.
#'           \emph{Law and Human Behavior, 22}(2), 217-237.
#'
#'            Tredoux, C. (1999). Statistical considerations when determining measures of
#'            lineup size and lineup bias. \emph{Applied Cognitive Psychology, 13}, S9-S26.
#'
#'            Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'            empirically assessing the fairness of a lineup. \emph{Law and Human Behavior,
#'            3}(4), 285-293.

#'@examples
#'#Target present data:
#'A <- rep(1:6, length.out = 100)
#'B <- rep(1:5, length.out = 70)
#'C <- rep(1:4, length.out = 20)
#'lineup_pres_list <- list(A, B, C)
#'rm(A, B, C)
#'
#'
#'#Target absent data:
#'A <- rep(6:1, length.out = 100)
#'B <- rep(5:1, length.out = 70)
#'C <- rep(4:1, length.out = 20)
#'lineup_abs_list <- list(A, B, C)
#'rm(A, B, C)
#'
#'# Suspect position for each TP/TA pair
#'pos_list <- c(3, 2, 1)
#'
#'#Nominal size:
#'k <- c(6, 5, 4)
#'
#'#Use diag param helper function to get data (n11, n21, n12, n22):
#'linedf <- diag_param(lineup_pres_list, lineup_abs_list, pos_list, k)
#'
#'#Call:
#'lnd <- ln_diag_ratio(linedf)
#'
#'@export
#'

ln_diag_ratio <- function(linedf, correction = TRUE){
    if (!is.logical(correction) || length(correction) != 1L || is.na(correction)) {
      stop("correction must be TRUE or FALSE.", call. = FALSE)
    }
    add <- if (correction) 0.5 else 0
    if (!correction && any(linedf$n11 <= 0 | linedf$n12 <= 0)) {
      stop("Uncorrected log diagnosticity requires positive suspect-ID counts.",
           call. = FALSE)
    }
    d   <- ((linedf$n11 + add)/(linedf$n11 + linedf$n21 + add)) /
           ((linedf$n12 + add)/(linedf$n12 + linedf$n22 + add))
    lnd <- log(d)
    lnd <- as.data.frame(lnd)
    return(lnd)
}
