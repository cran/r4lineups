#' I Component of Effective Size (Tredoux, 1998)
#'
#' Computes the Agresti-Agresti index of diversity I, the intermediate quantity
#' used to construct effective size E' = 1/(1 - I) (Tredoux, 1998).
#'
#' @param lineup_tab A table of lineup choices.
#' @details
#'   Tredoux (1998) used Agresti and Agresti's (1978) index of diversity:
#'   \deqn{I = 1 - \frac{\sum_i o_i^2}{N^2},}
#'   where \eqn{o_i} is the observed count for lineup member \eqn{i} and
#'   \eqn{N} is the total number of choices. This equals \eqn{1 - \sum_i p_i^2},
#'   where \eqn{p_i = o_i/N}. The effective size is the reciprocal transform
#'   \eqn{E' = 1/(1-I)}, computed by \code{\link{esize_T}}.
#' @return A scalar: the diversity index I.
#' @references
#'   Agresti, A., & Agresti, B. F. (1978). Statistical analysis of qualitative
#'   variation. \emph{Sociological Methodology, 9}, 204-237.
#'
#'   Tredoux, C. G. (1998). Statistical inference on measures of lineup fairness.
#'   \emph{Law and Human Behavior, 22}(2), 217-237.
#' @seealso \code{\link{esize_T}}
#' @examples
#' lineup_vec <- round(runif(100, 1, 6))
#' lineup_tab <- table(lineup_vec)
#' i <- i_esize_T(lineup_tab)
#' @export

i_esize_T <- function(lineup_tab){
  1 - 1 / esize_T(lineup_tab)
}
