#'Effective Size (Tredoux, 1998)
#'
#' Function for generating Effective Size (Tredoux, 1998)
#' @param lineup_table A table of lineup choices
#' @details Reduces the size of a lineup from a (corrected) nominal starting
#'          value by the degree to which members are, in sum, chosen below
#'          the level of chance expectation.
#' @references Malpass, R. S. (1981). Effective size and defendant bias in eyewitness
#'                  identification lineups. \emph{Law and Human Behavior, 5}(4), 299-309.
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
#'lineup_table <- table(lineup_vec)
#'
#'#Call:
#'e <- esize_T(lineup_table)
#'
#'@export

esize_T <- function(lineup_table){
  i <- 1-(1/(sum(lineup_table)^2))*sum(lineup_table^2)
  i <- 1/(1-i)
  return(i)
}
