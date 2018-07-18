#'Effective Size
#'
#'Function for computing Effective Size
#' @param lineup_table A table of lineup choices
#' @param both Defaults to FALSE. Returns Tredoux's adjusted effective size estimate.
#'
#'                 If TRUE, provides both Malpass's (1981) and Makpass's adjusted (see: Tredoux, 1998)
#'                 calculations of effective size.
#'@param k Number of members in lineup. Must be specified by user (scalar).
#' @details Reduces the size of a lineup from a (corrected) nominal starting
#'          value by the degree to which members are, in sum, chosen below
#'          the level of chance expectation.
#' @references Malpass, R. S. (1981). Effective size and defendant bias in
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
#'@return Malpass's original & adjusted estimates of effective size
#'@examples
#'#Data:
#'lineup_vec <- round(runif(100, 1, 6))
#'
#'#Call:
#'esize_m(lineup_vec, 6, both = TRUE)
#'esize_m(lineup_vec, 6)
#'
#'@export

esize_m <- function (lineup_table, k, both =FALSE){
  #Revised formulation (Tredoux, 1998)
  ea <- sum(lineup_table)/k
  x <- sum(abs(lineup_table-ea)/(2*ea))
  esize_ma = k-x


  #Original formulation (Malpass, 1981)
  if (0 %in% names(lineup_table)== TRUE ){
    ka <- k-1
    lineup_table_a <- lineup_table[-1]
  }
  else{
    ka <- k
    lineup_table_a <- lineup_table
  }

  ea <- (sum(lineup_table))/ka
  xa <- sum(abs(lineup_table_a-ea)/(2*ea))
  esize_ma_a = ka-xa

  #Output
  if (both) {
    cat("Effective size (Malpass, 1981) = ", esize_ma_a,"\n")
    cat("Effective size (Malpass, 1981,","\n",
        "            adj Tredoux, 1998) = ", esize_ma, "\n")
  }
  esize_ma
}
