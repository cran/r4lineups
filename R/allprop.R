#'Lineup proportion for all lineup members
#'
#'Computes lineup proportion for each member in a lineup
#'@param lineup_vec A numeric vector of lineup choices
#'@param k Number of members in lineup. Must be specified by user (scalar).
#'@return Returns a vector containing lineup proportion for each lineup member
#'@examples
#'#Data:
#'lineup_vec <- round(runif(100, 1, 6))
#'
#'#Call:
#'x <- allprop(lineup_vec, k = 6)
#'@references Wells, G. L., Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'             assessing the fairness of a lineup. \emph{Law and Human Behavior, 3}(4),
#'             285-293.
#'@export

allprop <- function(lineup_vec, k){
  lineup_vec <- typecheck(lineup_vec)
  datacheck1(lineup_vec, k)
  target_pos <- c(1:k)
    propvec <- as.data.frame(matrix(ncol= 1,
                                    nrow = length(target_pos)))
    for (i in 1:length(target_pos)){
        propvec[i,]=lineup_prop_vec(lineup_vec, target_pos[i], k)
        names(propvec) <- c("prop")
        propvec <- round(propvec, 3)
    }
    return(propvec)
}
