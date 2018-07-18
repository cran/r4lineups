#'Lineup proportion
#'
#'Computes the proportion of mock witnesses identifying a particular lineup member
#'@param lineup_vec A numeric vector of lineup choices
#'@param target_pos Suspect/lineup member position. Must be declared by user (scalar).
#'@param k Nominal size (i.e., total number of members in lineup). Must be specified by user (scalar).
#'@return Returns a proportion indicating the frequency with which a target was
#'        identified in a lineup
#'@references Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'            empirically assessing the fairness of a lineup. \emph{Law and Human Behavior,
#'            3}(4), 285-293.
#'@examples
#'#Data:
#'lineup_vec <- round(runif(100, 1, 6))
#'
#'#Call:
#'lineup_prop_vec(lineup_vec, 3, 6)
#'
#'@export

lineup_prop_vec <- function(lineup_vec, target_pos, k){
  lineup_vec <- typecheck(lineup_vec)
  datacheck1(lineup_vec, k)
    sum(lineup_vec == target_pos)/length(lineup_vec)
}


