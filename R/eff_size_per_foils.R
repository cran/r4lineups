#'Effective Size per Foils
#'
#'Function for computing effective size (Malpass, 1981) counting foils who fall
#'within the CI for chance guessing
#'@param lineup_vec A numeric vector of lineup choices
#'@param target_pos A numeric vector indexing all lineup members
#'@param k Number of members in lineup. Must be specified by user (scalar).
#'@param conf Desired level of alpha. Defaults to 0.95. May be specified by user (scalar).
#'@examples
#'#Data:
#'lineup_vec <- round(runif(100, 1, 6))
#'target_pos <- c(1, 2, 3, 4, 5, 6)
#'
#'#Call:
#'eff_size_per_foils(lineup_vec, target_pos, 6)
#'eff_size_per_foils(lineup_vec, target_pos, conf = 0.95, 6)
#'@references Malpass, R S. (1981). Effective size and defendant bias in eyewitness
#'            identification lineups. \emph{Law and Human Behavior, 5}, 299-309.
#'
#'            Tredoux, C. G. (1998). Statistical inference on measures of lineup
#'            fairness. \emph{Law and Human Behavior, 22}(2), 217-237.
#'
#'@export

eff_size_per_foils <- function(lineup_vec, target_pos, k, conf = 0.95){
  lineup_vec <- typecheck(lineup_vec)
  ci <- lineup_boot_allprop(lineup_vec, k, conf = 0.95)
  k0 <- 1/k
  ci_count <- cbind(ci[,1] <= k0 & ci[,2] >= k0)
  print(sum(ci_count == TRUE))
}
