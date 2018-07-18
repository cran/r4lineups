#'Helper function
#'
#'Checks that number of lineup choices contained in a vector is accurate
#'
#'@param lineup_pres A numeric vector of lineup choices for a lineup in which
#'                   the target was present
#'@param lineup_abs A numeric vector of lineup choices for a lineup in which
#'                   the target was absent
#'@param k1 Number of targets in TP lineup
#'@param k2 Number of targets in TA lineup
#'@details This function ensures that a non-selected lineup member is not accidentally
#'         omitted from the dataframe due to lack of selection by all mock witnesses.
#'         It functions as a check that the total number of lineup members is accurate
#'@export

datacheck2 <- function(lineup_pres, lineup_abs, k1, k2){
  if (length(table(lineup_pres))== k1){
    lineup_pres = lineup_pres
  }
  else{
    stop("User-declared nominal size does not match observed nominal size for TP lineup. Please
         check vector of TP target positions.")
  }

  if (length(unique(lineup_abs))== k2){
    lineup_abs = lineup_abs
  }
  else{
    stop("User-declared nominal size does not match observed nominal size for TA lineup. Please
         check vector of TA target positions.")
  }
}
