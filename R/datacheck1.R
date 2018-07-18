#'Helper function
#'
#'Checks that number of lineup choices contained in a vector is accurate
#'
#'@param lineup_vec A numeric vector of lineup choices
#'@param k Number of members in lineup
#'@details This function ensures that a non-selected lineup member is not accidentally
#'         omitted from the dataframe due to lack of selection by all mock witnesses.
#'         It functions as a check that the total number of lineup members is accurate.
#'@export

datacheck1 <- function(lineup_vec, k){
  if (length(table(lineup_vec))== k){
    lineup_vec = lineup_vec
  }
  else{
    stop("User-declared nominal size does not match observed nominal size. Please
         check vector of target positions.")
  }
}
