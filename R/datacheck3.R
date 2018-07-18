#'Helper function
#'
#'Checks that number of lineup choices contained in a data table is accurate
#'
#'@param lineup_table A table of lineup choices
#'@param k Nominal size (i.e., total number of members in lineup)
#'@details This function ensures that a non-selected lineup member is not accidentally
#'         omitted from the data due to lack of selection by all mock witnesses.
#'         It functions as a check that the total number of lineup members is accurate.
#'@export


datacheck3 <- function(lineup_table, k){
  if (length(lineup_table)== k){
    lineup_table = lineup_table
  }
  else{
    stop("User-declared nominal size does not match observed nominal size. Please
         check vector of target positions.")
  }
}
