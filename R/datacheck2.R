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
#'@examples
#' # Target-present and target-absent lineups of 6 members each
#' set.seed(1)
#' lineup_pres <- sample(1:6, 50, replace = TRUE)
#' lineup_abs <- sample(1:6, 50, replace = TRUE)
#' datacheck2(lineup_pres, lineup_abs, 6, 6)
#'@export

datacheck2 <- function(lineup_pres, lineup_abs, k1, k2){
  datacheck1(lineup_pres, k1)
  datacheck1(lineup_abs, k2)
  invisible(TRUE)
}
