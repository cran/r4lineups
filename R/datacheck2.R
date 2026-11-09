#'Helper function
#'
#'Checks that number of lineup choices contained in a vector is accurate
#'
#'@param lineup_pres A numeric vector of lineup choices for a lineup in which
#'                   the target was present
#'@param lineup_abs A numeric vector of lineup choices for a lineup in which
#'                   the target was absent
#'@param k1 Number of members (nominal size) of the TP lineup
#'@param k2 Number of members (nominal size) of the TA lineup
#'@details This function applies \code{datacheck1()} to each lineup: every choice
#'         must be an integer position between 1 and the lineup's \code{k}. It
#'         cannot detect a lineup member that no mock witness chose, because such
#'         a member does not appear in the choices at all, so \code{k1} and
#'         \code{k2} must be supplied correctly by the user.
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
