#'Diagnosticity Ratio (Wells & Lindsay, 1980; Wells & Turtle, 1986)
#'
#'Computes Wells' diagnosticity ratio for one lineup pair
#'
#'@param lineup_pres A numeric vector of lineup choices for a lineup in which
#'                   the target was present
#'@param lineup_abs A numeric vector of lineup choices for a lineup in which
#'                   the target was absent
#'@param pos_pres A scalar, representing target position in TP lineup. Must be declared by user
#'@param pos_abs A scalar, representing target position in TA lineup. Must be declared by user
#'@param k1 Number of targets in TP lineup. Must be specified by user (scalar).
#'@param k2 Number of targets in TA lineup. Must be specified by user (scalar).
#'@examples
#'#Data:
#'lineup_pres <- round(runif(100, 1, 6))
#'lineup_abs <- round(runif(70, 1, 5))
#'pos_pres <- 3
#'pos_abs <- 5
#'
#'#Call:
#'diag_ratio_W(lineup_pres, lineup_abs, pos_pres, pos_abs, 6, 5)
#'diag_ratio_W(lineup_pres, lineup_abs, 3, 5, 6, 5)
#'@references Wells, G. L., & Lindsay, R. C. L. (1980).On estimating the diagnosticity
#'            of eyewitness nonidentifications.\emph{Psychological Bulletin, 88}, 776-784.
#'
#'            Wells, G. L., & Turtle, J. W. (1986). Eyewitness identification:
#'            The importance of lineup models. \emph{Psychological Bulletin, 99}, 320-329.
#'@export

diag_ratio_W <- function(lineup_pres, lineup_abs, pos_pres, pos_abs, k1, k2){

  lineup_pres <- typecheck(lineup_pres)
  lineup_abs <- typecheck(lineup_abs)

  datacheck2(lineup_pres, lineup_abs, k1, k2)

    a <- sum(lineup_pres == pos_pres)/(length(lineup_pres))
    b <- sum(lineup_abs == pos_abs)/(length(lineup_abs))
    c <- a/b
    return(c)
}
