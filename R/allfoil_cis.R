#'Confidence Intervals for Proportion
#'
#'Function to compute ci high for each foil in a lineup
#'@param linetabprops A dataframe of bootstrapped lineup proportions
#'@param sumlineup Number of mock-witness observations used to estimate each proportion.
#'@references Malpass, R. S. (1981). Effective size and defendant bias in
#'            eyewitness identification lineups. \emph{Law and Human Behavior, 5}(4), 299-309.
#'@examples
#' # Upper CI bounds for the choice proportions of three lineup members,
#' # based on 20 mock witnesses
#' allfoil_cihigh(c(0.5, 0.3, 0.2), 20)
#'@export

allfoil_cihigh <- function(linetabprops, sumlineup){
    z <- seq_along(linetabprops)
    for (i in seq_along(linetabprops)){
        z[i] <-  boot0975(linetabprops[i],sumlineup)
    }
    z
}

allfoil_cilow <- function(linetabprops, sumlineup){
    z <- seq_along(linetabprops)
    for (i in seq_along(linetabprops)){
        z[i] <-  boot025(linetabprops[i],sumlineup)
    }
    z
}
