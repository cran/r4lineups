#'Confidence Intervals for Proportion
#'
#'Function to compute ci high for each foil in a lineup
#'@param linetabprops A dataframe of bootstrapped lineup proportions
#'@param sumlineup Number of members in a lineup
#'@references Malpass, R. S. (1981). Effective size and defendant bias in
#'            eyewitness identification lineups. \emph{Law and Human Behavior, 5}(4), 299-309.
#'@export

allfoil_cihigh <- function(linetabprops, sumlineup){
    z <- 1:length(linetabprops)
    for (i in 1:length(linetabprops)){
        z[i] <-  boot0975(linetabprops[i],sumlineup)
    }
    z
}

allfoil_cilow <- function(linetabprops, sumlineup){
    z <- 1:length(linetabprops)
    for (i in 1:length(linetabprops)){
        z[i] <-  boot025(linetabprops[i],sumlineup)
    }
    z
}
