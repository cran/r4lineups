#'Bootstrapped Confidence Intervals for Effective Size
#'
#'Function for computing Effective Size (Malpass, 1981, as adjusted by Tredoux, 1998)
#'with CIs from bootstrap df of lineups
#'@param lineupsizes A vector of bootstrapped effective sizes
#'@param perc Defaults to .05. Can be specified by user, according to desired
#'            level of alpha (scalar)
#'@return Confidence intervals for effective size
#'@examples
#'#Data:
#'lineup_vec <- round(runif(100, 1, 6))
#'k <- 6
#'
#'#Use gen_boot_samples to get bootstrapped data:
#'bootdata <- gen_boot_samples(lineup_vec, 1000)
#'
#'#Compute effective size over df of bootstrapped data:
#'lineupsizes <- gen_esize_m(bootdata, 6)
#'
#'#Call:
#'gen_esize_m_ci(lineupsizes)
#'gen_esize_m_ci(lineupsizes, perc = .025)
#'gen_esize_m_ci(lineupsizes, perc = .975)
#'
#'@export
#'

gen_esize_m_ci <- function (lineupsizes, perc=.05){
    if(perc >= 0 & perc <= 1) {
        quantile(lineupsizes, probs = perc)
    }
    else {
        cat("Illegal value entered (perc must be proportion)")
    }
}
