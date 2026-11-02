#'Bootstrap Quantile for Effective Size
#'
#'Computes one requested quantile from a vector of bootstrapped Malpass effective
#'sizes. Call the function twice (for example, at 0.025 and 0.975) to obtain both
#'endpoints of a percentile interval.
#'@param lineupsizes A non-empty numeric vector of bootstrapped effective sizes.
#'@param perc A single quantile probability between 0 and 1. Defaults to 0.05.
#'@return A single named bootstrap quantile.
#'@examples
#'#Data:
#'lineup_vec <- rep(1:6, length.out = 100)
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

gen_esize_m_ci <- function(lineupsizes, perc = .05) {
    if (!is.numeric(lineupsizes) || length(lineupsizes) == 0L ||
        any(!is.finite(lineupsizes))) {
        stop("lineupsizes must be a non-empty finite numeric vector")
    }
    if (!is.numeric(perc) || length(perc) != 1L || !is.finite(perc) ||
        perc < 0 || perc > 1) {
        stop("perc must be a single finite number between 0 and 1")
    }
    stats::quantile(lineupsizes, probs = perc)
}
