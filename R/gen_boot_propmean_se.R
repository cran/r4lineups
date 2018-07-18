#'Descriptive statistics for bootstrapped lineup proportion
#'
#'Function for computing mean. med and se of boot proportion
#'@param lineuprops A dataframe of bootstrapped lineup proportions
#'@return Mean, median, standard deviation, standard error & 95 CIs of
#'        lineup proportion across a bootstrapped dataframe
#'
#'@examples
#'#Data:
#'lineup_vec <- round(runif(100, 1, 6))
#'target_pos <- 3
#'
#'#Bootstrap data:
#'lineup_boot_df <- gen_boot_samples(lineup_vec, 1000)
#'
#'#Compute proportion for bootstrap samples:
#'lineuprops <- gen_lineup_prop(lineup_boot_df, target_pos = 3, k = 6)
#'
#'#Call:
#'gen_boot_propmean_se(lineuprops)
#'
#'#OR:
#'
#'lineuprops <- boot::boot(lineup_vec, lineup_prop_boot, target_pos = 3, R = 1000)
#'gen_boot_propmean_se(lineuprops$t)

#'@importFrom stats median sd
#'@export


gen_boot_propmean_se <- function (lineuprops){
    mean_boot_prop = round(mean(lineuprops, na.rm = T), 3)
    median_boot_prop = round(median(lineuprops, na.rm = T),3)
    stdev_boot_prop = round(sd(lineuprops, na.rm = T),3)
    n = length(lineuprops)
    std_error_boot_prop    = round(stdev_boot_prop/sqrt(n), 3)
    ci025 = gen_boot_propci(lineuprops,.025)
    ci975 = gen_boot_propci(lineuprops,.975)
    cat("Boot prop. (mean)   = ", mean_boot_prop,"\n")
    cat("Boot prop. (median) = ", median_boot_prop, "\n")
    cat("SD of boot prop     = ", stdev_boot_prop, "\n")
    cat("SE of boot prop     = ", stdev_boot_prop/sqrt(n), "\n")
    cat("2.5% boot CI lvl    = ", ci025, "\n")
    cat("97.5% boot CI lvl   = ", ci975, "\n")
}
