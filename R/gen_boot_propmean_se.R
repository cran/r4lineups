#'Descriptive statistics for bootstrapped lineup proportion
#'
#'Function for computing the mean, median, and bootstrap standard error. The
#'standard deviation of the bootstrap replicates is the estimated standard error;
#'it is not divided by the square root of the number of replicates.
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
    lineuprops <- as.numeric(lineuprops)
    if (any(!is.finite(lineuprops))) {
      stop("Bootstrap estimates must all be finite; failed draws must be reported explicitly.", call. = FALSE)
    }
    valid <- lineuprops
    if (length(valid) < 2L) {
      stop("lineuprops must contain at least two finite bootstrap estimates.", call. = FALSE)
    }
    mean_boot_prop <- mean(valid)
    median_boot_prop <- median(valid)
    stdev_boot_prop <- sd(valid)
    # The bootstrap standard deviation is the estimated standard error.
    std_error_boot_prop <- stdev_boot_prop
    ci025 <- gen_boot_propci(valid, .025)
    ci975 <- gen_boot_propci(valid, .975)
    cat("Boot prop. (mean)   = ", mean_boot_prop,"\n")
    cat("Boot prop. (median) = ", median_boot_prop, "\n")
    cat("SD of boot prop     = ", stdev_boot_prop, "\n")
    cat("SE of boot prop     = ", round(std_error_boot_prop, 3), "\n")
    cat("2.5% boot CI lvl    = ", ci025, "\n")
    cat("97.5% boot CI lvl   = ", ci975, "\n")
    invisible(list(
      mean = mean_boot_prop,
      median = median_boot_prop,
      sd = stdev_boot_prop,
      se = std_error_boot_prop,
      ci = c(lower = unname(ci025), upper = unname(ci975)),
      n_bootstrap = length(valid)
    ))
}
