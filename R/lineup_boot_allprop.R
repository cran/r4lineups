#'Confidence intervals for lineup proportion
#'
#'Computes bootstrapped confidence intervals for lineup proportion
#'@param lineup_vec A numeric vector of lineup choices
#'@param k Number of targets in lineup. Must be specified by user (scalar).
#'@param conf Confidence level for the intervals, e.g. 0.95 for 95 percent
#'            intervals. Defaults to 0.95. May be specified by user (scalar).
#'@param R Number of bootstrap replications. Defaults to 1000.
#'@return A dataframe of \code{k} rows and two columns, \code{ci_low} and
#'        \code{ci_high}, holding the bias-corrected and accelerated (BCa)
#'        confidence interval for the proportion choosing each lineup member.
#'        Rows are named by lineup position.
#'@seealso \code{\link[boot:boot]{boot}}: https://cran.r-project.org/web/packages/boot/boot.pdf
#'@details Computes a bootstrapped lineup proportion for each member in turn by
#'         calling the \code{boot} function in the \code{boot} package. Where a
#'         BCa interval cannot be computed, the percentile interval is used
#'         instead.
#'
#'         Note that the \code{k} intervals are each computed at the nominal
#'         \code{conf} level, with \strong{no} adjustment for the fact that
#'         \code{k} of them are inspected together. Interpreting all of them at
#'         once inflates the chance that at least one excludes the chance value
#'         by more than the nominal amount. If a family-wise statement is wanted,
#'         raise the level for each interval accordingly, e.g.
#'         \code{conf = 1 - 0.05 / k} for a Bonferroni-style correction.
#'@references Davison,  A.C. & Hinkley,  D.V. (1997). \emph{Bootstrap methods and their
#'            application}. Cambridge University Press.
#'
#'            Wells, G. L., Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'            assessing the fairness of a lineup. \emph{Law and Human Behavior, 3}(4),
#'            285-293.
#'@examples
#'#Data:
#'lineup_vec <- round(runif(100, 1, 6))
#'
#'#Call:
#'lineuprops_ci <- lineup_boot_allprop(lineup_vec, k= 6)
#'lineuprops_ci <- lineup_boot_allprop(lineup_vec, k= 6, conf = 0.975)
#'
#'@export
#'@importFrom boot boot boot.ci
#'@importFrom magrittr %>% extract
#'@importFrom purrr map map_df
#'@importFrom dplyr slice

lineup_boot_allprop <- function(lineup_vec, k, conf = 0.95, R = 1000){
  lineup_vec <- typecheck(lineup_vec)
  datacheck1(lineup_vec, k)
  if (!is.numeric(conf) || length(conf) != 1L || conf <= 0 || conf >= 1) {
    stop("conf must be between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(R) || length(R) != 1L || R < 1 || R != as.integer(R)) {
    stop("R must be a positive integer.", call. = FALSE)
  }

  target_pos <- seq_len(k)
  ci_values <- lapply(target_pos, function(pos) {
    boot_obj <- boot(lineup_vec, lineup_prop_boot, target_pos = pos, R = R)
    ci_obj <- suppressWarnings(tryCatch(
      boot.ci(boot_obj, conf = conf, type = "bca", target_pos = pos),
      error = function(e) NULL
    ))
    if (!is.null(ci_obj) && !is.null(ci_obj$bca) && all(is.finite(ci_obj$bca[4:5]))) {
      return(ci_obj$bca[4:5])
    }
    alpha <- (1 - conf) / 2
    as.numeric(stats::quantile(boot_obj$t, c(alpha, 1 - alpha), na.rm = TRUE))
  })
  ci <- as.data.frame(do.call(rbind, ci_values))
  names(ci) <- c("ci_low", "ci_high")
  rownames(ci) <- target_pos
  ci
}
