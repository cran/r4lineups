#'Confidence intervals for lineup proportion
#'
#'Computes bootstrapped confidence intervals for lineup proportion
#'@param lineup_vec A numeric vector of lineup choices
#'@param k Number of targets in lineup. Must be specified by user (scalar).
#'@param conf Desired level of alpha. Defaults to 0.95. May be specified by user (scalar).
#'@return Returns a vector of bias corrected confidence intervals for
#'        lineup proportion for each member in a lineup
#'@seealso \code{\link[boot:boot]{boot}}: https://cran.r-project.org/web/packages/boot/boot.pdf
#'@details Function that computes bootstrapped lineup proportion using 1000 bootstrap draws
#'         Calls 'boot function in 'boot' package
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

lineup_boot_allprop <- function(lineup_vec, k, conf = 0.95){
  lineup_vec <- typecheck(lineup_vec)
  datacheck1(lineup_vec, k)
  target_pos <- c(1:k)
  z <- map(target_pos,~boot(lineup_vec, lineup_prop_boot, target_pos = .x, R = 1000) %>%
             boot.ci(conf = 0.95, type = "bca")) %>%
    map(magrittr::extract, "bca") %>%
    map_df(magrittr::extract,"bca")

  z2 <-  matrix(ncol = length(target_pos),nrow = 5, z$bca) %>%
    data.frame() %>%
    slice(4:5)
  ci <- as.data.frame(t(z2))

  member <- seq(from = 1, to = length(target_pos), by = 1)
  rownames(ci) <- member
  colnames(ci) <- c("ci_low", "ci_high")
  ci <- round(ci, 3)

  return(ci)
}
