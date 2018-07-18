#'Homogeneity of diagnosticity ratio with bootstrapped CIs
#'
#'Function for computing bootstrapped estimates of homogeneity of diagnosticity ratio
#'
#'@param lineup_pres_list A list containing k vectors of lineup choices for k lineups, in which the
#'                        target was present
#'@param lineup_abs_list A list containing k vectors of lineup choices for k lineups, in which the
#'                       target was absent
#'@param k Number of members in lineup. Must be specified by user (scalar).
#'@param R Number of bootstrap replications. Defaults to R = 100.
#'@details Computes bootstrapped diagnosticity ratio with chi-squared estimate,
#'         significance level and confidence intervals for k lineup pairs
#'@references Malpass, R. S. (1981). Effective size and defendant bias in
#'            eyewitness identification lineups. \emph{Law and Human Behavior, 5}(4), 299-309.
#'
#'            Malpass, R. S., Tredoux, C., & McQuiston-Surrett, D. (2007). Lineup
#'            construction and lineup fairness. In R. Lindsay, D. F. Ross, J. D. Read,
#'            & M. P. Toglia (Eds.), \emph{Handbook of Eyewitness Psychology, Vol. 2: Memory for
#'            people} (pp. 155-178). Mahwah, NJ: Lawrence Erlbaum Associates.
#'
#'            Tredoux, C. G. (1998). Statistical inference on measures of lineup fairness.
#'            \emph{Law and Human Behavior, 22}(2), 217-237.
#'
#'            Tredoux, C. (1999). Statistical considerations when determining measures of
#'            lineup size and lineup bias. \emph{Applied Cognitive Psychology}, 13, S9-S26.
#'
#'            Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'            empirically assessing the fairness of a lineup. \emph{Law and Human Behavior,
#'            3}(4), 285-293.
#'@examples
#'#Target present data:
#'A <-  round(runif(100,1,6))
#'B <-  round(runif(70,1,5))
#'C <-  round(runif(20,1,4))
#'lineup_pres_list <- list(A, B, C)
#'rm(A, B, C)
#'
#'#Target absent data:
#'A <-  round(runif(100,1,6))
#'B <-  round(runif(70,1,5))
#'C <-  round(runif(20,1,4))
#'lineup_abs_list <- list(A, B, C)
#'rm(A, B, C)
#'
#'@export
#'@importFrom boot boot boot.ci
#'@importFrom stats setNames

homog_diag_boot <- function(lineup_pres_list, lineup_abs_list, k, R=100){

  datacheck1(lineup_pres_list[[1]], k)

  bootdata1 <- gen_boot_samples_list(lineup_pres_list, R)
  bootdata2 <- gen_boot_samples_list(lineup_abs_list, R)
  pres_boot.dat <- suppressWarnings(lapply(bootdata1, diag_param_boot))
  abs_boot.dat <- suppressWarnings(lapply(bootdata2, diag_param_boot))
  bootlist <- mapply(cbind,pres_boot.dat, abs_boot.dat)

  listdf <- as.data.frame(bootlist)
  names<-  c("n11", "n21", "n12", "n22")
  linedf <- lapply(listdf, data.frame, stringsAsFactors = FALSE)
  linedf <- lapply(listdf, setNames, nm = names)

  par1 <- lapply(linedf, var_lnd) %>% unlist
  par2 <- lapply(linedf, ln_diag_ratio) %>% unlist
  par3 <- lapply(linedf, d_weights) %>% unlist
  par4 <- as.data.frame(rbind(par1, par2, par3))
  rownames(par4) <- c("var", "lnd", "wi")

  bootmean <- boot(par4, d_bar.boot, R)
  ci.mean <- boot.ci(bootmean, type = "bca")
  bootchi <- boot(par4, chi_diag.boot, R)
  ci.chi <- boot.ci(bootchi, type = "bca")

  cat ("Mean diagnosticity ratio is", round(bootmean$t0, 3))
  cat ("\n")
  cat ("Confidence intervals (bias-corrected)", round(ci.mean$bca[4:5], 3))
  cat ("\n")
  cat ("Chi-squared estimate is", round(bootchi$t0, 3))
  cat ("\n")
  cat ("Confidence interavals (bias-corrected):", round(ci.chi$bca[4:5], 3))

}
