#' Master function: Homogeneity of diagnosticity ratio
#'
#' This function provides assesses the homogeneity of the diagnosticity ratio of
#'  k lineup pairs.
#'
#'@param lineup_pres_list A list containing k vectors of lineup choices for k lineups, in which the
#'                        target was present
#'@param lineup_abs_list A list containing k vectors of lineup choices for k lineups, in which the
#'                       target was absent
#'@param pos_list A numeric vector indexing lineup member positions for the target
#'                present & absent conditions.
#'@param k A vector indexing number of members in each lineup pair (nominal size). Must be specified by user (scalar).
#'@return Computes diagnosticity ratio with chi-squared estimate and significance
#'         level for k lineup pairs
#'@details Master function for assessing homogeneity of diagnosticity ratio for
#'         k independent lineups.
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
#'#Pos list
#'lineup1_pos <- c(1, 2, 3, 4, 5, 6)
#'lineup2_pos <- c(1, 2, 3, 4, 5)
#'lineup3_pos <- c(1, 2, 3, 4)
#'pos_list <- list(lineup1_pos, lineup2_pos, lineup3_pos)
#'rm(lineup1_pos, lineup2_pos, lineup3_pos)
#'
#'#Nominal size:
#'k <- c(6, 5, 4)
#'
#'#Call:
#'homog_diag(lineup_pres_list, lineup_abs_list, pos_list, k)
#'
#'@export

homog_diag <- function(lineup_pres_list, lineup_abs_list, pos_list, k){
  linedf <- suppressWarnings(diag_param(lineup_pres_list, lineup_abs_list, pos_list, k))
  par1 <- ln_diag_ratio(linedf)
  par2 <- var_lnd(linedf)
  par3 <- d_weights(linedf)
  par4 <- t(cbind(par1, par2, par3))
  par5 <- chi_diag(par4)
  par6 <- pchisq(par5, df = ncol(linedf)-1, lower.tail=F)
  par7 <- d_bar(par4)
  cat("Mean diagnosticity ratio:", par7)
  cat("\n")
  cat("Chi-square estimate (q):", par5)
  cat("\n")
  cat("Sig:", par6)
}
