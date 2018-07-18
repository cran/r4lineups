#'Mean diagnosticity ratio for k lineup pairs
#'
#'Function for computing pooled estimator from a set of k diagnosticity ratios
#'
#'@param df A dataframe containing rows: ln(d), variance of ln(d), d weights
#'@return Mean diagnosticity ratio for k independent lineups
#'@references Malpass, R. S. (1981). Effective size and defendant bias in
#'            eyewitness identification lineups. \emph{Law and Human Behavior, 5}(4), 299-309.
#'
#'            Malpass, R. S., Tredoux, C., & McQuiston-Surrett, D. (2007). Lineup
#'            construction and lineup fairness. In R. Lindsay, D. F. Ross, J. D. Read,
#'            & M. P. Toglia (Eds.), \emph{Handbook of Eyewitness Psychology, Vol. 2: Memory for
#'            people} (pp. 155-178). Mahwah, NJ: Lawrence Erlbaum Associates.
#'
#'            Tredoux, C. G. (1998). Statistical inference on measures of lineup fairness.
#'           \emph{Law and Human Behavior, 22}(2), 217-237.
#'
#'            Tredoux, C. (1999). Statistical considerations when determining measures of
#'            lineup size and lineup bias. \emph{Applied Cognitive Psychology, 13}, S9-S26.
#'
#'            Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'            empirically assessing the fairness of a lineup. \emph{Law and Human Behavior,
#'            3}(4), 285-293.
#'@details. The order in which the estimates are bound together (i.e., their position
#'         in the dataframe) is important, and should always be as follows:
#'
#'         \itemize{
#'         \item row 1: var
#'         \item row 2: lnd
#'         \item row 3: wi}
#'@examples
#'#Target present data:
#'A <-  round(runif(100,1,6))
#'B <-  round(runif(70,1,5))
#'C <-  round(runif(20,1,4))
#'lineup_pres_list <- list(A, B, C)
#'rm(A, B, C)
#'
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
#'#Use diag param helper function to get data (n11, n21, n12, n22):
#'linedf <- diag_param(lineup_pres_list, lineup_abs_list, pos_list, k)
#'
#'#Compute ln(d):
#'ratio <- ln_diag_ratio(linedf)
#'
#'#Compute variance of ln(d):
#'var <- var_lnd(linedf)
#'
#'#Compute weights for pooled estimator:
#'wi <- d_weights(linedf)
#'
#'#Bind ln(d), variance of ln(d) and weights into one df (of 3 rows & x observations)
#'#(see Details above):
#'df <- t(cbind(ratio, var, wi))
#'
#'#Call:
#'d_bar(df)
#'@export

d_bar <- function(df){
  numerator   <- sum(df[3,]*df[2,])
  denominator <- sum(df[3,])
  d_bar       <- exp(numerator/denominator)
  return(d_bar)
}
