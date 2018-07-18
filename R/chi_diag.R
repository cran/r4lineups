#'Chi-squared estimate of homogeneity of diagnosticity ratio
#'
#'Function for getting chi-squared value for homogeneity of diagnosticity ratios
#'@param df A dataframe containing: ln(d), variance of ln(d), d weights
#'@return Chi squared estimate of homogeneity of diagnosticity ratios for k independent lineups
#'@details \itemize{
#'         \item To compute linedf, use the diag_param helper function.
#'
#'         \item To compute df, apply ln_diag_ratio, var_lnd & d_weights functions
#'         to linedf, then bind results into one dataframe (see \strong{Examples})
#'
#'         \item The order in which the estimates are bound together (i.e., their position
#'         in the dataframe) is important, and should always be as follows:
#'         row 1: \emph{var}, row 2: \emph{lnd}, row 3: \emph{wi}.
#'         }
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
#'#Get ln(d), variance of ln(d) & d weights:
#'ratio <- ln_diag_ratio(linedf)
#'var <- var_lnd(linedf)
#'wi <- d_weights(linedf)
#'
#'#Bind estimates into one df of 3 rows & x observations
#'#(see Details above)
#'df <- t(cbind(ratio, var, wi))
#'
#'#Call:
#'chi_diag(df)
#'
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
#'            lineup size and lineup bias. \emph{Applied Cognitive Psychology, 13}, S9-S26.
#'
#'            Wells, G. L., Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'            empirically assessing the fairness of a lineup. \emph{Law and Human Behavior,
#'            3}(4), 285-293.
#'@export

chi_diag <- function(df){
    q <- sum(((df[2,]-log(d_bar(df)))^2)/(df[1,]))
    return(q)
}
