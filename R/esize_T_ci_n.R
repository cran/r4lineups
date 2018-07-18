#'Effective Size with Confidence Intervals from Normal Theory (Tredoux, 1998)
#'
#'Function for generating Effective Size (Tredoux, 1998) with CIs from normal theory
#'@param lineup_table A table of lineup choices
#'@param alpha Alpha level to be declared by user (scalar)
#'@details Reduces the size of a lineup from a (corrected) nominal starting
#'          value by the degree to which members are, in sum, chosen below
#'          the level of chance expectation
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
#'#Data:
#'lineup_vec <- round(runif(100, 1, 6))
#'lineup_table <- table(lineup_vec)
#'
#'#Call:
#'e_ci <- esize_T_ci_n(lineup_table, .95)
#'
#'@importFrom stats qnorm
#'@export

esize_T_ci_n <- function(lineup_table, alpha){
    N = sum(lineup_table)
    a <- 4/N
    b <- sum((lineup_table/N)^3)
    c <- (sum((lineup_table/N)^2)^2)
    var_i <- a*(b-c)
    sd_i <- sqrt(var_i)
    ci_low <- i_esize_T(lineup_table) + qnorm((1-alpha)/2)*sd_i
    ci_high <- i_esize_T(lineup_table) + qnorm(alpha+((1-alpha)/2))*sd_i
    e_ci <- list(ci_low = 1/(1-ci_low), ci_high = 1/(1-ci_high))
}
