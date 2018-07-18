#'Master Function: Comparing Effective Size
#'
#'Function for comparing effective size of two independent lineups (Tredoux, 1998)
#'
#'@param linedf A dataframe of lineup data. Contains 2 columns, each of which
#'              hold data for 2 independent lineups
#'@return Effective size, significance level, and confidence intervals (95% ,
#'         normal theory, percentile & bias-corrected)
#'@details This function is a master function, calling other functions
#'         it needs, and reporting results in some detail
#'@seealso \code{\link[boot:boot]{boot}}: https://cran.r-project.org/web/packages/boot/boot.pdf
#'@references Davison,  A.C. & Hinkley,  D.V. (1997). \emph{Bootstrap methods and their
#'                  application}. Cambridge University Press.
#'
#'            Malpass, R. S. (1981). Effective size and defendant bias in
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
#'@examples
#'#Data:
#'
#'lineup_vec1 <- round(runif(100, 1, 6))
#'lineup_vec2 <- round(runif(100, 1, 6))
#'linedf <- as.data.frame(cbind(lineup_vec1, lineup_vec2))
#'
#'#Call:
#'x <- effsize_compare(linedf)
#'
#'@export
#'@importFrom boot boot boot.ci

effsize_compare <- function(linedf){
    cat ("\n")
    temp1 <- boot(linedf, compare_eff_sizes.boot, R=1000)
    temp2 <- boot.ci(temp1, type = c("norm","bca","perc"))
    cat ("The two Effective sizes are ",esize_T(table(linedf[1]))," ",
         esize_T(table(linedf[2])))
    cat ("\n")
    cat ("If the interval includes 0, ns at p = .05")
    cat ("\n")
    cat ("Confidence intervals of difference [95%]")
    cat ("\n")
    cat ("Normal Theory", round(temp2$normal[2:3],3))
    cat ("\n")
    cat ("Bootstrap: percentile (R = 1000)", round(temp2$percent[4:5],3))
    cat ("\n")
    cat ("Bootstrap: bias-corrected (R = 1000)", round(temp2$bca[4:5],3))
}



