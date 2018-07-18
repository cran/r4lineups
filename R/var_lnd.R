#' Variance of ln of diagnosticity ratio
#'
#' Function to compute variance of ln(d) for k lineup pairs
#'
#' @param linedf A dataframe of parameters for computing diagnosticity ratio
#' @details \strong{To get linedf, use the diag_param helper function}
#'
#'          \emph{diag_param} returns a dataframe containing the following:
#'
#'          \itemize{
#'          \item \emph{n11}: Number of mock witnesses who identified the suspect in the target
#'               present condition
#'
#'          \item \emph{n21}: Number of mock witnesses who did not identify the suspect in the
#'              target present condition
#'
#'          \item \emph{n12}: Number of mock witnesses who identified the suspect in the target
#'              absent condition
#'
#'          \item \emph{n13}: Number of mock witnesses who did not identify the suspect in the
#'              target absent condition
#'              }
#'@return A dataframe containing ln of the variance of the diagnosticity ratio for
#'        each lineup.
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
#'            lineup size and lineup bias. \emph{Applied Cognitive Psychology}, 13, S9-S26.
#'
#'            Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'            empirically assessing the fairness of a lineup. \emph{Law and Human Behavior,
#'            3}(4), 285-293.
#'
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
#'#Call:
#'var <- var_lnd(linedf)
#'
#'@export

var_lnd <- function(linedf){

    var <- (linedf$n21/(linedf$n11+(linedf$n11+linedf$n21)))+
        (linedf$n22/(linedf$n12+(linedf$n12+linedf$n22)))
    var <- as.data.frame(var)

    return(var)

    }
