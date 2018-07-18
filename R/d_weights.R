#'Diagnosticity ratio weights
#'
#'Function to compute weights of each diagnosticity ratio  for k lineup pairs
#'@param linedf A dataframe of parameters for computing diagnosticity ratio
#'@details In order to obtain a pooled estimate of a set of diagnosticity ratios,
#'         we use a weight for each ratio that is equal to the inverse of its
#'         variance.
#'
#'         \strong{To get linedf, use the diag_param helper function}
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
#'@return A dataframe with one column containing weights for each pair of
#'         lineups for which diagnosticity ratio is being calculated.
#'@references Tredoux, C. G. (1998). Statistical inference on measures of lineup
#'            fairness. \emph{Law and Human Behavior, 22}(2), 217-237.
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
#'#Call:
#'wi <- d_weights(linedf)
#'
#'@export

d_weights <- function(linedf){
    numerator   <- linedf$n11*linedf$n12*(linedf$n11+linedf$n21)*
                   (linedf$n12+linedf$n22)
    denominator <- linedf$n11*linedf$n22*(linedf$n11+linedf$n21)+
                   linedf$n12*linedf$n21*(linedf$n12+linedf$n22)
    wi <- numerator/denominator
    wi <- as.data.frame(wi)
    return(wi)

}
