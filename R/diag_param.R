#'Parameters for diagnosticity ratio
#'
#'This function calculates the parameters needed to calculate the diagnosticity
#'         ratio for several lineup pairs.
#'
#'@param lineup_pres_list A list containing k vectors of lineup choices for k lineups, in which the
#'                        target was present
#'@param lineup_abs_list A list containing k vectors of lineup choices for k lineups, in which the
#'                       target was absent
#'@param pos_list Suspect positions for each lineup pair. Supply a numeric vector
#'                with one position per pair when TP and TA suspect positions match,
#'                or a list whose elements contain either one shared position or
#'                two positions in TP, TA order.
#'@param k A vector indexing number of members in each lineup pair. Must be specified by user (scalar).
#'@return Returns a dataframe containing:
#'
#'         \itemize{
#'          \item \emph{n11}: Number of mock witnesses who identified the suspect in the target
#'               present condition
#'
#'          \item \emph{n21}: Number of mock witnesses who did not identify the suspect in the
#'              target present condition
#'
#'          \item \emph{n12}: Number of mock witnesses who identified the suspect in the target
#'              absent condition
#'
#'          \item \emph{n22}: Number of mock witnesses who did not identify the suspect in the
#'              target absent condition
#'              }
#'@details \itemize{
#'          \item Lineup pairs consist of one lineup in which the target was present (TP)
#'                and one lineup in which the target was absent (TA).
#'
#'          \item Each lineup pair must occupy corresponding positions in the TA and TP lists.
#'
#'                Example:
#'
#'                For a lineup pair A that consists of (1)TP lineup and (2)TA lineup:
#'                A(1) is the first vector in the TP list
#'                A(2) is the first vector in the TA list
#'        \item The order in which nominal size for each lineup pair is listed must
#'              also correspond with the positions of each respective lineup in the
#'              lineup lists (i.e., if lineup 1 has k = 6, then the first element of
#'              vector 'k' = 6)
#'
#'       \item Data must be in a list format. This allows the function to compare
#'             lineups in which the number of choices and number of lineup members differs.
#'       \item TP and TA vectors may have different sample sizes; they are counted
#'             independently and are never compared element by element.}
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
#'@examples
#'#Target present data:
#'A <- rep(1:6, length.out = 100)
#'B <- rep(1:5, length.out = 70)
#'C <- rep(1:4, length.out = 20)
#'lineup_pres_list <- list(A, B, C)
#'rm(A, B, C)
#'
#'
#'#Target absent data:
#'A <- rep(6:1, length.out = 100)
#'B <- rep(5:1, length.out = 70)
#'C <- rep(4:1, length.out = 20)
#'lineup_abs_list <- list(A, B, C)
#'rm(A, B, C)
#'
#'# Suspect position for each TP/TA pair
#'pos_list <- c(3, 2, 1)
#'
#'#Nominal size:
#'k <- c(6, 5, 4)
#'
#'#Call:
#'linedf <- diag_param(lineup_pres_list, lineup_abs_list, pos_list, k)
#'@importFrom stats pchisq
#'@export

diag_param <- function(lineup_pres_list, lineup_abs_list, pos_list, k){
  if (!is.list(lineup_pres_list) || !is.list(lineup_abs_list) ||
      length(lineup_pres_list) != length(lineup_abs_list) ||
      length(lineup_pres_list) != length(k)) {
    stop("TP lineups, TA lineups, positions, and k must describe the same number of pairs.",
         call. = FALSE)
  }
  datacheck4(pos_list, k)
  if (!is.list(pos_list)) {
    pos_list <- as.list(pos_list)
  }
  rows <- lapply(seq_along(lineup_pres_list), function(i) {
    tp <- typecheck(lineup_pres_list[[i]])
    ta <- typecheck(lineup_abs_list[[i]])
    datacheck1(tp, k[[i]])
    datacheck1(ta, k[[i]])
    positions <- pos_list[[i]]
    pos_tp <- positions[[1L]]
    pos_ta <- if (length(positions) == 2L) positions[[2L]] else pos_tp
    n11 <- sum(tp == pos_tp)
    n12 <- sum(ta == pos_ta)
    data.frame(
      n11 = n11,
      n21 = length(tp) - n11,
      n12 = n12,
      n22 = length(ta) - n12
    )
  })
  do.call(rbind, rows)
}
