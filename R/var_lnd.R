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
#'          \item \emph{n22}: Number of mock witnesses who did not identify the suspect in the
#'              target absent condition
#'              }
#'@return A dataframe containing the estimated variance of the log diagnosticity ratio for
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
#'#Use diag param helper function to get data (n11, n21, n12, n22):
#'linedf <- diag_param(lineup_pres_list, lineup_abs_list, pos_list, k)
#'#Call:
#'var <- var_lnd(linedf)
#'
#'@export

var_lnd <- function(linedf){
  required <- c("n11", "n21", "n12", "n22")
  if (!all(required %in% names(linedf))) {
    stop("linedf must contain n11, n21, n12, and n22.", call. = FALSE)
  }
  cells <- as.data.frame(linedf[required])
  if (any(cells < 0) || any(!is.finite(as.matrix(cells)))) {
    stop("linedf counts must be finite and non-negative.", call. = FALSE)
  }
  if (any(cells$n11 == 0 | cells$n12 == 0)) {
    stop(paste0(
      "The large-sample homogeneity variance requires positive suspect-ID ",
      "counts in both conditions; Tredoux (1998) did not specify a zero-cell correction."
    ), call. = FALSE)
  }
  var <- 1 / cells$n11 - 1 / (cells$n11 + cells$n21) +
    1 / cells$n12 - 1 / (cells$n12 + cells$n22)
  data.frame(var = var)
}
