#'Effective Size
#'
#'Function for computing Effective Size
#' @param lineup_tab A table of lineup choices.
#' @param k Number of members in the lineup. Must be specified by the user (scalar).
#' @param both Logical. Defaults to FALSE, returning Tredoux's adjusted effective
#'   size estimate only. If TRUE, both Malpass's (1981) original formulation and
#'   Malpass's adjusted version (Tredoux, 1998) are printed to the console.
#' @details Reduces the size of a lineup from a (corrected) nominal starting
#'          value by the degree to which members are, in sum, chosen below
#'          the level of chance expectation.
#' @seealso \code{\link{esize_T}}, whose \dQuote{Small-sample bias} section
#'   describes a downward bias affecting plug-in effective-size estimators at
#'   small numbers of choices, and an optional correction.
#'
#' @references Malpass, R. S. (1981). Effective size and defendant bias in
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
#'@return The Tredoux-adjusted Malpass effective-size estimate. With
#'  \code{both = TRUE}, the original and adjusted estimates are also printed.
#'@examples
#'#Data:
#'lineup_vec <- round(runif(100, 1, 6))
#'
#'#Call:
#'esize_m(lineup_vec, 6, both = TRUE)
#'esize_m(lineup_vec, 6)
#'
#'@export

esize_m <- function (lineup_tab, k, both =FALSE){
  if (inherits(lineup_tab, "table")) {
    datacheck3(lineup_tab, k)
    counts <- numeric(k)
    if (is.null(names(lineup_tab))) {
      counts[] <- as.numeric(lineup_tab)
    } else {
      counts[as.integer(names(lineup_tab))] <- as.numeric(lineup_tab)
    }
  } else {
    lineup_vec <- typecheck(lineup_tab)
    datacheck1(lineup_vec, k)
    counts <- tabulate(as.integer(lineup_vec), nbins = k)
  }
  if (sum(counts) == 0) {
    stop("At least one lineup choice is required.", call. = FALSE)
  }

  # Revised formulation using the declared nominal size (Tredoux, 1998).
  expected_adjusted <- sum(counts) / k
  esize_ma <- k - sum(abs(counts - expected_adjusted) / (2 * expected_adjusted))

  # Original formulation uses only lineup members receiving at least one choice.
  selected_counts <- counts[counts > 0]
  ka <- length(selected_counts)
  expected_original <- sum(selected_counts) / ka
  esize_ma_a <- ka - sum(abs(selected_counts - expected_original) /
                           (2 * expected_original))

  #Output
  if (both) {
    cat("Effective size (Malpass, 1981) = ", esize_ma_a,"\n")
    cat("Effective size (Malpass, 1981,","\n",
        "            adj Tredoux, 1998) = ", esize_ma, "\n")
  }
  unname(esize_ma)
}
