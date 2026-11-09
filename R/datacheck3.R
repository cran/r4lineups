#'Helper function
#'
#'Checks that number of lineup choices contained in a data table is accurate
#'
#'@param lineup_tab A table of lineup choices
#'@param k Nominal size (i.e., total number of members in lineup)
#'@details This function checks that the table has no more than \code{k} cells,
#'         that its counts are finite and non-negative, and that its names (if
#'         any) are integer positions between 1 and \code{k}. An unnamed table
#'         must have exactly \code{k} cells. A named table with fewer than
#'         \code{k} positions, such as \code{table()} of choices in which one
#'         member was never chosen, is accepted, because the missing member cannot
#'         be detected from the table, so \code{k} must be supplied correctly by
#'         the user. \code{lineup_table()} builds a table with a cell for every
#'         member.
#'@examples
#' # Table of choices from 50 mock witnesses to a 6-member lineup
#' set.seed(1)
#' lineup_tab <- table(sample(1:6, 50, replace = TRUE))
#' datacheck3(lineup_tab, 6)
#'@export


datacheck3 <- function(lineup_tab, k){
  if (!is.numeric(k) || length(k) != 1L || is.na(k) || k < 1 || k != as.integer(k)) {
    stop("k must be a positive integer.", call. = FALSE)
  }
  counts <- as.numeric(lineup_tab)
  if (length(counts) > k || anyNA(counts) || any(!is.finite(counts)) || any(counts < 0)) {
    stop("lineup_tab must contain at most k finite non-negative counts.", call. = FALSE)
  }
  if (!is.null(names(lineup_tab))) {
    positions <- suppressWarnings(as.numeric(names(lineup_tab)))
    if (anyNA(positions) || any(!is.finite(positions)) ||
        any(positions != floor(positions)) || anyDuplicated(positions) ||
        any(positions < 1L | positions > k)) {
      stop("Named lineup-table positions must be integers between 1 and k.", call. = FALSE)
    }
  } else if (length(counts) != k) {
    stop("An unnamed lineup table must contain exactly k counts.", call. = FALSE)
  }
  invisible(TRUE)
}
