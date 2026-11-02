#'Helper function
#'
#'Checks that number of lineup choices contained in a data table is accurate
#'
#'@param lineup_tab A table of lineup choices
#'@param k Nominal size (i.e., total number of members in lineup)
#'@details This function ensures that a non-selected lineup member is not accidentally
#'         omitted from the data due to lack of selection by all mock witnesses.
#'         It functions as a check that the total number of lineup members is accurate.
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
