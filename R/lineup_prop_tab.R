#'Lineup proportion
#'
#'Computes the proportion of mock witnesses identifying a particular lineup member
#'@param lineup_tab A table of lineup choices
#'@param target_pos A scalar, representing target position in lineup. Must be declared by user
#'@return Returns a proportion indicating the frequency with which a lineup
#'        member was selected
#'@references Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'            empirically assessing the fairness of a lineup. \emph{Law and Human Behavior,
#'            3}(4), 285-293.
#'@examples
#'#Data:
#'lineup_vec <- round(runif(100, 1))
#'lineup_tab <- table(lineup_vec)
#'
#'#Call:
#'lineup_prop_tab(lineup_tab, 3)
#'lineup_prop_tab(table(lineup_vec), 2)
#'
#'@export

lineup_prop_tab <- function(lineup_tab, target_pos){
  counts <- as.numeric(lineup_tab)
  if (length(target_pos) != 1L || is.na(target_pos)) {
    stop("target_pos must identify exactly one lineup member.", call. = FALSE)
  }
  total <- sum(counts)
  if (!is.finite(total) || total <= 0 || any(!is.finite(counts)) || any(counts < 0)) {
    stop("lineup_tab must contain finite non-negative counts with a positive total.", call. = FALSE)
  }

  if (!is.null(names(lineup_tab))) {
    idx <- match(as.character(target_pos), names(lineup_tab))
    count <- if (is.na(idx)) 0 else counts[idx]
  } else {
    if (!is.numeric(target_pos) || target_pos != as.integer(target_pos) ||
        target_pos < 1 || target_pos > length(counts)) {
      stop("target_pos is outside the unnamed lineup table.", call. = FALSE)
    }
    count <- counts[target_pos]
  }
  unname(count / total)
}

