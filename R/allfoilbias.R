#'Bias for each lineup member
#'
#'Function to compute bias for each lineup member (assuming foil is suspect,
#'from Malpass, 1981)
#'
#'@param lineup_tab A table of lineup choices
#'@param target_pos A scalar, representing target position in lineup. Must be declared by user
#'@param k Nominal size (i.e., total number of members in lineup). Must be specified by user (scalar).
#'@examples
#'lineup_vec <- round(runif(100, 1, 6))
#'lineup_tab <- table(lineup_vec)
#'x <- allfoilbias(lineup_tab, 5, 6)
#'
#'@references Malpass, R. S. (1981). Effective size and defendant bias in
#'            eyewitness identification lineups. \emph{Law and Human Behavior, 5}(4), 299-309.
#'@export

allfoilbias <- function (lineup_tab, target_pos, k){
  datacheck3(lineup_tab, k)
  if (!is.numeric(target_pos) || length(target_pos) != 1L || is.na(target_pos) ||
      target_pos < 1 || target_pos > k || target_pos != as.integer(target_pos)) {
    stop("target_pos must be one integer member position between 1 and k.", call. = FALSE)
  }
  out <- vapply(seq_len(k), function(pos) lineup_prop_tab(lineup_tab, pos), numeric(1))
  names(out) <- paste0("member_", seq_len(k))
  out
}
