#'Effective Size per Foils
#'
#'A package-specific count of members whose choice-proportion intervals include
#'chance guessing. This is distinct from Malpass' continuous effective size.
#'@param lineup_vec A numeric vector of lineup choices
#'@param target_pos A numeric vector indexing all lineup members
#'@param k Number of members in lineup. Must be specified by user (scalar).
#'@param conf Confidence level for the intervals, e.g. 0.95 for 95 percent
#'            intervals. Defaults to 0.95. May be specified by user (scalar).
#'@param R Number of bootstrap replications. Defaults to 1000.
#'@return A single integer: the number of lineup members whose bootstrapped
#'        confidence interval for the proportion choosing them includes the
#'        chance value \eqn{1/k}. These are the members that a mock witness
#'        chose at a rate not distinguishable from guessing.
#'@details Because the result is a count of members rather than a continuous
#'         quantity, it changes only in whole steps as \code{conf} varies, and
#'         it is flat across ranges of \code{conf} in which no member's interval
#'         crosses \eqn{1/k}. A value that does not move when \code{conf} is
#'         changed from, say, 0.95 to 0.99 is therefore expected behaviour and
#'         not a sign that the argument is being ignored. Widening the interval
#'         can only ever increase the count.
#'
#'         Intervals are computed by \code{\link{lineup_boot_allprop}}, and are
#'         not corrected for the fact that \code{k} of them are examined at once.
#'@examples
#'#Data:
#'lineup_vec <- round(runif(100, 1, 6))
#'target_pos <- c(1, 2, 3, 4, 5, 6)
#'
#'#Call:
#'eff_size_per_foils(lineup_vec, target_pos, 6)
#'eff_size_per_foils(lineup_vec, target_pos, conf = 0.95, 6)
#'@references Malpass, R S. (1981). Effective size and defendant bias in eyewitness
#'            identification lineups. \emph{Law and Human Behavior, 5}, 299-309.
#'
#'            Tredoux, C. G. (1998). Statistical inference on measures of lineup
#'            fairness. \emph{Law and Human Behavior, 22}(2), 217-237.
#'
#'@export

eff_size_per_foils <- function(lineup_vec, target_pos, k, conf = 0.95, R = 1000){
  lineup_vec <- typecheck(lineup_vec)
  if (!setequal(as.integer(target_pos), seq_len(k))) {
    stop("target_pos must contain each lineup member position exactly once.", call. = FALSE)
  }
  ci <- lineup_boot_allprop(lineup_vec, k, conf = conf, R = R)
  k0 <- 1/k
  ci_count <- ci[, 1] <= k0 & ci[, 2] >= k0
  sum(ci_count)
}
