#'Functional Size with Bootstrapped Confidence Intervals
#'
#'This function is a master function, calling other functions it needs,
#'and reporting results in some detail
#'
#'@param lineup_vec A numeric vector of lineup choices
#'@param target_pos A scalar, representing target position in lineup. Must be declared by user
#'@param R Number of bootstrap samples. Defaults to 1000
#'@param k Number of members in lineup. Must be specified by user (scalar).
#'@return Invisibly returns the functional-size estimate; the normal,
#'  percentile and BCa bootstrap intervals; the exact interval; the number of
#'  replications; the number of replications in which nobody chose the target
#'  (\code{n_infinite}); and the bootstrap object, whose statistic is the
#'  proportion choosing the target.
#'@details Functional size is the reciprocal of the proportion of mock witnesses
#'  choosing the target, p. The bootstrap resamples p, which stays finite when a
#'  resample contains no target choices, and the interval limits for p are
#'  inverted. Percentile and BCa intervals respect monotone transformations, so
#'  they are the intervals for functional size itself, computed from the whole
#'  bootstrap distribution; a lower limit of 0 for p gives an unbounded
#'  (\code{Inf}) upper limit. The normal interval is the normal-theory interval
#'  for p, inverted. The exact interval inverts the Clopper-Pearson interval for
#'  p (\code{\link[stats]{binom.test}}).
#'  Near zero or one, empirical bootstrap intervals can have severe undercoverage,
#'  including a degenerate interval when the suspect was never chosen. Prefer
#'  the exact interval in these cases; retaining infinite limits alone does not
#'  guarantee nominal bootstrap coverage.
#'
#'  Function depends on functions from package 'boot'
#'@seealso \code{\link[boot:boot]{boot}}: https://cran.r-project.org/web/packages/boot/boot.pdf
#'@references Davison,  A.C. & Hinkley,  D.V. (1997). \emph{Bootstrap methods and their
#'            application}. Cambridge University Press.
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
#'#Data:
#'lineup_vec <- round(runif(100, 1, 6))
#'target_pos <- 3
#'
#'#Call:
#'x <- func_size_report(lineup_vec, target_pos, 6)
#'x <- func_size_report(lineup_vec, 3, 6)
#'
#'@export
#'@importFrom boot boot boot.ci

func_size_report <- function(lineup_vec, target_pos, k, R = 1000){
  lineup_vec <- typecheck(lineup_vec)
  datacheck1(lineup_vec, k)
  if (!is.numeric(R) || length(R) != 1L || R < 1 || R != as.integer(R)) {
    stop("R must be a positive integer.", call. = FALSE)
  }
  estimate <- suppressWarnings(func_size(lineup_vec, target_pos))
  prop_boot <- function(x, d, target_pos) mean(x[d] == target_pos)
  temp1 <- boot(lineup_vec, prop_boot, target_pos = target_pos, R = R)
  temp2 <- tryCatch(
    suppressWarnings(boot.ci(temp1, type = c("norm", "bca", "perc"))),
    error = function(e) NULL
  )
  # Limits for p, reversed and inverted; p limits are kept within [0, 1].
  invert <- function(lim) {
    if (is.null(lim)) return(c(NA_real_, NA_real_))
    1 / rev(pmin(pmax(lim, 0), 1))
  }
  normal <- invert(temp2$normal[2:3])
  percentile <- invert(temp2$percent[4:5])
  bca <- invert(temp2$bca[4:5])
  exact <- invert(stats::binom.test(sum(lineup_vec == target_pos),
                                    length(lineup_vec))$conf.int)
  n_infinite <- sum(temp1$t[, 1] == 0)

  cat("Functional size of lineup is ", round(estimate, 3), "\n", sep = "")
  cat("Confidence intervals [95%]\n")
  cat("Normal Theory", round(normal, 3), "\n")
  cat(sprintf("Bootstrap: percentile (R = %d)", R), round(percentile, 3), "\n")
  cat(sprintf("Bootstrap: bias-corrected (R = %d)", R), round(bca, 3), "\n")
  cat("Exact (Clopper-Pearson)", round(exact, 3), "\n")
  if (n_infinite > 0) {
    cat(sprintf("%d of %d bootstrap samples had no target choices (functional size Inf)\n",
                n_infinite, R))
  }

  invisible(list(
    estimate = estimate,
    normal = normal,
    percentile = percentile,
    bca = bca,
    exact = exact,
    R = as.integer(R),
    n_infinite = n_infinite,
    boot = temp1
  ))
}
