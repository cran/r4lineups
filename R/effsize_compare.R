#'Master Function: Comparing Effective Size
#'
#'Function for comparing effective size of two independent lineups (Tredoux, 1998)
#'
#'@param linedf A dataframe of lineup data. Contains 2 columns, each of which
#'              hold data for 2 independent lineups
#'@param R Number of bootstrap replications.
#'@return Effective size, significance level, and confidence intervals (95% ,
#'         normal theory, percentile & bias-corrected)
#'@details This function is a master function, calling other functions
#'         it needs, and reporting results in some detail
#'         Each lineup is resampled independently; row alignment has no sampling
#'         meaning. Confidence intervals are bootstrap approximations and can be
#'         unreliable for small or nearly uniform empirical choice distributions.
#'         Normal and BCa intervals can under-cover in finite concentrated samples;
#'         percentile intervals can be conservative near uniformity. None provides
#'         a distribution-free level-.05 test of equal population effective sizes.
#'@seealso \code{\link[boot:boot]{boot}}: https://cran.r-project.org/web/packages/boot/boot.pdf
#'@references Davison,  A.C. & Hinkley,  D.V. (1997). \emph{Bootstrap methods and their
#'                  application}. Cambridge University Press.
#'
#'            Malpass, R. S. (1981). Effective size and defendant bias in
#'            eyewitness identification lineups. \emph{Law and Human Behavior, 5}(4), 299-309.
#'
#'            Malpass, R. S., Tredoux, C., & McQuiston-Surrett, D. (2007). Lineup
#'            construction and lineup fairness. In R. Lindsay, D. F. Ross, J. D. Read,
#'            & M. P. Toglia (Eds.), \emph{Handbook of Eyewitness Psychology, Vol. 2: Memory for
#'            people} (pp. 155-178). Mahwah, NJ: Lawrence Erlbaum Associates.
#'
#'            Tredoux, C. G. (1998). Statistical inference on measures of lineup fairness.
#'           \emph{Law and Human Behavior, 22}(2), 217-237.
#'
#'            Tredoux, C. (1999). Statistical considerations when determining measures of
#'            lineup size and lineup bias. \emph{Applied Cognitive Psychology, 13}, S9-S26.
#'
#'            Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'            empirically assessing the fairness of a lineup. \emph{Law and Human Behavior,
#'            3}(4), 285-293.
#'@examples
#'#Data:
#'
#'lineup_vec1 <- round(runif(100, 1, 6))
#'lineup_vec2 <- round(runif(100, 1, 6))
#'linedf <- as.data.frame(cbind(lineup_vec1, lineup_vec2))
#'
#'#Call:
#'x <- effsize_compare(linedf, R = 100)
#'
#'@export
#'@importFrom boot boot boot.ci

effsize_compare <- function(linedf, R = 1000){
    if (!is.data.frame(linedf) || ncol(linedf) != 2L || nrow(linedf) < 2L) {
      stop("linedf must be a two-column data frame with at least two rows.", call. = FALSE)
    }
    if (!is.numeric(R) || length(R) != 1L || R < 2 || R != as.integer(R)) {
      stop("R must be an integer of at least 2.", call. = FALSE)
    }
    cat ("\n")
    if (anyNA(linedf)) stop("Independent lineup samples must not contain missing choices.", call. = FALSE)
    long <- data.frame(choice = c(as.character(linedf[[1]]), as.character(linedf[[2]])),
                       sample = rep(1:2, each = nrow(linedf)))
    statistic <- function(data, indices) {
      x <- data[indices, , drop = FALSE]
      esize_T(table(x$choice[x$sample == 1L])) -
        esize_T(table(x$choice[x$sample == 2L]))
    }
    temp1 <- boot(long, statistic, R = R, strata = long$sample)
    # Exact within-sample jackknife influences. Arbitrary pairing of independent
    # rows must enter neither the bootstrap nor the BCa acceleration.
    influences <- unlist(lapply(1:2, function(group) {
      ix <- which(long$sample == group)
      loo <- vapply(ix, function(j) statistic(long[-j, , drop = FALSE],
                                             seq_len(nrow(long) - 1L)), numeric(1))
      (length(ix) - 1L) * (mean(loo) - loo)
    }))
    temp2 <- suppressWarnings(boot.ci(temp1, type = c("norm","bca","perc"),
                                    L = influences))
    estimates <- c(esize_T(table(linedf[[1]])), esize_T(table(linedf[[2]])))
    normal <- if (!is.null(temp2$normal)) temp2$normal[2:3] else c(NA_real_, NA_real_)
    percentile <- if (!is.null(temp2$percent)) temp2$percent[4:5] else c(NA_real_, NA_real_)
    bca <- if (!is.null(temp2$bca)) temp2$bca[4:5] else c(NA_real_, NA_real_)
    cat ("The two Effective sizes are ", estimates[1], " ", estimates[2])
    cat ("\n")
    cat ("If the interval includes 0, ns at p = .05")
    cat ("\n")
    cat ("Confidence intervals of difference [95%]")
    cat ("\n")
    cat ("Normal Theory", round(normal,3))
    cat ("\n")
    cat (sprintf("Bootstrap: percentile (R = %d)", R), round(percentile,3))
    cat ("\n")
    cat (sprintf("Bootstrap: bias-corrected (R = %d)", R), round(bca,3))
    invisible(list(estimates = estimates, difference = unname(temp1$t0),
                   normal = normal, percentile = percentile, bca = bca,
                   R = as.integer(R), boot = temp1))
}


