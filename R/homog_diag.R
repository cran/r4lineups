#' Master function: Homogeneity of diagnosticity ratio
#'
#' This function provides assesses the homogeneity of the diagnosticity ratio of
#'  k lineup pairs.
#'
#'@param lineup_pres_list A list containing k vectors of lineup choices for k lineups, in which the
#'                        target was present
#'@param lineup_abs_list A list containing k vectors of lineup choices for k lineups, in which the
#'                       target was absent
#'@param pos_list Suspect positions for each lineup pair. See \code{diag_param()}.
#'@param k A vector indexing number of members in each lineup pair (nominal size). Must be specified by user (scalar).
#'@param conf.level Confidence level for the per-pair intervals, and the
#'                  complement of the level at which heterogeneity is declared.
#'                  Defaults to 0.95.
#'@return Invisibly, a list with the pooled (precision-weighted) diagnosticity
#'        ratio, the chi-square statistic and its degrees of freedom and p-value,
#'        a \code{heterogeneous} flag, and a \code{parameters} data frame giving
#'        the cell counts plus \eqn{d}, \eqn{\ln d}, its variance, its weight and
#'        a confidence interval for each lineup pair. The same information is
#'        printed.
#'@details Master function for assessing homogeneity of diagnosticity ratio for
#'         k independent lineups.
#'
#'         The chi-square is Cochran's Q, \eqn{\sum w_i (\ln d_i - \ln \bar{d})^2},
#'         so it measures the \emph{spread} of the k ratios around their pooled
#'         value. It answers a different question from a confidence interval on
#'         the pooled ratio, which asks whether the average ratio differs from 1.
#'         The two can disagree: ratios that differ in direction (say 6, 1 and
#'         0.44) give a very large Q while averaging to roughly 1. When Q is
#'         significant the ratios do not share a common value, the pooled
#'         estimate should not be reported as a single summary, and the pairs
#'         should be interpreted separately. This function prints a note to that
#'         effect and records it in the \code{heterogeneous} element.
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
#'homog_diag(lineup_pres_list, lineup_abs_list, pos_list, k)
#'
#'@export

homog_diag <- function(lineup_pres_list, lineup_abs_list, pos_list, k,
                       conf.level = 0.95){
  linedf <- suppressWarnings(diag_param(lineup_pres_list, lineup_abs_list, pos_list, k))
  par1 <- var_lnd(linedf)
  par2 <- ln_diag_ratio(linedf, correction = FALSE)
  par3 <- d_weights(linedf)
  par4 <- t(cbind(par1, par2, par3))
  par5 <- chi_diag(par4)
  df <- nrow(linedf) - 1L
  par6 <- pchisq(par5, df = df, lower.tail = FALSE)
  par7 <- d_bar(par4)
  pairs <- diag_pair_summary(linedf, conf.level = conf.level)
  print_diag_pairs(pairs, conf.level = conf.level)
  cat("\n")
  cat("Mean diagnosticity ratio:", par7)
  cat("\n")
  cat("Chi-square estimate (q):", par5)
  cat("\n")
  cat("Sig:", par6)
  cat("\n")
  heterogeneous <- isTRUE(par6 < 1 - conf.level)
  print_pooling_note(heterogeneous, conf.level = conf.level)
  invisible(list(
    mean_diagnosticity = par7,
    chi_square = par5,
    df = df,
    p_value = par6,
    heterogeneous = heterogeneous,
    parameters = pairs
  ))
}


# Internal helpers shared by homog_diag() and homog_diag_boot().
#
# diag_pair_summary() turns the raw cell counts from diag_param() into the
# per-pair quantities a reader needs to interpret Cochran's Q: the ratio
# itself, its log, the large-sample variance of the log, the weight it carries
# in the pooled estimate, and a normal-theory interval on the ratio scale.
diag_pair_summary <- function(linedf, conf.level = 0.95) {
  if (!is.numeric(conf.level) || length(conf.level) != 1L ||
      is.na(conf.level) || conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be a single number between 0 and 1.", call. = FALSE)
  }
  ln_d <- ln_diag_ratio(linedf, correction = FALSE)$lnd
  v    <- var_lnd(linedf)$var
  se   <- sqrt(v)
  z    <- stats::qnorm(1 - (1 - conf.level) / 2)
  data.frame(
    linedf,
    d       = exp(ln_d),
    ln_d    = ln_d,
    var_lnd = v,
    wi      = 1 / v,
    lower   = exp(ln_d - z * se),
    upper   = exp(ln_d + z * se)
  )
}

print_diag_pairs <- function(pairs, conf.level = 0.95) {
  pct <- format(100 * conf.level, trim = TRUE)
  tbl <- data.frame(
    pair = seq_len(nrow(pairs)),
    d    = sprintf("%.3f", pairs$d),
    ci   = sprintf("[%.2f, %.2f]", pairs$lower, pairs$upper),
    row.names = NULL
  )
  names(tbl) <- c("pair", "d", paste0(pct, "% CI"))
  cat("Diagnosticity ratio for each lineup pair:\n")
  print(tbl, row.names = FALSE)
}

# The pooled ratio is only a meaningful summary when the k ratios share a common
# value. Say so when they do not, rather than leaving the reader to reconcile a
# large Q with a pooled interval that covers 1.
print_pooling_note <- function(heterogeneous, conf.level = 0.95) {
  alpha <- format(1 - conf.level, trim = TRUE)
  msg <- if (heterogeneous) {
    paste0("Note: the ratios are heterogeneous (p < ", alpha, "), so they do ",
           "not share a common diagnosticity ratio. Q measures how far the ",
           "ratios lie from their pooled value, which is a different question ",
           "from whether the pooled ratio differs from 1. The mean above ",
           "averages ratios that genuinely differ and should not be reported ",
           "as a single summary for these lineups; interpret the pairs ",
           "separately.")
  } else {
    paste0("Note: no evidence of heterogeneity (p >= ", alpha, "), so the ",
           "ratios are consistent with a common value and the pooled mean is ",
           "a reasonable summary.")
  }
  cat("\n")
  cat(strwrap(msg, width = 76), sep = "\n")
}
