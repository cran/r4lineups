#'Helper functions
#'
#'Several helper functions that compute bootstrap confidence limits from proportions.
#'@param prop A single finite proportion between 0 and 1.
#'@param n A positive whole-number sample size. \code{n * prop} must be a whole number.
#'@examples
#' # Expand a choice proportion of 0.25 among 20 mock witnesses into a
#' # binary choice vector
#' makevec_prop(0.25, 20)
#'@export
#'@importFrom purrr map
#'@importFrom stats quantile

makevec_prop <- function(prop, n) {
    if (!is.numeric(prop) || length(prop) != 1L || !is.finite(prop) ||
        prop < 0 || prop > 1) {
        stop("prop must be a single finite number between 0 and 1")
    }
    if (!is.numeric(n) || length(n) != 1L || !is.finite(n) ||
        n < 1 || n != as.integer(n)) {
        stop("n must be a positive whole number")
    }
    n_selected <- n * prop
    if (abs(n_selected - round(n_selected)) > sqrt(.Machine$double.eps)) {
        stop("n * prop must be a whole number")
    }
    n_selected <- as.integer(round(n_selected))
    c(rep(1L, n_selected), rep(0L, as.integer(n) - n_selected))
}

bp <- function(lineup_vec){
    mean(.resample_values(lineup_vec) == TRUE)
}

boot025 <- function(prop,n){
    y = makevec_prop(prop,n)
    x <- map_dbl(1:1000,~bp(y))
    quantile(x,probs = .025)
}

boot0975 <- function(prop,n){
    y = makevec_prop(prop,n)
    x <- map_dbl(1:1000,~bp(y))
    quantile(x,probs = .975)
}
