#'Helper functions
#'
#'Several helper functions that compute bootcis from proportions
#'@param prop A dataframe of bootstrapped proportions
#'@param n Number of lineup members
#'@export
#'@importFrom purrr map
#'@importFrom stats quantile

makevec_prop <- function(prop,n){
    lineup_vec_1 = rep(1,n*prop)
    lineup_vec_2 = rep(0,n-n*prop)
    lineup_vec = c(lineup_vec_1,lineup_vec_2)
}

bp <- function(lineup_vec){
    (sum(sample(lineup_vec, length(lineup_vec),
                replace = TRUE) == TRUE))/length(lineup_vec)
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
