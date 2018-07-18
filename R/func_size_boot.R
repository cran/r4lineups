#'Bootstrapped Functional Size
#'
#'This function is a base function for the bootstrapping that ensues
#'to compute bootstrapped confidence intervals for the estimate of functional size
#'@param lineup_vec A numeric vector of lineup choices
#'@param d Indices for bootstrap resampling
#'@param target_pos A scalar, representing position of target in lineup. Must be declared by user
#'@seealso \code{\link[boot:boot]{boot}}: https://cran.r-project.org/web/packages/boot/boot.pdf
#'@details Function is called bootstrap resampling using boot function (in package 'boot')
#'         This function is never called by the user - it is called in the functional
#'         size master function
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
#'
#'@export

func_size.boot <- function(lineup_vec, d=d, target_pos){
    return(func_size(lineup_vec[d], target_pos))
}
