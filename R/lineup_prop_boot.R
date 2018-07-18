#'Bootstrapped lineup proportion
#'
#'Base function for computing bootstrapped lineup proportion for a lineup member
#'@param lineup_vec A numeric vector of lineup choices
#'@param d Indices for bootstrap sample. Argument used by boot function to
#'           select samples for bootstrapping
#'@param target_pos A scalar, representing target position in lineup. Must be declared by user
#'@seealso \code{\link[boot:boot]{boot}}: https://cran.r-project.org/web/packages/boot/boot.pdf
#'@details Function to call when bootstrap resampling using boot function
#'@references Davison,  A.C. & Hinkley,  D.V. (1997). \emph{Bootstrap methods and their
#'            application}. Cambridge University Press.
#'
#'            Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'            empirically assessing the fairness of a lineup. \emph{Law and Human Behavior,
#'            3}(4), 285-293.
#'
#'@examples
#'lineup_vec <- round(runif(100, 1, 6))
#'
#'bootobject <- boot::boot(lineup_vec, lineup_prop_boot, target_pos = 3, R = 1000)
#'cis <- boot::boot.ci(bootobject, conf = 0.95, type = "all")
#'
#'@export
#'@importFrom boot boot boot.ci

lineup_prop_boot <- function(lineup_vec, d,  target_pos){
    sum(lineup_vec[d] == target_pos)/length(lineup_vec)
}
