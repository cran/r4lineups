#'Bias for each lineup member
#'
#'Function to compute bias for each lineup member (assuming foil is suspect,
#'from Malpass, 1981)
#'
#'@param lineup_table A table of lineup choices
#'@param target_pos A scalar, representing target position in lineup. Must be declared by user
#'@param k Nominal size (i.e., total number of members in lineup). Must be specified by user (scalar).
#'@examples
#'lineup_vec <- round(runif(100, 1, 6))
#'lineup_table <- table(lineup_vec)
#'x <- allfoilbias(lineup_table, 5, 6)
#'
#'@references Malpass, R. S. (1981). Effective size and defendant bias in
#'            eyewitness identification lineups. \emph{Law and Human Behavior, 5}(4), 299-309.
#'@export

allfoilbias <- function (lineup_table, target_pos, k){
    datacheck3(lineup_table, k)
    # Make df for testing
    linedf <- as.data.frame(matrix(ncol = length(lineup_table),
                                   nrow = length(lineup_table)))
    linebias <- NULL
    for (i in 1:length(lineup_table)){
        linebias[i] = lineup_prop_tab(lineup_table,target_pos)
        linedf[,i] = rot_vector(lineup_table)
        linetab = linedf[,i]
    }
    # linebias[length(linetab)] = lineup_prop_pos(linetab,1)
    linebias
}

