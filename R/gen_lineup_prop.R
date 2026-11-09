#'Lineup proportion over dataframe
#'
#'Function for computing lineup proportion over a dataframe
#'@param lineup_boot_df Dataframe of lineup choices (usually a bootstrapped set)
#'@param target_pos A scalar, representing target position in lineup. Must be declared by user.
#'@param k A vector indexing number of members in each lineup pair (nominal size). Must be specified by user (scalar).
#'@return A vector of bootstrapped proportions, indicating the frequency with which
#'        a target was identified in a lineup
#'        Length of vector = number of bootstrap sample draws
#'@export
#'@importFrom purrr map map_dbl
#'@importFrom magrittr %>%
#'@examples
#'#Data
#'lineup_vec <- round(runif(100, 1, 6))
#'target_pos <- 3
#'
#'#Get bootstrapped data:
#'lineup_boot_df <- gen_boot_samples(lineup_vec, 1000)
#'
#'#Call:
#'lineuprops <- gen_lineup_prop(lineup_boot_df, 3, 6)
#'


gen_lineup_prop <- function(lineup_boot_df, target_pos, k){
  if (is.matrix(lineup_boot_df)) lineup_boot_df <- as.data.frame(lineup_boot_df)
  if (!is.list(lineup_boot_df) || length(lineup_boot_df) == 0L) {
    stop("lineup_boot_df must contain non-empty columns of lineup choices.",
         call. = FALSE)
  }
  vapply(lineup_boot_df, function(x) lineup_prop_vec(x, target_pos, k), numeric(1))
}
