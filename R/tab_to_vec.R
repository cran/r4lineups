#'Lineup vector
#'
#'A function that takes a table of lineup choices and generates a vector of raw
#'lineup data
#'
#'@param lineup_table A table of lineup choices
#'@param k Nominal size (i.e.,total number of lineup members).
#'                Must be declared by user
#'@return Returns a vector of lineup choices
#'@examples
#'#Data:
#'lineup_vec <- round(runif(100, 1, 6))
#'lineup_table <- table(lineup_vec)
#'
#'#Call:
#'lineup_vec <- gen_linevec(lineup_table, 3)
#'@importFrom purrr map2
#'@export

gen_linevec <- function (lineup_table, k){
  line_index <- 1:(length(lineup_table))
  map2(line_index, lineup_table, rep_index) %>% unlist
}
