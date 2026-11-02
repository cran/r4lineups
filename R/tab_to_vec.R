#'Lineup vector
#'
#'A function that takes a table of lineup choices and generates a vector of raw
#'lineup data
#'
#'@param lineup_tab A table of lineup choices
#'@param k Nominal size (i.e.,total number of lineup members).
#'                Must be declared by user
#'@return Returns a vector of lineup choices
#'@examples
#'#Data:
#'lineup_vec <- round(runif(100, 1, 6))
#'lineup_tab <- table(lineup_vec)
#'
#'#Call:
#'lineup_vec <- gen_linevec(lineup_tab, 6)
#'@importFrom purrr map2
#'@export

gen_linevec <- function (lineup_tab, k){
  datacheck3(lineup_tab, k)
  counts <- as.numeric(lineup_tab)
  if (any(counts != floor(counts))) stop("Choice counts must be whole numbers.", call. = FALSE)
  positions <- if (is.null(names(lineup_tab))) seq_len(k) else as.integer(names(lineup_tab))
  sort(rep(positions, counts))
}
