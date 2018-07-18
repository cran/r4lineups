#'Rep index
#'
#'Helper function that repeats a number k times
#'@details Can be used to create vectors of lineup data
#'@param index Number to be repeated
#'@param num Number of times to repeat index
#'@examples
#'rep_index(1, 10)
#'@export

rep_index <- function(index,num){
  rep(index, num)
}
