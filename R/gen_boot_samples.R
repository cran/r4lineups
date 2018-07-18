#'Bootstrap resampling
#'
#'Function for generating bootstrapped samples from 1 vector of lineup data
#'@param lineup_vec A numeric vectors of lineup choices
#'@param bootno Number of bootstrap samples
#'@return A dataframe of bootstrapped lineup data
#'@examples
#'#Data:
#'lineup_vec <- round(runif(100,1,6))
#'bootno <- 1000
#'
#'#Call:
#'bootdf <- gen_boot_samples(lineup_vec, bootno)
#'
#'@export
#'@importFrom magrittr %>% extract
#'@importFrom purrr rerun map_df

gen_boot_samples <- function (lineup_vec, bootno){
  names_a <- rep("sample_",bootno)
  names_b <- as.character(1:bootno)
  bootno %>% rerun(sample(lineup_vec, length(lineup_vec), replace = TRUE))  -> x
  names(x) <- paste(names_a,names_b,sep = "")
  lineup_boot_samples <- map_df(x, magrittr::extract, c(1:length(lineup_vec)))
}

