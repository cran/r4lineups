#'Bootstrapped resampling
#'
#'Function for generating bootstrapped samples for of k vectors of lineup choices
#'@param lineup_list A list containing k vectors of lineup choices for k lineups,
#'                        in which the target was either absent or present
#'@param bootno Number of bootstrap samples
#'@return A list of bootstrapped lineup data
#'         Length of list = no. of boostrap sample draws
#'@examples
#'#Data:
#'A <-  round(runif(100,1,6))
#'B <-  round(runif(70,1,5))
#'C <-  round(runif(20,1,4))
#'linelist <- list(A, B, C)
#'rm(A, B, C)
#'
#'bootno <- 1000
#'
#'#Call:
#'bootdata <- gen_boot_samples_list(linelist, bootno)
#'
#'@export

gen_boot_samples_list <- function(lineup_list, bootno){
  lineup_boot_samples <- NULL
  for (i in 1:length(lineup_list)){

    names_a <- rep("sample_",bootno)
    names_b <- as.character(1:bootno)
    bootno %>% rerun(sample(lineup_list[[i]], length(lineup_list[[i]]), replace = TRUE))  -> x
    names(x) <- paste(names_a,names_b,sep = "")
    lineup_boot_samples[[i]] <- map_df(x, magrittr::extract, c(1:length(lineup_list[[i]])))
  }
  return(lineup_boot_samples)
}
