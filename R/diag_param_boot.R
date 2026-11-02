diag_param_boot <- function(lineup_list){
  pos_list <- list(NULL)
  for (i in seq_along(lineup_list)){
    pos_list[[i]]= c(sort(unique(lineup_list[[i]])))
  }

  diagdf <- as.data.frame(matrix(ncol = 2,
                                  nrow = length(lineup_list)))



  for (i in seq_along(lineup_list)){
    diagdf[i,1]= sum(lineup_list[[i]] == pos_list[[i]])
    diagdf[i,2] = sum(lineup_list[[i]] != pos_list[[i]])
     diagdf= as.data.frame(sapply(diagdf, as.numeric))
  }
  return(diagdf)

}
