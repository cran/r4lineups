datacheck5 <- function(lineup_pres_list, k){
  pos_list <- list(NULL)
  for (i in 1:length(lineup_pres_list)){
    pos_list[[i]]= c(sort(unique(lineup_pres_list[[i]])))
  }

  for (i in 1:length(pos_list)){

    pos_list
    if (length(pos_list[[i]])== k[[i]]){
      pos_list = pos_list
    }
    else{
      stop("User-declared nominal size does not match observed nominal size. Please
           check vector of target positions.")
    }
    }
  }
