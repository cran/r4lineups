datacheck4 <- function(pos_list, k){

  for (i in 1:length(pos_list)){

    if (length(table(pos_list[i]))== k[[i]]){
      pos_list = pos_list
    }
    else{
      stop("User-declared nominal size does not match observed nominal size. Please
         check vector of target positions.")
    }
  }
}
