datacheck4 <- function(pos_list, k){
  if (!is.list(pos_list)) {
    pos_list <- as.list(pos_list)
  }
  if (length(pos_list) != length(k)) {
    stop("pos_list and k must describe the same number of lineup pairs.", call. = FALSE)
  }
  for (i in seq_along(pos_list)) {
    positions <- pos_list[[i]]
    if (!is.numeric(positions) || !length(positions) %in% c(1L, 2L) ||
        anyNA(positions) || any(positions != as.integer(positions)) ||
        any(positions < 1 | positions > k[[i]])) {
      stop("Each pos_list element must contain one suspect position (shared by TP/TA) or two positions (TP, TA), each between 1 and k.",
           call. = FALSE)
    }
  }
  invisible(TRUE)
}
