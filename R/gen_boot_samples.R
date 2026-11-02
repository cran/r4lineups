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
#'@importFrom purrr map map_df

gen_boot_samples <- function (lineup_vec, bootno){
  if (!is.numeric(bootno) || length(bootno) != 1L || bootno < 1 ||
      bootno != as.integer(bootno)) {
    stop("bootno must be a positive integer.", call. = FALSE)
  }
  names_a <- rep("sample_",bootno)
  names_b <- as.character(seq_len(bootno))
  x <- purrr::map(seq_len(bootno),
                  ~ .resample_values(lineup_vec))
  names(x) <- paste(names_a,names_b,sep = "")
  lineup_boot_samples <- map_df(x, magrittr::extract, c(seq_along(lineup_vec)))
}
