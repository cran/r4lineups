#Helper function for bootstrapping average diagnosticity ratio for k lineup pairs
d_bar.boot <- function(df, d=d){
  numerator   <- df[d] %>% sum(df[3,]*df[2,])
  denominator <- df[d] %>% sum(df[3,])
  d_bar1       <- exp(numerator/denominator)
  return(d_bar1)
}
