chi_diag.boot <- function(df, d=d){
  q <- df[d] %>% sum((df[2,]-log(d_bar(df))/(df[1,])))
  return(q)
}
