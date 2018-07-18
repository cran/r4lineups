#Helper function

#Checks that data have been entered into the function correctly
#Many functions require that the data is formattted as a vector of values,
#rather than a df or as part of a matrix.

#If data is passed to the function in matrix/df format, typecheck will take the column in
#question and convert it from a df into a vector of values.
typecheck <- function(df){
  x <- typeof(df)

  if (x == "double"){
    df = df
  }
  else{
    df <- df[[1]]}
  return(df)
}
