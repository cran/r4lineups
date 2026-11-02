#Helper function

#Checks that data have been entered into the function correctly
#Many functions require that the data is formattted as a vector of values,
#rather than a df or as part of a matrix.

#If data is passed to the function in matrix/df format, typecheck will take the column in
#question and convert it from a df into a vector of values.
typecheck <- function(df) {
  if (is.data.frame(df) || is.matrix(df)) {
    if (ncol(df) != 1L) {
      stop("Lineup data must be a vector or a one-column object.", call. = FALSE)
    }
    df <- if (is.matrix(df)) df[, 1L] else df[[1L]]
  }

  if (is.factor(df)) {
    df <- as.character(df)
  }

  if (is.character(df)) {
    numeric_df <- suppressWarnings(as.numeric(df))
    if (anyNA(numeric_df)) {
      stop("Lineup choices must be numeric member positions.", call. = FALSE)
    }
    df <- numeric_df
  }

  if (!is.numeric(df) || length(df) == 0L || anyNA(df) || any(!is.finite(df))) {
    stop("Lineup choices must be a non-empty finite numeric vector.", call. = FALSE)
  }

  as.numeric(df)
}
