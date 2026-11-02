#' Plot Methods for ROC and CAC Objects
#'
#' \code{plot()} methods for the objects returned by the ROC and CAC functions,
#' so that \code{plot(x)} works directly rather than requiring the user to know
#' which plot builder matches which object.
#'
#' @details
#' Each ROC/CAC analysis comes in two forms. The \code{*_data()} function
#' computes and returns the data only; the function without the suffix wraps it,
#' builds the plot, and returns a classed object with the plot stored in
#' \code{$plot}:
#'
#' \itemize{
#'   \item \code{\link{make_rocdata}} / \code{\link{make_roc}}
#'   \item \code{\link{make_cacdata}} / \code{\link{make_cac}}
#'   \item \code{\link{make_fullroc_data}} / \code{\link{make_fullroc}}
#' }
#'
#' These methods accept either form. When a plot has already been built it is
#' returned as-is; otherwise it is constructed with the matching builder
#' (\code{\link{make_roc_gg}}, \code{\link{make_cac_gg}} or
#' \code{\link{plot_fullroc}}).
#'
#' The value is an ordinary ggplot object, so it can be modified with the usual
#' \code{+} syntax, e.g. \code{plot(x) + ggplot2::theme_minimal()}.
#'
#' @param x An object returned by one of the ROC or CAC functions listed above.
#' @param ... Additional arguments passed to the underlying plot builder.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' data(lineup_example)
#' plot(make_rocdata(lineup_example))
#' plot(make_cacdata(lineup_example))
#'
#' @name plot_roc_cac
NULL


#' @rdname plot_roc_cac
#' @export
plot.lineup_roc_data <- function(x, ...) {
  make_roc_gg(x, ...)
}


#' @rdname plot_roc_cac
#' @export
plot.lineup_roc <- function(x, ...) {
  if (!is.null(x$plot) && length(list(...)) == 0L) {
    return(x$plot)
  }
  make_roc_gg(x, ...)
}


#' @rdname plot_roc_cac
#' @export
plot.lineup_cac_data <- function(x, ...) {
  make_cac_gg(x, ...)
}


#' @rdname plot_roc_cac
#' @export
plot.lineup_cac <- function(x, ...) {
  if (!is.null(x$plot) && length(list(...)) == 0L) {
    return(x$plot)
  }
  make_cac_gg(x, ...)
}


#' @rdname plot_roc_cac
#' @export
plot.lineup_fullroc_data <- function(x, ...) {
  plot_fullroc(x, ...)
}


#' @rdname plot_roc_cac
#' @export
plot.lineup_fullroc <- function(x, ...) {
  if (!is.null(x$plot) && length(list(...)) == 0L) {
    return(x$plot)
  }
  plot_fullroc(x, ...)
}
