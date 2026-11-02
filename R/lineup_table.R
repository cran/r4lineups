#' Tabulate Lineup Choices, Including Null Foils
#'
#' Builds a table of lineup choices with one entry for every lineup member,
#' including members that no mock witness chose.
#'
#' @param lineup_vec A numeric vector of lineup choices.
#' @param k Number of members in the lineup (suspect plus foils). Must be
#'   specified by the user (scalar).
#' @param suspect_pos Optional position of the nominated suspect, marked in the
#'   printed output. Defaults to \code{NULL}, for lineups with no pre-specified
#'   suspect.
#'
#' @details
#' Calling \code{table()} directly on a vector of lineup choices tabulates only
#' the positions that were actually chosen. A lineup member that no witness
#' selected — a \emph{null foil} — is silently dropped, so the resulting table has
#' fewer than \code{k} entries and nothing in the output reveals that a member is
#' missing. This is important because a foil nobody would pick is not a
#' functioning alternative to the suspect, one of several things that can make a
#' lineup unfair. A member with no choices in a small sample is not necessarily
#' such a foil.
#'
#' \code{lineup_table()} tabulates over all \code{k} positions, so unchosen
#' members appear explicitly as zero counts, and its print method names them.
#'
#' The measures in this package already handle null foils correctly when given a
#' complete table. \code{esize_m()}, for instance, reindexes counts by position
#' and deducts unchosen members from the adjusted estimate. What
#' \code{lineup_table()} adds is visibility, not a change in the arithmetic.
#'
#' @return An object of class \code{lineup_table}, inheriting from \code{table},
#'   with \code{k} named entries. Because it inherits from \code{table} it can be
#'   passed to \code{\link{esize_m}}, \code{\link{esize_T}} and the other
#'   table-taking functions in this package unchanged.
#'
#' @seealso \code{\link{esize_m}}, \code{\link{esize_T}}, \code{\link{allprop}}
#'
#' @references Malpass, R. S. (1981). Effective size and defendant bias in
#'            eyewitness identification lineups. \emph{Law and Human Behavior, 5}(4), 299-309.
#'
#'            Tredoux, C. G. (1998). Statistical inference on measures of lineup
#'            fairness. \emph{Law and Human Behavior, 22}(2), 217-237.
#'
#' @examples
#' # A lineup in which position 4 was never chosen
#' lineup_vec <- rep(c(5, 3, 2, 1, 8, 6, 7), each = 4)
#'
#' # table() drops the unchosen member: only 7 entries for an 8-person lineup
#' table(lineup_vec)
#'
#' # lineup_table() keeps it, and says so
#' lineup_table(lineup_vec, k = 8, suspect_pos = 3)
#'
#' # Accepted by the effective size functions unchanged
#' esize_m(lineup_table(lineup_vec, k = 8), k = 8)
#'
#' @export
lineup_table <- function(lineup_vec, k, suspect_pos = NULL) {
  lineup_vec <- typecheck(lineup_vec)
  datacheck1(lineup_vec, k)
  k <- as.integer(k)

  if (!is.null(suspect_pos)) {
    if (length(suspect_pos) != 1L || !is.numeric(suspect_pos) ||
        !is.finite(suspect_pos) || suspect_pos != as.integer(suspect_pos) ||
        suspect_pos < 1L || suspect_pos > k) {
      stop("suspect_pos must be a single integer between 1 and k.", call. = FALSE)
    }
    suspect_pos <- as.integer(suspect_pos)
  }

  # Tabulating over an explicit factor of all k levels is what keeps unchosen
  # members in the table; table(lineup_vec) alone would drop them.
  out <- table(factor(as.integer(lineup_vec), levels = seq_len(k)))
  names(dimnames(out)) <- NULL

  attr(out, "k") <- k
  attr(out, "suspect_pos") <- suspect_pos
  class(out) <- c("lineup_table", class(out))
  out
}


#' Print Method for lineup_table Objects
#'
#' @param x A \code{lineup_table} object.
#' @param ... Additional arguments (ignored).
#'
#' @return \code{x}, invisibly.
#'
#' @examples
#' print(lineup_table(rep(c(5, 3, 2, 1, 8, 6, 7), each = 4), k = 8))
#'
#' @export
print.lineup_table <- function(x, ...) {
  k <- attr(x, "k")
  suspect_pos <- attr(x, "suspect_pos")
  counts <- as.integer(x)

  labels <- as.character(seq_len(k))
  if (!is.null(suspect_pos)) {
    labels[suspect_pos] <- paste0(labels[suspect_pos], "*")
  }

  width <- pmax(nchar(labels), nchar(as.character(counts)))
  pad <- function(v) mapply(formatC, as.character(v), width = width,
                            USE.NAMES = FALSE)

  cat("Lineup choices (k = ", k, ", N = ", sum(counts), ")\n\n", sep = "")
  cat(" pos: ", paste(pad(labels), collapse = "  "), "\n", sep = "")
  cat(" cnt: ", paste(pad(counts), collapse = "  "), "\n", sep = "")

  if (!is.null(suspect_pos)) {
    cat("\n * = nominated suspect\n")
  }

  null_foils <- which(counts == 0L)
  if (length(null_foils) > 0L) {
    cat("\n Note: ", length(null_foils),
        if (length(null_foils) == 1L) " null foil (position " else " null foils (positions ",
        paste(null_foils, collapse = ", "),
        ") received no choices.\n", sep = "")
    cat(" Unchosen members reduce effective size; see ?esize_m.\n")
  }

  invisible(x)
}
