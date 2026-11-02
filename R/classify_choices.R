#' Classify Raw Lineup Choices into Identification Outcomes
#'
#' Derives an identification outcome — suspect identification, foil
#' identification, or rejection — from the raw record of which lineup member
#' each witness actually chose.
#'
#' @param data A data frame of lineup trials, one row per witness.
#' @param member_col Name of the column holding the position of the lineup
#'   member chosen, with \code{0} (or \code{NA}) denoting a rejection. Defaults
#'   to \code{"lineup_member"}.
#' @param suspect_col Either the name of a column holding the suspect's position
#'   for each trial, or a single number giving a suspect position common to all
#'   trials. Defaults to \code{"suspect_pos"}.
#' @param into Name of the column to write the derived outcome into. Defaults to
#'   \code{"identification"}.
#' @param k Optional lineup size. When supplied, choices are checked to lie in
#'   \code{0:k} and the positions of any null foils are attached to the result
#'   as the \code{"null_foils"} attribute.
#' @param overwrite Logical. Whether to overwrite \code{into} if it already
#'   exists. Defaults to \code{FALSE}, which raises an error instead.
#'
#' @details
#' Lineup data should record \emph{which member} a witness chose, not merely
#' whether the choice happened to fall on the suspect. Foils are not
#' interchangeable: they differ in how plausible they are, and a foil that no
#' witness ever chooses is not a functioning alternative to the suspect. Storing
#' only a collapsed "suspect / filler / reject" label discards the information
#' needed to see that.
#'
#' This function treats the collapsed label as \emph{derived}. Raw data keeps
#' the chosen position; \code{classify_lineup_choices()} produces the label on
#' demand, so both are available and the two cannot drift apart.
#'
#' The outcome is \code{"suspect"} when the chosen member is the suspect,
#' \code{"reject"} when no member was chosen, and \code{"filler"} otherwise.
#' These are the labels used throughout the package, so the result can be passed
#' straight to \code{\link{make_roc}}, \code{\link{make_cac}} and the other
#' analysis functions.
#'
#' @return \code{data} with the derived outcome column added. If \code{k} was
#'   supplied, the \code{"null_foils"} attribute gives the positions that no
#'   witness chose.
#'
#' @seealso \code{\link{lineup_table}} for a tabulation that shows null foils,
#'   and \code{\link{simulate_lineup_data}}, which records \code{lineup_member}
#'   directly.
#'
#' @references Malpass, R. S. (1981). Effective size and defendant bias in
#'            eyewitness identification lineups. \emph{Law and Human Behavior,
#'            5}(4), 299-309.
#'
#'            Tredoux, C. G. (1998). Statistical inference on measures of lineup
#'            fairness. \emph{Law and Human Behavior, 22}(2), 217-237.
#'
#' @examples
#' # Raw choices from an 8-person lineup in which the suspect stands at
#' # position 3; 0 records a rejection
#' trials <- data.frame(
#'   lineup_member = c(3, 0, 5, 3, 1, 0, 3, 7),
#'   suspect_pos   = 3
#' )
#'
#' classify_lineup_choices(trials, k = 8)
#'
#' # A suspect position common to every trial can be given as a number
#' classify_lineup_choices(
#'   data.frame(lineup_member = c(2, 0, 4)),
#'   suspect_col = 2
#' )
#'
#' @export
classify_lineup_choices <- function(data,
                                    member_col = "lineup_member",
                                    suspect_col = "suspect_pos",
                                    into = "identification",
                                    k = NULL,
                                    overwrite = FALSE) {

  if (!is.data.frame(data)) {
    stop("data must be a data frame.", call. = FALSE)
  }
  if (nrow(data) == 0L) {
    stop("data has no rows.", call. = FALSE)
  }
  if (!is.character(member_col) || length(member_col) != 1L) {
    stop("member_col must be a single column name.", call. = FALSE)
  }
  if (!is.character(into) || length(into) != 1L) {
    stop("into must be a single column name.", call. = FALSE)
  }
  if (!member_col %in% names(data)) {
    stop("Column '", member_col, "' not found in data. It should record which ",
         "lineup member was chosen, with 0 for a rejection.", call. = FALSE)
  }
  if (into %in% names(data) && !isTRUE(overwrite)) {
    stop("Column '", into, "' already exists. Pass overwrite = TRUE to replace it.",
         call. = FALSE)
  }

  members <- data[[member_col]]
  if (!is.numeric(members)) {
    stop("Column '", member_col, "' must be numeric: the position of the lineup ",
         "member chosen, or 0 for a rejection.", call. = FALSE)
  }
  # NA and 0 both denote "chose nobody"; normalising here keeps the comparison
  # against suspect positions free of NA propagation.
  if (any(!is.na(members) & (!is.finite(members) | members != floor(members)))) {
    stop("Chosen positions must be whole finite member positions or NA.", call. = FALSE)
  }
  members <- ifelse(is.na(members), 0L, as.integer(members))
  if (any(members < 0L)) {
    stop("Column '", member_col, "' contains negative positions.", call. = FALSE)
  }

  # suspect_col is deliberately dual-purpose: a column when the suspect moves
  # between lineups, a scalar when one position serves every trial.
  if (is.character(suspect_col) && length(suspect_col) == 1L) {
    if (!suspect_col %in% names(data)) {
      stop("Column '", suspect_col, "' not found in data. Supply the suspect ",
           "position as a number instead if it is the same for every trial.",
           call. = FALSE)
    }
    suspects <- data[[suspect_col]]
    if (!is.numeric(suspects)) {
      stop("Column '", suspect_col, "' must be numeric.", call. = FALSE)
    }
  } else if (is.numeric(suspect_col) && length(suspect_col) == 1L &&
             is.finite(suspect_col)) {
    suspects <- rep(suspect_col, nrow(data))
  } else {
    stop("suspect_col must be a single column name or a single suspect position.",
         call. = FALSE)
  }

  if (anyNA(suspects) || any(!is.finite(suspects)) ||
      any(suspects != floor(suspects)) || any(suspects < 1L)) {
    stop("Suspect positions must be present and at least 1.", call. = FALSE)
  }

  if (!is.null(k)) {
    if (length(k) != 1L || !is.numeric(k) || !is.finite(k) ||
        k != as.integer(k) || k < 2L) {
      stop("k must be a single integer of at least 2.", call. = FALSE)
    }
    k <- as.integer(k)
    if (any(members > k)) {
      stop("Column '", member_col, "' contains positions greater than k = ", k,
           ".", call. = FALSE)
    }
    if (any(suspects > k)) {
      stop("Suspect positions greater than k = ", k, ".", call. = FALSE)
    }
  }

  outcome <- ifelse(members == 0L, "reject",
                    ifelse(members == suspects, "suspect", "filler"))

  data[[into]] <- outcome

  if (!is.null(k)) {
    chosen <- members[members > 0L]
    attr(data, "null_foils") <- setdiff(seq_len(k), unique(chosen))
  }

  data
}
