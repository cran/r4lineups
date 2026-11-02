#' Run the r4lineups Shiny app
#'
#' Launches the bundled Shiny app. It covers lineup fairness from mock-witness
#' choices (bias, effective and functional size, diagnosticity), Bayesian
#' posteriors, Lee and Penrod's Multi-d' model, ROC/CAC/full ROC with pAUC
#' comparison, calibration and decision analyses (ANRI, DPP, expected utility,
#' Bayesian and BREE curves, RAC), expected information gain and PPV ranges,
#' and, when Python is available, face similarity. Every result is computed by
#' an exported r4lineups function.
#'
#' The app needs the suggested packages \pkg{shiny}, \pkg{bslib} and \pkg{DT}.
#'
#' @return None. Launches a Shiny app.
#' @examples
#' \dontrun{
#' run_r4lineups_app()
#' }
#' @export
run_r4lineups_app <- function() {
  needed <- c("shiny", "bslib", "DT")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("The app needs these packages: ", paste(missing, collapse = ", "),
         ". Install them with install.packages(c(",
         paste0("\"", missing, "\"", collapse = ", "), ")).", call. = FALSE)
  }
  app_dir <- system.file("shiny/r4lineups_app", package = "r4lineups")
  if (app_dir == "") {
    stop("Shiny app directory not found. Was the package installed with inst/shiny?", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
