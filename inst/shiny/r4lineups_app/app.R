# r4lineups app. Modules live in R/, which Shiny sources automatically.

library(shiny)
# In the browser (Shinylive/webR) the webR repository carries an old r4lineups,
# so the current version is installed from r-universe.
if (identical(R.version$os, "emscripten") &&
    (!requireNamespace("r4lineups", quietly = TRUE) ||
     utils::packageVersion("r4lineups") < "2.3.0")) {
  webr::install("r4lineups", repos = c("https://cgtza2.r-universe.dev", "https://repo.r-wasm.org"))
}
library(r4lineups)

ui <- bslib::page_navbar(
  title = "r4lineups",
  id = "nav",
  theme = bslib::bs_theme(version = 5, primary = "#2c5d8a"),
  fillable = FALSE,
  bslib::nav_panel("Home", icon = icon("house"), home_ui("home")),
  bslib::nav_panel("Data", icon = icon("table"), data_ui("data")),
  bslib::nav_menu(
    "Lineup fairness",
    bslib::nav_panel("Bias & size", fairness_ui("fairness")),
    bslib::nav_panel("Diagnosticity", diagnosticity_ui("diag"))
  ),
  bslib::nav_panel("Multi-d'", multid_ui("multid")),
  bslib::nav_panel("ROC & CAC", roc_ui("roc")),
  bslib::nav_panel("Calibration & decision", calibration_ui("calibration")),
  bslib::nav_panel("EIG & PPV", eig_ui("eig")),
  bslib::nav_panel("Bayesian", bayes_ui("bayes")),
  if (face_tab_available()) bslib::nav_panel("Face similarity", face_ui("face")),
  bslib::nav_spacer(),
  bslib::nav_item(tags$a(icon("book"), "Docs", href = docs_url, target = "_blank"))
)

server <- function(input, output, session) {
  home_server("home")
  data <- data_server("data")
  fairness_server("fairness")
  diagnosticity_server("diag")
  multid_server("multid", data)
  roc_server("roc", data)
  calibration_server("calibration", data)
  eig_server("eig", data)
  bayes_server("bayes", data)
  if (face_tab_available()) face_server("face")
}

shinyApp(ui, server)
