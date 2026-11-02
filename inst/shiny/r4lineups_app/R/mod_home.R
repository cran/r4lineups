# Home: what the app covers, data formats, version and citation.

docs_url <- "https://cgtza2.github.io/r4lineups/"

home_ui <- function(id) {
  ns <- NS(id)
  article <- function(file, label) tags$a(href = paste0(docs_url, "articles/", file, ".html"),
                                          target = "_blank", label)
  bslib::layout_columns(
    col_widths = c(6, 6),
    bslib::card(
      bslib::card_header("r4lineups"),
      tags$p(
        "Interactive analyses of eyewitness lineups. Every number shown here is computed by an exported ",
        "function of the r4lineups package, named on each tab, so results can be reproduced in R."
      ),
      tags$h6("Mock-witness data (a vector of chosen positions, or counts per position)"),
      tags$ul(
        tags$li(tags$strong("Lineup fairness: "), "suspect bias against chance, member proportions, ",
                "effective size (Tredoux's E', Malpass), functional size, and diagnosticity ratios ",
                "with a homogeneity test. ", article("Vignette", "Vignette")),
        tags$li(tags$strong("Bayesian: "), "posterior distributions for E', functional size, ",
                "diagnosticity, calibration and SDT comparisons. ", article("bayesian_inference", "Vignette"))
      ),
      tags$h6("Witness data with confidence (load on the Data tab)"),
      tags$ul(
        tags$li(tags$strong("Multi-d': "), "Lee and Penrod's (2019) signal detection indices, including ",
                "d'(IFa) as a resultant measure of lineup bias. ", article("multid_model", "Vignette")),
        tags$li(tags$strong("ROC & CAC: "), "partial ROC, CAC, full ROC and pAUC comparison. ",
                article("fullroc_analysis", "Vignette")),
        tags$li(tags$strong("Calibration & decision: "), "calibration, ANRI, DPP, expected utility, ",
                "Bayesian curves, BREE and RAC. ", article("calibration_decision_analysis", "Vignette")),
        tags$li(tags$strong("EIG & PPV: "), "expected information gain and PPV ranges. ",
                article("information_error_rate_analysis", "Vignette"))
      ),
      if (face_tab_available()) tags$p(
        tags$strong("Face similarity "), "(local only) compares face images with deepface embeddings."
      ),
      tags$p(
        "Analyses run when you click ", tags$em("Run analysis"),
        "; tabs that use the Data tab also rerun when the data change. Tables and plots can be downloaded."
      )
    ),
    bslib::card(
      bslib::card_header("About"),
      tags$p("Package version ", textOutput(ns("version"), inline = TRUE)),
      tags$p(tags$a(href = docs_url, target = "_blank", "Documentation"), " · ",
             tags$a(href = "https://github.com/CGTZA2/r4lineups", target = "_blank", "Source"), " · ",
             tags$a(href = "https://github.com/CGTZA2/r4lineups/issues", target = "_blank", "Report a problem")),
      tags$h6("Citation"),
      uiOutput(ns("citation")),
      tags$h6("Run locally"),
      tags$pre("install.packages(\"r4lineups\")\nr4lineups::run_r4lineups_app()"),
      if (is_webr()) tags$p(class = "small text-muted",
                            "This copy runs entirely in your browser: data you load are not sent to a server.")
    )
  )
}

home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$version <- renderText(as.character(utils::packageVersion("r4lineups")))
    output$citation <- renderUI({
      cit <- tryCatch({
        file <- system.file("CITATION", package = "r4lineups")
        format(utils::readCitationFile(file, meta = list(Encoding = "UTF-8")), style = "html")
      }, error = function(e) character())
      tagList(lapply(cit, function(x) tags$p(class = "small", HTML(x))))
    })
  })
}
