# Face similarity with deepface embeddings. Local only: needs Python.

face_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 330,
      fileInput(ns("target"), "Target (suspect) image", accept = c(".jpg", ".jpeg", ".png")),
      fileInput(ns("foils"), "Foil images", multiple = TRUE, accept = c(".jpg", ".jpeg", ".png")),
      selectInput(ns("model"), "Embedding model", choices = r4lineups::available_models(),
                  selected = "ArcFace"),
      selectInput(ns("detector"), "Face detector", choices = r4lineups::available_detectors(),
                  selected = "retinaface"),
      numericInput(ns("resize"), "Resize width before embedding (px, 0 = none)", 200, min = 0, step = 10),
      run_button(ns("run"), "Compute similarity"),
      actionButton(ns("run_matrix"), "Compute pairwise matrix", class = "w-100"),
      tags$hr(),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "Python setup",
          helpText("Face similarity uses the Python package deepface through reticulate."),
          actionButton(ns("check"), "Check Python dependencies", class = "btn-sm w-100 mb-2"),
          bslib::input_switch(ns("confirm"), "I understand this installs Python packages"),
          actionButton(ns("install"), "Install Python dependencies", class = "btn-sm w-100")
        )
      )
    ),
    verbatimTextOutput(ns("status")),
    bslib::card(
      bslib::card_header("Lineup"),
      imageOutput(ns("lineup"), height = "auto")
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      plot_card(ns, "sim_plot", "Similarity of each foil to the target"),
      table_card(ns, "sim_table", "Similarity table")
    ),
    table_card(ns, "matrix", "Pairwise similarity matrix")
  )
}

face_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    status <- reactiveVal("Upload a target and at least one foil, then click 'Compute similarity'.")
    output$status <- renderText(status())

    observeEvent(input$check, {
      status(paste(utils::capture.output(
        tryCatch(r4lineups::check_python_deps(verbose = TRUE),
                 error = function(e) cat("Check failed:", conditionMessage(e)))
      ), collapse = "\n"))
    })
    observeEvent(input$install, {
      if (!isTRUE(input$confirm)) {
        status("Turn on the confirmation switch before installing.")
        return()
      }
      status("Installing Python dependencies; this can take several minutes...")
      status(tryCatch({
        r4lineups::install_r4lineups_python()
        "Install complete. Click 'Check Python dependencies' to verify."
      }, error = function(e) paste("Install failed:", conditionMessage(e))))
    })

    paths <- reactive({
      req(input$target, input$foils)
      width <- input$resize
      foil_names <- tools::file_path_sans_ext(input$foils$name)
      prep <- function(path, name) {
        if (is.na(width) || width <= 0) return(path)
        out <- file.path(tempdir(), paste0("r4l_", name, "_", width, ".jpg"))
        img <- magick::image_scale(magick::image_read(path), paste0(width, "x"))
        magick::image_write(img, out, format = "jpg")
        out
      }
      list(target = prep(input$target$datapath, "target"),
           foils = unname(mapply(prep, input$foils$datapath, paste0("foil_", foil_names))),
           foil_names = foil_names)
    })

    similarity <- reactive({
      p <- paths()
      withProgress(message = "Extracting embeddings", value = 0.3, {
        capture_notes(r4lineups::lineup_similarity(p$target, p$foils, model = input$model,
                                                   detector = input$detector,
                                                   foil_names = p$foil_names))
      })
    }) |>
      bindEvent(input$run)

    pairwise <- reactive({
      p <- paths()
      withProgress(message = "Computing pairwise similarities", value = 0.3, {
        m <- capture_notes(r4lineups::lineup_pairwise_matrix(p$target, p$foils, model = input$model,
                                                             detector = input$detector))$value
        as.data.frame(m)
      })
    }) |>
      bindEvent(input$run_matrix)

    output$lineup <- renderImage({
      p <- paths()
      img <- r4lineups::display_lineup(p$target, p$foils, ncol = 4, scale = 140)
      out <- tempfile(fileext = ".png")
      magick::image_write(img, out, format = "png")
      list(src = out, contentType = "image/png", width = "100%")
    }, deleteFile = TRUE)

    sim_table <- reactive({
      res <- similarity()$value
      res[, intersect(c("foil_name", "distance", "similarity", "rank"), names(res))]
    })
    serve_table(output, "sim_table", sim_table, "face_similarity.csv")
    serve_plot(output, "sim_plot", function() r4lineups::plot_lineup_similarity(similarity()$value),
               "face_similarity.png")
    output$matrix <- DT::renderDT(dt_table(cbind(face = rownames(pairwise()), pairwise())))
    output$matrix_csv <- downloadHandler(
      filename = function() "face_similarity_matrix.csv",
      content = function(file) utils::write.csv(pairwise(), file)
    )
  })
}
