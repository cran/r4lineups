# Expected information gain (Starns et al.) and PPV ranges (Fitzgerald et al.).

eig_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      uiOutput(ns("banner")),
      sliderInput(ns("prior"), "Prior probability of guilt", min = 0.05, max = 0.95,
                  value = 0.5, step = 0.05),
      run_button(ns("run"))
    ),
    uiOutput(ns("notes")),
    bslib::layout_column_wrap(
      width = 1 / 4, fill = FALSE,
      bslib::value_box("Expected information gain", textOutput(ns("vb_eig")), theme = "primary"),
      bslib::value_box("PPV (nominal size)", textOutput(ns("vb_ppv_nom")), theme = "light"),
      bslib::value_box("PPV (effective size)", textOutput(ns("vb_ppv_eff")), theme = "light"),
      bslib::value_box("PPV (no correction)", textOutput(ns("vb_ppv_none")), theme = "light")
    ),
    bslib::navset_card_underline(
      id = ns("tabs"),
      bslib::nav_panel(
        "Information gain",
        bslib::layout_columns(
          col_widths = c(6, 6),
          plot_card(ns, "eig_plot", "Information gain by response"),
          plot_card(ns, "eig_post", "Posterior probability of guilt")
        ),
        table_card(ns, "eig_table", "Responses")
      ),
      bslib::nav_panel(
        "PPV range",
        bslib::layout_columns(
          col_widths = c(6, 6),
          plot_card(ns, "ppv_plot", "PPV by confidence"),
          plot_card(ns, "ppv_eff", "Effective size by confidence")
        ),
        table_card(ns, "ppv_table", "PPV range by confidence")
      )
    )
  )
}

eig_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$banner <- renderUI(data_banner(data()))

    result <- reactive({
      d <- data()
      run <- capture_notes(list(
        eig = r4lineups::make_eig(d$data, prior_guilt = input$prior, confidence_bins = d$bins,
                                  show_plot = FALSE, lineup_size = d$lineup_size,
                                  innocent_suspect = d$innocent_suspect),
        ppv = r4lineups::make_ppv_range(d$data, lineup_size = d$lineup_size,
                                        innocent_suspect = d$innocent_suspect,
                                        confidence_bins = d$bins, show_plots = FALSE)
      ))
      c(run$value, list(notes = run$notes))
    }) |>
      bindEvent(input$run, data(), ignoreNULL = FALSE)

    output$notes <- renderUI(notes_ui(result()$notes))
    output$vb_eig <- renderText(fmt_num(result()$eig$eig))
    output$vb_ppv_nom <- renderText(fmt_num(result()$ppv$ppv_nominal$overall_ppv))
    output$vb_ppv_eff <- renderText(fmt_num(result()$ppv$ppv_effective$overall_ppv))
    output$vb_ppv_none <- renderText(fmt_num(result()$ppv$ppv_none$overall_ppv))

    serve_plot(output, "eig_plot", function() r4lineups::plot_eig(result()$eig), "eig.png")
    serve_plot(output, "eig_post", function() r4lineups::plot_eig_posteriors(result()$eig),
               "eig_posteriors.png")
    serve_table(output, "eig_table", function() result()$eig$response_data, "eig_responses.csv")
    serve_plot(output, "ppv_plot", function() r4lineups::plot_ppv_range(result()$ppv), "ppv_range.png")
    serve_plot(output, "ppv_eff", function() r4lineups::plot_effective_size_conf(result()$ppv),
               "ppv_effective_size.png")
    serve_table(output, "ppv_table", function() result()$ppv$ppv_range_data, "ppv_range.csv")

    result
  })
}
