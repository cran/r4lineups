# ROC, CAC and full ROC, with a pAUC comparison when two groups are chosen.

roc_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      uiOutput(ns("banner")),
      selectInput(ns("order"), "Full ROC category order",
                  choices = c("A priori (for inference)" = "apriori",
                              "By diagnosticity" = "diagnosticity")),
      bslib::layout_columns(
        numericInput(ns("n_boot"), "pAUC bootstrap R", value = 1000, min = 100, step = 100),
        numericInput(ns("seed"), "Seed", value = 2026, step = 1)
      ),
      helpText("The pAUC comparison runs when two groups are chosen on the Data tab."),
      run_button(ns("run"))
    ),
    uiOutput(ns("notes")),
    uiOutput(ns("scope")),
    bslib::layout_column_wrap(
      width = 1 / 3, fill = FALSE,
      bslib::value_box("Partial AUC", textOutput(ns("vb_pauc")), theme = "primary"),
      bslib::value_box("CAC: overall suspect-ID accuracy", textOutput(ns("vb_cac")), theme = "light"),
      bslib::value_box("Full ROC AUC", textOutput(ns("vb_auc")), theme = "light")
    ),
    bslib::navset_card_underline(
      id = ns("tabs"),
      bslib::nav_panel(
        "ROC",
        bslib::layout_columns(
          col_widths = c(6, 6),
          plot_card(ns, "roc_plot", "ROC (suspect identifications)"),
          table_card(ns, "roc_table", "ROC points")
        ),
        table_card(ns, "pauc_table", "pAUC comparison")
      ),
      bslib::nav_panel(
        "CAC",
        bslib::layout_columns(
          col_widths = c(6, 6),
          plot_card(ns, "cac_plot", "Confidence-accuracy characteristic"),
          table_card(ns, "cac_table", "CAC by confidence bin")
        )
      ),
      bslib::nav_panel(
        "Full ROC",
        bslib::layout_columns(
          col_widths = c(6, 6),
          plot_card(ns, "fullroc_plot", "Full ROC (Smith & Yang, 2020)"),
          table_card(ns, "fullroc_table", "Full ROC categories")
        )
      )
    )
  )
}

roc_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$banner <- renderUI(data_banner(data()))

    result <- reactive({
      d <- data()
      df <- d$data
      run <- capture_notes({
        roc <- r4lineups::make_rocdata(df, lineup_size = d$lineup_size,
                                       innocent_suspect = d$innocent_suspect)
        cac <- r4lineups::make_cacdata(df, lineup_size = d$lineup_size,
                                       innocent_suspect = d$innocent_suspect, confidence_bins = d$bins)
        fullroc <- r4lineups::make_fullroc_data(df, conf_bins = d$bins, order = input$order,
                                                lineup_size = d$lineup_size,
                                                innocent_suspect = d$innocent_suspect)
        pauc <- if (!is.null(d$groups)) {
          r4lineups::compare_pauc(d$groups$data[[1]], d$groups$data[[2]],
                                  lineup_size = d$lineup_size,
                                  innocent_suspect = d$innocent_suspect,
                                  n_bootstrap = as.integer(input$n_boot), seed = input$seed,
                                  label1 = d$groups$labels[1], label2 = d$groups$labels[2])
        }
        list(roc = roc, cac = cac, fullroc = fullroc, pauc = pauc)
      })
      c(run$value, list(notes = run$notes))
    }) |>
      bindEvent(input$run, data(), ignoreNULL = FALSE)

    output$notes <- renderUI(notes_ui(result()$notes))
    output$scope <- renderUI(group_scope_note(data(), paste(
      "The ROC plot and pAUC table compare the groups;",
      "the summary boxes, CAC and full ROC use all rows.")))
    output$vb_pauc <- renderText(fmt_num(result()$roc$pauc, 4))
    output$vb_cac <- renderText(fmt_num(result()$cac$overall_accuracy))
    output$vb_auc <- renderText(fmt_num(result()$fullroc$auc))

    serve_plot(output, "roc_plot", function() {
      r <- result()
      if (is.null(r$pauc)) {
        return(r4lineups::make_roc_gg(r$roc))
      }
      pts <- rbind(cbind(group = r$pauc$label1, r$pauc$roc1$roc_data),
                   cbind(group = r$pauc$label2, r$pauc$roc2$roc_data))
      ggplot2::ggplot(pts, ggplot2::aes(false_id_rate, correct_id_rate, colour = group)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::geom_vline(xintercept = r$pauc$max_false_id_rate, linetype = "dotted") +
        ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
        ggplot2::coord_cartesian(xlim = c(0, max(pts$false_id_rate) * 1.1), ylim = c(0, 1)) +
        ggplot2::labs(x = "False ID rate (innocent suspect)", y = "Correct ID rate (guilty suspect)",
                      colour = NULL, caption = "Dotted line: pAUC cut-off.") +
        ggplot2::theme_bw(base_size = 13)
    }, "roc.png")
    serve_table(output, "roc_table", function() result()$roc$roc_data, "roc_points.csv")
    serve_table(output, "pauc_table", function() {
      p <- result()$pauc
      validate(need(!is.null(p), "Choose two groups on the Data tab to compare pAUC."))
      data.frame(
        quantity = c(paste("pAUC", p$label1), paste("pAUC", p$label2), "Difference",
                     "SE of difference", sprintf("%d%% CI of difference", round(100 * p$conf_level)),
                     "z", "p", "False ID cut-off", "Bootstrap R"),
        value = c(fmt_num(p$pauc1, 4), fmt_num(p$pauc2, 4), fmt_num(p$pauc_diff, 4),
                  fmt_num(p$se_diff, 4), fmt_ci(p$ci_diff, 4), fmt_num(p$z_score),
                  format.pval(p$p_value, digits = 3), fmt_num(p$max_false_id_rate), p$n_bootstrap)
      )
    }, "pauc_comparison.csv")

    serve_plot(output, "cac_plot", function() r4lineups::make_cac_gg(result()$cac), "cac.png")
    serve_table(output, "cac_table", function() result()$cac$cac_data, "cac.csv")
    serve_plot(output, "fullroc_plot", function() r4lineups::plot_fullroc(result()$fullroc),
               "full_roc.png")
    serve_table(output, "fullroc_table", function() result()$fullroc$diagnosticity_table,
                "full_roc_categories.csv")

    result
  })
}
