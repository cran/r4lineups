# Lee and Penrod's (2019) Multi-d' model, from counts or from the Data tab.

multid_count_inputs <- function(ns, suffix, values) {
  labels <- c(G = "G: guilty suspect", Fp = "Fp: TP filler", Rp = "Rp: TP reject",
              I = "I: innocent suspect", Fa = "Fa: TA filler", Ra = "Ra: TA reject")
  inputs <- lapply(names(labels), function(nm) {
    numericInput(ns(paste0(nm, suffix)), labels[[nm]], value = values[[nm]], min = 0, step = 1)
  })
  do.call(bslib::layout_columns, c(list(col_widths = c(4, 4, 4)), inputs))
}

multid_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 380,
      radioButtons(ns("source"), "Input", inline = TRUE,
                   choices = c("Counts" = "counts", "Data tab" = "data")),
      conditionalPanel(
        sprintf("input['%s'] == 'counts'", ns("source")),
        textInput(ns("label1"), "Condition 1", value = "Simultaneous"),
        multid_count_inputs(ns, "1", c(G = 52, Fp = 25, Rp = 24, I = 28, Fa = 26, Ra = 46)),
        checkboxInput(ns("compare"), "Compare with a second condition", value = FALSE),
        conditionalPanel(
          sprintf("input['%s']", ns("compare")),
          textInput(ns("label2"), "Condition 2", value = "Sequential"),
          multid_count_inputs(ns, "2", c(G = 44, Fp = 19, Rp = 39, I = 15, Fa = 17, Ra = 68))
        ),
        numericInput(ns("lineup_size"), "Lineup size", value = 6, min = 2, step = 1),
        helpText("Example: Lee & Penrod's Table 3, rates per 100 lineups (Steblay et al., 2011).")
      ),
      conditionalPanel(
        sprintf("input['%s'] == 'data'", ns("source")),
        uiOutput(ns("banner")),
        checkboxInput(ns("by_conf"), "Also compute cumulatively by confidence", value = TRUE)
      ),
      tags$hr(),
      selectInput(ns("filler_rate"), "Filler rate",
                  choices = c("Total (Lee & Penrod)" = "total", "Per filler" = "per_filler")),
      selectInput(ns("correction"), "Extreme-rate correction",
                  choices = c("Log-linear" = "loglinear", "Half count" = "half", "None" = "none")),
      bslib::layout_columns(
        numericInput(ns("n_boot"), "Bootstrap R (0 = none)", value = 1000, min = 0, step = 100),
        numericInput(ns("seed"), "Seed", value = 2026, step = 1)
      ),
      run_button(ns("run"))
    ),
    uiOutput(ns("notes")),
    bslib::navset_card_underline(
      id = ns("tabs"),
      bslib::nav_panel(
        "Indices",
        table_card(ns, "indices", "Multi-d' indices")
      ),
      bslib::nav_panel(
        "Plots",
        uiOutput(ns("plot_cond_ui")),
        bslib::layout_columns(
          col_widths = c(6, 6),
          plot_card(ns, "decomp", "Decomposition of d'(GI)"),
          plot_card(ns, "index_plot", "Indices with intervals")
        ),
        plot_card(ns, "conf_plot", "Indices by confidence threshold")
      ),
      bslib::nav_panel(
        "Comparison",
        table_card(ns, "comparison", "Difference between conditions")
      ),
      bslib::nav_panel(
        "About d'(IFa)",
        tags$p(
          "d'(IFa) = z(I) − z(Fa) compares the innocent suspect with the target-absent fillers and is ",
          "Lee and Penrod's index of lineup bias. With the total filler rate, a perfectly fair lineup does ",
          "not give 0: the row ", tags$code("fair_reference"), " is the value a fair lineup would give at ",
          "the observed rejection rate, and ", tags$code("ifa_vs_fair"),
          " is the observed d'(IFa) minus that reference (positive = the innocent suspect draws more ",
          "than a fair share). These two rows are aids to interpretation, not part of Lee and Penrod's model."
        ),
        tags$p("See ", tags$code("vignette(\"multid_model\")"), " and ", tags$code("?make_multid"), ".")
      )
    )
  )
}

multid_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$banner <- renderUI(data_banner(data()))

    counts_for <- function(suffix) {
      vals <- vapply(c("G", "Fp", "Rp", "I", "Fa", "Ra"),
                     function(nm) as.numeric(input[[paste0(nm, suffix)]] %||% NA), numeric(1))
      validate(need(all(is.finite(vals) & vals >= 0), "Counts must be non-negative numbers."))
      vals
    }

    result <- reactive({
      args <- list(filler_rate = input$filler_rate, correction = input$correction,
                   n_bootstrap = as.integer(input$n_boot))
      run <- capture_notes({
        if (identical(input$source, "counts")) {
          fits <- list(do.call(r4lineups::make_multid,
                               c(list(counts = counts_for("1"), lineup_size = input$lineup_size,
                                      seed = input$seed), args)))
          labels <- input$label1
          if (isTRUE(input$compare)) {
            fits[[2]] <- do.call(r4lineups::make_multid,
                                 c(list(counts = counts_for("2"), lineup_size = input$lineup_size,
                                        seed = input$seed + 1), args))
            labels <- c(labels, input$label2)
          }
        } else {
          d <- data()
          sets <- if (is.null(d$groups)) list(d$data) else d$groups$data
          labels <- if (is.null(d$groups)) d$label else d$groups$labels
          fits <- lapply(seq_along(sets), function(i) {
            do.call(r4lineups::make_multid,
                    c(list(data = sets[[i]], lineup_size = d$lineup_size,
                           innocent_suspect = d$innocent_suspect,
                           by_confidence = isTRUE(input$by_conf), seed = input$seed + i - 1), args))
          })
        }
        comparison <- if (length(fits) == 2) {
          r4lineups::compare_multid(fits[[1]], fits[[2]], labels = labels, seed = input$seed)
        }
        list(fits = fits, labels = labels, comparison = comparison)
      })
      c(run$value, list(notes = run$notes))
    }) |>
      bindEvent(input$run, data(), ignoreNULL = FALSE)

    output$notes <- renderUI(notes_ui(result()$notes))

    serve_table(output, "indices", function() {
      r <- result()
      do.call(rbind, lapply(seq_along(r$fits), function(i) {
        cbind(condition = r$labels[i], r$fits[[i]]$indices)
      }))
    }, "multid_indices.csv")

    output$plot_cond_ui <- renderUI({
      labels <- result()$labels
      if (length(labels) < 2) return(NULL)
      radioButtons(session$ns("plot_cond"), NULL, inline = TRUE,
                   choices = stats::setNames(seq_along(labels), labels))
    })
    plot_fit <- reactive({
      fits <- result()$fits
      fits[[min(as.integer(input$plot_cond %||% 1), length(fits))]]
    })

    serve_plot(output, "decomp", function() plot(plot_fit(), type = "decomposition"),
               "multid_decomposition.png")
    serve_plot(output, "index_plot", function() {
      fit <- plot_fit()
      validate(need(fit$n_bootstrap > 0, "Set bootstrap R above 0 to plot intervals."))
      plot(fit, type = "indices")
    }, "multid_indices.png")
    serve_plot(output, "conf_plot", function() {
      fit <- plot_fit()
      validate(need(!is.null(fit$by_confidence),
                    "Use the Data tab input with 'by confidence' checked."))
      plot(fit, type = "confidence")
    }, "multid_by_confidence.png")
    serve_table(output, "comparison", function() {
      cmp <- result()$comparison
      validate(need(!is.null(cmp), paste(
        "Add a second condition (counts), or choose two groups on the Data tab.")))
      as.data.frame(cmp)
    }, "multid_comparison.csv")

    result
  })
}
