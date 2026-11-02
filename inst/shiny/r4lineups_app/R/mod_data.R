# Data tab: one confidence-based data set shared by the ROC, calibration,
# EIG/PPV, Multi-d' and Bayesian calibration analyses.

data_required_cols <- c("target_present", "identification", "confidence")

data_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      radioButtons(
        ns("source"), "Data source",
        choices = c(
          "Built-in: lineup_example" = "lineup_example",
          "Built-in: two simulated procedures" = "two_procedures",
          "Upload CSV" = "upload"
        )
      ),
      conditionalPanel(
        sprintf("input['%s'] == 'upload'", ns("source")),
        fileInput(ns("file"), NULL, accept = ".csv"),
        downloadLink(ns("template"), "Download a template CSV")
      ),
      numericInput(ns("lineup_size"), "Nominal lineup size", value = 6, min = 2, step = 1),
      selectInput(ns("innocent_suspect"), "Innocent suspect in target-absent lineups",
                  choices = c("Auto-detect" = "auto",
                              "Designated suspect" = "designated",
                              "None: estimate from fillers" = "estimated")),
      textInput(ns("bins"), "Confidence bins (edges)", value = "0, 60, 80, 100"),
      helpText("Bin edges are used by CAC, full ROC, calibration, EIG and PPV."),
      selectInput(ns("group_col"), "Compare two groups by column", choices = c("None" = "")),
      uiOutput(ns("group_levels_ui"))
    ),
    uiOutput(ns("status")),
    bslib::layout_column_wrap(
      width = 1 / 3, fill = FALSE,
      bslib::value_box("Lineups", textOutput(ns("n_rows")), theme = "primary"),
      bslib::value_box("Target present / absent", textOutput(ns("n_tp_ta")), theme = "light"),
      bslib::value_box("Suspect / filler / reject", textOutput(ns("n_resp")), theme = "light")
    ),
    bslib::card(
      bslib::card_header("Preview"),
      DT::DTOutput(ns("preview"))
    ),
    bslib::card(
      bslib::card_header("Expected format"),
      tags$p(
        "One row per witness. Required columns: ",
        tags$code("target_present"), " (TRUE/FALSE or 1/0), ",
        tags$code("identification"), " (suspect, filler or reject) and ",
        tags$code("confidence"), " (numeric). If ", tags$code("identification"),
        " is missing but ", tags$code("lineup_member"), " (0 = rejection) and ",
        tags$code("suspect_pos"), " are present, it is derived with ",
        tags$code("classify_lineup_choices()"), ". A ", tags$code("response_time"),
        " column enables the RAC analysis, and any other column can be used to compare two groups."
      )
    )
  )
}

data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    raw <- reactive({
      if (identical(input$source, "upload")) {
        req(input$file)
        utils::read.csv(input$file$datapath, stringsAsFactors = FALSE)
      } else {
        example_trial_data(input$source)
      }
    })

    observeEvent(input$source, {
      size <- if (identical(input$source, "upload")) input$lineup_size else 6
      updateNumericInput(session, "lineup_size", value = size)
      bins <- "0, 60, 80, 100"
      updateTextInput(session, "bins", value = bins)
    })

    prepared <- reactive({
      df <- raw()
      notes <- character()
      if (!"identification" %in% names(df) &&
          all(c("lineup_member", "suspect_pos") %in% names(df))) {
        df <- safely_validate(r4lineups::classify_lineup_choices(df, k = input$lineup_size))
        notes <- c(notes, "identification was derived from lineup_member and suspect_pos.")
      }
      missing <- setdiff(data_required_cols, names(df))
      validate(need(length(missing) == 0,
                    paste("Missing required column(s):", paste(missing, collapse = ", "))))
      tp <- df$target_present
      if (is.character(tp)) tp <- toupper(trimws(tp)) %in% c("TRUE", "T", "1", "TP", "YES")
      df$target_present <- as.logical(tp)
      df$identification <- tolower(trimws(df$identification))
      bad <- setdiff(unique(df$identification), c("suspect", "filler", "reject"))
      validate(need(length(bad) == 0,
                    paste("identification must be suspect, filler or reject; found:",
                          paste(utils::head(bad, 5), collapse = ", "))))
      validate(need(is.numeric(df$confidence), "confidence must be numeric."))
      list(data = df, notes = notes)
    })

    observe({
      df <- raw()
      candidates <- names(df)[vapply(df, function(x) {
        n <- length(unique(x))
        n >= 2 && n <= 20
      }, logical(1))]
      candidates <- setdiff(candidates, c(data_required_cols, "lineup_member", "suspect_pos"))
      selected <- if ("procedure" %in% candidates) "procedure" else ""
      updateSelectInput(session, "group_col", choices = c("None" = "", candidates),
                        selected = selected)
    })

    output$group_levels_ui <- renderUI({
      req(nzchar(input$group_col))
      lv <- sort(unique(raw()[[input$group_col]]))
      tagList(
        selectInput(session$ns("level1"), "Group 1", choices = lv, selected = lv[1]),
        selectInput(session$ns("level2"), "Group 2", choices = lv, selected = lv[min(2, length(lv))])
      )
    })

    groups <- reactive({
      col <- input$group_col
      if (is.null(col) || !nzchar(col) || is.null(input$level1) || is.null(input$level2) ||
          identical(input$level1, input$level2)) {
        return(NULL)
      }
      df <- prepared()$data
      lv <- c(input$level1, input$level2)
      out <- lapply(lv, function(l) df[as.character(df[[col]]) == l, , drop = FALSE])
      if (any(vapply(out, nrow, integer(1)) == 0)) {
        return(NULL)
      }
      names(out) <- lv
      list(column = col, labels = lv, data = out)
    })

    output$status <- renderUI({
      p <- prepared()
      notes_ui(p$notes)
    })

    output$n_rows <- renderText(nrow(prepared()$data))
    output$n_tp_ta <- renderText({
      tp <- prepared()$data$target_present
      sprintf("%d / %d", sum(tp, na.rm = TRUE), sum(!tp, na.rm = TRUE))
    })
    output$n_resp <- renderText({
      tab <- table(factor(prepared()$data$identification, c("suspect", "filler", "reject")))
      paste(as.integer(tab), collapse = " / ")
    })
    output$preview <- DT::renderDT(dt_table(utils::head(prepared()$data, 200)))

    output$template <- downloadHandler(
      filename = function() "r4lineups_template.csv",
      content = function(file) utils::write.csv(trial_template(), file, row.names = FALSE)
    )

    reactive(list(
      data = prepared()$data,
      label = if (identical(input$source, "upload")) input$file$name else input$source,
      lineup_size = input$lineup_size,
      innocent_suspect = input$innocent_suspect %||% "auto",
      bins = parse_bins(input$bins),
      groups = groups(),
      has_rt = "response_time" %in% names(prepared()$data)
    ))
  })
}

# A one-line reminder, shown on each analysis tab, of which data it uses.
data_banner <- function(d) {
  g <- if (is.null(d$groups)) "" else
    sprintf(" · comparing %s: %s vs %s", d$groups$column, d$groups$labels[1], d$groups$labels[2])
  div(
    class = "small text-muted mb-2",
    icon("database"),
    sprintf(" Data: %s (%d rows) · lineup size %d%s. Change on the Data tab.",
            d$label, nrow(d$data), as.integer(d$lineup_size), g)
  )
}

# Says, when two groups are being compared, which results pool them.
group_scope_note <- function(d, text) {
  if (is.null(d$groups)) {
    return(NULL)
  }
  div(class = "small text-muted mb-2", icon("circle-info"),
      sprintf(" Comparing %s %s and %s. %s", d$groups$column, d$groups$labels[1],
              d$groups$labels[2], text))
}
