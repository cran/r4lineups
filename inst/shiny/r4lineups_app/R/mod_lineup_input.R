# Mock-witness lineup entry: a vector of chosen positions or a table of counts.

lineup_input_ui <- function(id, target = TRUE) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("type"), "Input", choices = c("Vector" = "vector", "Counts" = "table"),
                 inline = TRUE),
    textAreaInput(ns("values"), NULL, rows = 4,
                  value = paste(r4lineups::nortje2012$lineup_1, collapse = ", "),
                  placeholder = "Vector: 3, 2, 5, 6, 1, 3 ...   Counts: 2, 5, 1, 0, 3, 1"),
    helpText("A vector lists the position each mock witness chose; counts list choices per position 1..k."),
    bslib::layout_columns(
      numericInput(ns("k"), "Lineup size (k)", value = 8, min = 2, step = 1),
      if (target) numericInput(ns("target"), "Suspect position", value = 3, min = 1, step = 1)
    ),
    actionLink(ns("example"), "Load example (nortje2012$lineup_1)")
  )
}

lineup_input_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$example, {
      updateRadioButtons(session, "type", selected = "vector")
      updateTextAreaInput(session, "values",
                          value = paste(r4lineups::nortje2012$lineup_1, collapse = ", "))
      updateNumericInput(session, "k", value = 8)
      updateNumericInput(session, "target", value = 3)
    })

    reactive({
      vals <- parse_numeric_vector(input$values)
      validate(need(length(vals) > 0, "Enter a lineup vector or counts, or load the example."))
      k <- input$k
      validate(need(is.numeric(k) && !is.na(k) && k >= 2, "Lineup size must be at least 2."))
      if (identical(input$type, "table")) {
        validate(need(length(vals) == k, "Counts must have one entry per lineup position (k)."),
                 need(all(vals >= 0 & vals == round(vals)), "Counts must be non-negative integers."))
        vec <- rep(seq_len(k), vals)
      } else {
        vec <- vals
      }
      tab <- safely_validate(r4lineups::lineup_table(vec, k))
      validate(need(sum(tab) > 1, "At least two choices are needed."))
      target <- input$target %||% NA
      if (!is.na(target)) {
        validate(need(target >= 1 && target <= k, "Suspect position must be between 1 and k."))
      }
      list(vec = as.numeric(vec), tab = tab, k = as.integer(k), target = target, n = length(vec))
    })
  })
}
