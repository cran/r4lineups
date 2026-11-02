# Shared helpers for the r4lineups app. Statistics are never computed here:
# every estimate shown in the app comes from an exported r4lineups function.

`%||%` <- function(a, b) if (is.null(a)) b else a

is_webr <- function() identical(R.version$os, "emscripten")

face_tab_available <- function() {
  !is_webr() &&
    requireNamespace("reticulate", quietly = TRUE) &&
    requireNamespace("magick", quietly = TRUE)
}

parse_numeric_vector <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) {
    return(numeric(0))
  }
  vals <- unlist(regmatches(x, gregexpr("-?\\d+\\.?\\d*", x)))
  as.numeric(vals)
}

parse_bins <- function(x) {
  vals <- parse_numeric_vector(x)
  if (length(vals) < 2) {
    return(NULL)
  }
  sort(unique(vals))
}

# One numeric vector per non-empty line, for lists of lineups.
parse_lines <- function(x) {
  if (is.null(x)) {
    return(list())
  }
  lines <- strsplit(x, "\n", fixed = TRUE)[[1]]
  lines <- lines[nzchar(trimws(lines))]
  lapply(lines, parse_numeric_vector)
}

# Runs `expr`, discarding printed output and collecting messages and warnings
# so that the package's methodological notes can be shown beside the results.
# Errors become validation messages.
capture_notes <- function(expr) {
  notes <- character()
  value <- NULL
  utils::capture.output(
    value <- withCallingHandlers(
      safely_validate(expr),
      message = function(m) {
        notes <<- c(notes, conditionMessage(m))
        invokeRestart("muffleMessage")
      },
      warning = function(w) {
        notes <<- c(notes, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  )
  notes <- unique(trimws(notes))
  notes <- notes[nzchar(notes) & !grepl("^Computing bootstrap|^Done!?$", notes)]
  list(value = value, notes = notes)
}

# Turns an error from a package function into a Shiny validation message.
safely_validate <- function(expr) {
  tryCatch(expr, error = function(e) validate(need(FALSE, conditionMessage(e))))
}

notes_ui <- function(notes) {
  if (length(notes) == 0) {
    return(NULL)
  }
  div(
    class = "alert alert-info small mb-2",
    tags$strong("Notes from r4lineups"),
    tags$ul(lapply(notes, tags$li))
  )
}

round_df <- function(df, digits = 3) {
  if (is.null(df)) {
    return(df)
  }
  df <- as.data.frame(df)
  df[] <- lapply(df, function(x) if (is.numeric(x)) round(x, digits) else x)
  df
}

dt_table <- function(df, digits = 3, page_length = 25) {
  df <- round_df(df, digits)
  DT::datatable(
    df,
    rownames = FALSE,
    class = "compact stripe",
    options = list(pageLength = page_length, scrollX = TRUE,
                   dom = if (nrow(df) > page_length) "tip" else "t")
  )
}

fmt_ci <- function(x, digits = 3) {
  # An infinite limit is a real (unbounded) limit; only missing ones are blank.
  if (length(x) < 2 || anyNA(x[1:2])) {
    return("")
  }
  sprintf("[%s, %s]", format(round(x[1], digits), nsmall = digits),
          format(round(x[2], digits), nsmall = digits))
}

fmt_num <- function(x, digits = 3) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("—")
  }
  format(round(x, digits), nsmall = digits)
}

run_button <- function(id, label = "Run analysis") {
  actionButton(id, label, class = "btn-primary w-100", icon = icon("play"))
}

# A card holding a plot, with a PNG download in its header.
plot_card <- function(ns, id, title, height = "420px") {
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center",
      title,
      downloadButton(ns(paste0(id, "_png")), "PNG", class = "btn-sm btn-outline-secondary")
    ),
    plotOutput(ns(id), height = height)
  )
}

# A card holding a table, with a CSV download in its header.
table_card <- function(ns, id, title) {
  bslib::card(
    fill = FALSE,
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center",
      title,
      downloadButton(ns(paste0(id, "_csv")), "CSV", class = "btn-sm btn-outline-secondary")
    ),
    DT::DTOutput(ns(id), fill = FALSE)
  )
}

# Fills the outputs that plot_card() and table_card() declare.
# On screen the card header names the plot, so the plot's own title is dropped;
# the PNG download keeps it.
serve_plot <- function(output, id, plot_fn, filename = paste0(id, ".png")) {
  output[[id]] <- renderPlot({
    p <- plot_fn()
    if (!inherits(p, "ggplot")) return(p)
    p <- p + ggplot2::labs(title = NULL)
    if (nchar(p$labels$subtitle %||% "") > 45) p <- p + ggplot2::labs(subtitle = NULL)
    p
  }, res = 96)
  output[[paste0(id, "_png")]] <- downloadHandler(
    filename = function() filename,
    content = function(file) {
      ggplot2::ggsave(file, plot = plot_fn(), width = 7, height = 5, dpi = 300)
    }
  )
}

serve_table <- function(output, id, data_fn, filename = paste0(id, ".csv")) {
  output[[id]] <- DT::renderDT(dt_table(data_fn()))
  output[[paste0(id, "_csv")]] <- downloadHandler(
    filename = function() filename,
    content = function(file) utils::write.csv(data_fn(), file, row.names = FALSE)
  )
}

# Example confidence-based data sets offered on the Data tab.
example_trial_data <- function(name = c("lineup_example", "two_procedures")) {
  name <- match.arg(name)
  if (name == "lineup_example") {
    return(r4lineups::lineup_example)
  }
  sim <- function(d_prime, seed, label) {
    df <- as.data.frame(r4lineups::simulate_lineup_data(
      n_tp = 300, n_ta = 300, d_prime = d_prime, c_criterion = 1,
      include_response_time = TRUE, seed = seed
    ))
    df$confidence <- df$confidence * 20
    df$procedure <- label
    df
  }
  out <- rbind(sim(1.5, 11, "A"), sim(1.0, 12, "B"))
  out$participant_id <- seq_len(nrow(out))
  out
}

trial_template <- function() {
  data.frame(
    target_present = c(TRUE, TRUE, FALSE, FALSE),
    identification = c("suspect", "filler", "reject", "suspect"),
    confidence = c(90, 40, 60, 70),
    lineup_member = c(3L, 5L, 0L, 3L),
    suspect_pos = c(3L, 3L, 3L, 3L)
  )
}
