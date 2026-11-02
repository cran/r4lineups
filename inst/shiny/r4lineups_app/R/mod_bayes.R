# Bayesian posteriors for effective size, functional size, diagnosticity,
# calibration and SDT comparisons.

bayes_ui <- function(id) {
  ns <- NS(id)
  sdt_inputs <- function(g, vals) {
    bslib::layout_columns(
      col_widths = c(6, 6),
      numericInput(ns(paste0("hits_", g)), "Hits", vals[1], min = 0),
      numericInput(ns(paste0("misses_", g)), "Misses", vals[2], min = 0),
      numericInput(ns(paste0("fas_", g)), "False alarms", vals[3], min = 0),
      numericInput(ns(paste0("crs_", g)), "Correct rejections", vals[4], min = 0)
    )
  }
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 360,
      selectInput(ns("analysis"), "Analysis", choices = c(
        "Effective size E'" = "esize", "Functional size" = "func",
        "Diagnosticity ratio" = "diag", "Calibration (Data tab)" = "calibration",
        "SDT d' comparison" = "sdt"
      )),
      conditionalPanel(
        sprintf("input['%s'] == 'esize' || input['%s'] == 'func'", ns("analysis"), ns("analysis")),
        lineup_input_ui(ns("lineup"))
      ),
      conditionalPanel(
        sprintf("input['%s'] == 'diag'", ns("analysis")),
        textAreaInput(ns("diag_tp"), "Target-present choices", rows = 3),
        textAreaInput(ns("diag_ta"), "Target-absent choices", rows = 3),
        bslib::layout_columns(
          col_widths = c(6, 6),
          numericInput(ns("diag_pos_tp"), "TP suspect position", 3, min = 1),
          numericInput(ns("diag_pos_ta"), "TA suspect position", 3, min = 1),
          numericInput(ns("diag_k_tp"), "TP lineup size", 8, min = 2),
          numericInput(ns("diag_k_ta"), "TA lineup size", 8, min = 2)
        ),
        actionLink(ns("diag_example"), "Load example (nortje2012 lineups 1 and 2)")
      ),
      conditionalPanel(
        sprintf("input['%s'] == 'calibration'", ns("analysis")),
        uiOutput(ns("banner")),
        checkboxInput(ns("choosers"), "Choosers only", value = TRUE),
        selectInput(ns("cal_metric"), "Plot", choices = c("C", "OU", "NRI"))
      ),
      conditionalPanel(
        sprintf("input['%s'] == 'sdt'", ns("analysis")),
        textInput(ns("label_A"), "Condition A", "A"),
        sdt_inputs("A", c(45, 55, 12, 88)),
        textInput(ns("label_B"), "Condition B", "B"),
        sdt_inputs("B", c(38, 62, 10, 90))
      ),
      tags$hr(),
      bslib::layout_columns(
        numericInput(ns("alpha"), "Dirichlet/Beta prior", 0.5, min = 0.01, step = 0.1),
        numericInput(ns("S"), "Posterior draws", 5000, min = 500, step = 500)
      ),
      bslib::layout_columns(
        numericInput(ns("threshold"), "Threshold t (optional)", NA),
        numericInput(ns("seed"), "Seed", 2026, step = 1)
      ),
      helpText("With a threshold, the summary reports P(statistic < t) and P(statistic > t)."),
      run_button(ns("run"))
    ),
    uiOutput(ns("notes")),
    table_card(ns, "summary", "Posterior summary"),
    plot_card(ns, "plot", "Posterior distribution"),
    table_card(ns, "draws", "Posterior draws (first 1,000)")
  )
}

bayes_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    lineup <- lineup_input_server("lineup")
    output$banner <- renderUI(data_banner(data()))

    observeEvent(input$diag_example, {
      updateTextAreaInput(session, "diag_tp", value = paste(r4lineups::nortje2012$lineup_1, collapse = ", "))
      updateTextAreaInput(session, "diag_ta", value = paste(r4lineups::nortje2012$lineup_2, collapse = ", "))
    })

    threshold <- reactive(if (is.na(input$threshold %||% NA)) NULL else input$threshold)
    probs <- function(res) {
      tp <- res$threshold_probs
      c(P_below_t = if (is.null(tp)) NA_real_ else tp$P_below,
        P_above_t = if (is.null(tp)) NA_real_ else tp$P_above)
    }
    interval <- function(res) {
      c(ci_lower = unname(res$credible_interval["lower"]),
        ci_upper = unname(res$credible_interval["upper"]))
    }

    result <- reactive({
      a <- input$alpha
      S <- as.integer(input$S)
      set.seed(input$seed)
      run <- capture_notes(switch(
        input$analysis,
        esize = {
          L <- lineup()
          res <- r4lineups::esize_T_bayes(L$tab, alpha = a, S = S, threshold = threshold())
          list(summary = data.frame(statistic = "E'", n = res$n, k = res$k,
                                    posterior_mean = res$posterior_mean,
                                    posterior_median = res$posterior_median,
                                    t(interval(res)), t(probs(res))),
               draws = data.frame(E_prime = res$E_draws), plot = plot(res))
        },
        func = {
          L <- lineup()
          validate(need(!is.na(L$target), "Enter the suspect position."))
          res <- r4lineups::func_size_bayes(L$vec, target_pos = L$target, alpha = a, S = S,
                                            threshold = threshold())
          list(summary = data.frame(statistic = "Functional size", n = res$n,
                                    suspect_ids = res$n_suspect,
                                    posterior_mean = res$posterior_mean,
                                    posterior_median = res$posterior_median,
                                    t(interval(res)), t(probs(res))),
               draws = data.frame(functional_size = res$F_draws), plot = plot(res))
        },
        diag = {
          tp <- parse_numeric_vector(input$diag_tp)
          ta <- parse_numeric_vector(input$diag_ta)
          validate(need(length(tp) > 0 && length(ta) > 0, "Enter TP and TA choices, or load the example."))
          res <- r4lineups::diag_ratio_T_bayes(tp, ta, pos_pres = input$diag_pos_tp,
                                               pos_abs = input$diag_pos_ta, k1 = input$diag_k_tp,
                                               k2 = input$diag_k_ta, alpha = a, S = S,
                                               threshold = threshold())
          list(summary = data.frame(statistic = "Diagnosticity ratio", n_tp = res$n_tp,
                                    n_ta = res$n_ta, posterior_mean = res$posterior_mean,
                                    posterior_median = res$posterior_median,
                                    t(interval(res)), t(probs(res))),
               draws = data.frame(DR = res$DR_draws, lnDR = res$lnDR_draws), plot = plot(res))
        },
        calibration = {
          d <- data()
          res <- r4lineups::calibration_bayes(d$data, confidence_bins = d$bins,
                                              choosers_only = isTRUE(input$choosers),
                                              lineup_size = d$lineup_size,
                                              innocent_suspect = d$innocent_suspect, alpha = a, S = S)
          stat_row <- function(m) {
            data.frame(statistic = m, posterior_mean = res[[paste0(m, "_mean")]],
                       posterior_median = res[[paste0(m, "_median")]],
                       ci_lower = unname(res[[paste0(m, "_ci")]]["lower"]),
                       ci_upper = unname(res[[paste0(m, "_ci")]]["upper"]))
          }
          list(summary = do.call(rbind, lapply(c("C", "OU", "NRI"), stat_row)),
               draws = data.frame(C = res$C_draws, OU = res$OU_draws, NRI = res$NRI_draws),
               plot = plot(res, metric = input$cal_metric))
        },
        sdt = {
          res <- r4lineups::sdt_compare(
            hits_A = input$hits_A, misses_A = input$misses_A, fas_A = input$fas_A, crs_A = input$crs_A,
            hits_B = input$hits_B, misses_B = input$misses_B, fas_B = input$fas_B, crs_B = input$crs_B,
            label_A = input$label_A, label_B = input$label_B, alpha = a, S = S
          )
          list(summary = data.frame(statistic = "d'(A) - d'(B)",
                                    dprime_A_mean = res$dprime_A_mean, dprime_B_mean = res$dprime_B_mean,
                                    posterior_mean = res$posterior_mean,
                                    posterior_median = res$posterior_median, t(interval(res)),
                                    P_A_greater = res$P_A_greater),
               draws = data.frame(dprime_A = res$dprime_A_draws, dprime_B = res$dprime_B_draws,
                                  delta_dprime = res$delta_dprime_draws),
               plot = plot(res))
        }
      ))
      c(run$value, list(notes = run$notes))
    }) |>
      bindEvent(input$run, ignoreNULL = FALSE)

    output$notes <- renderUI(notes_ui(result()$notes))
    serve_table(output, "summary", function() result()$summary, "bayes_summary.csv")
    serve_plot(output, "plot", function() result()$plot, "bayes_posterior.png")
    output$draws <- DT::renderDT(dt_table(utils::head(result()$draws, 1000)))
    output$draws_csv <- downloadHandler(
      filename = function() paste0("bayes_", input$analysis, "_draws.csv"),
      content = function(file) utils::write.csv(result()$draws, file, row.names = FALSE)
    )

    result
  })
}
