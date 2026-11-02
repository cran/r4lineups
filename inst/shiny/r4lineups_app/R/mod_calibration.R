# Calibration, ANRI, DPP, expected utility, Bayesian curves and RAC.
# Each sub-tab has its own reactive, so only the visible one is computed.

calibration_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      uiOutput(ns("banner")),
      checkboxInput(ns("choosers"), "Choosers only (suspect and filler IDs)", value = TRUE),
      bslib::layout_columns(
        numericInput(ns("n_boot"), "ANRI and DPP bootstrap R", value = 1000, min = 100, step = 100),
        numericInput(ns("seed"), "Seed", value = 2026, step = 1)
      ),
      tags$strong("Expected utility"),
      sliderInput(ns("base_rate"), "Base rate of guilt", min = 0.05, max = 0.95, value = 0.5, step = 0.05),
      bslib::layout_columns(
        col_widths = c(6, 6),
        numericInput(ns("u_tp"), "Utility: guilty ID", value = 1, step = 0.5),
        numericInput(ns("u_fn"), "Utility: guilty missed", value = -0.5, step = 0.5),
        numericInput(ns("u_fp"), "Utility: innocent ID", value = -2, step = 0.5),
        numericInput(ns("u_tn"), "Utility: innocent cleared", value = 0.5, step = 0.5)
      ),
      selectInput(ns("bayes_cat"), "Bayesian curves: response categories",
                  choices = c("By confidence" = "confidence", "Suspect / filler / reject" = "simple")),
      textInput(ns("time_bins"), "RAC response-time bin edges (blank = quintiles)", value = ""),
      run_button(ns("run"))
    ),
    bslib::navset_card_underline(
      id = ns("tabs"),
      bslib::nav_panel(
        "Calibration",
        uiOutput(ns("cal_notes")),
        uiOutput(ns("scope")),
        bslib::layout_column_wrap(
          width = 1 / 3, fill = FALSE,
          bslib::value_box("Calibration C", textOutput(ns("vb_c")), theme = "primary"),
          bslib::value_box("Over/underconfidence", textOutput(ns("vb_ou")), theme = "light"),
          bslib::value_box("Resolution NRI", textOutput(ns("vb_nri")), theme = "light")
        ),
        plot_card(ns, "cal_plot", "Calibration curve", height = "460px"),
        table_card(ns, "cal_table", "Calibration by confidence bin")
      ),
      bslib::nav_panel(
        "ANRI",
        uiOutput(ns("anri_notes")),
        table_card(ns, "anri_table", "Adjusted normalised resolution index"),
        plot_card(ns, "anri_plot", "ANRI comparison")
      ),
      bslib::nav_panel(
        "DPP",
        uiOutput(ns("dpp_notes")),
        table_card(ns, "dpp_table", "Deviation from perfect performance"),
        plot_card(ns, "dpp_plot", "DPP by operating point")
      ),
      bslib::nav_panel(
        "Expected utility",
        uiOutput(ns("util_notes")),
        bslib::layout_columns(
          col_widths = c(6, 6),
          plot_card(ns, "util_plot", "Expected utility by decision criterion"),
          table_card(ns, "util_table", "Utility by criterion")
        )
      ),
      bslib::nav_panel(
        "Bayesian curves",
        uiOutput(ns("bayes_notes")),
        plot_card(ns, "bayes_pp", "Prior-posterior curves", height = "460px"),
        plot_card(ns, "bayes_ig", "Information gain", height = "460px"),
        plot_card(ns, "bree_plot", "Base-rate effect equivalency (two groups)")
      ),
      bslib::nav_panel(
        "RAC",
        uiOutput(ns("rac_notes")),
        bslib::layout_columns(
          col_widths = c(6, 6),
          plot_card(ns, "rac_plot", "Response time-accuracy characteristic"),
          table_card(ns, "rac_table", "Accuracy by response-time bin")
        )
      )
    )
  )
}

calibration_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$banner <- renderUI(data_banner(data()))

    # Wraps one analysis so that it reruns on Run or on new data.
    analysis <- function(fn) {
      reactive({
        capture_notes(fn(data()))
      }) |>
        bindEvent(input$run, data(), ignoreNULL = FALSE)
    }

    cal <- analysis(function(d) {
      single <- r4lineups::make_calibration(d$data, d$bins, choosers_only = isTRUE(input$choosers),
                                            lineup_size = d$lineup_size,
                                            innocent_suspect = d$innocent_suspect, show_plot = FALSE)
      by_group <- if (!is.null(d$groups)) {
        df <- rbind(d$groups$data[[1]], d$groups$data[[2]])
        r4lineups::make_calibration_by_condition(df, d$groups$column, d$bins,
                                                 choosers_only = isTRUE(input$choosers),
                                                 lineup_size = d$lineup_size,
                                                 innocent_suspect = d$innocent_suspect)
      }
      list(single = single, by_group = by_group)
    })
    output$cal_notes <- renderUI(notes_ui(cal()$notes))
    output$scope <- renderUI(group_scope_note(data(), "The summary boxes use all rows; the plot and table compare the groups."))
    output$vb_c <- renderText(fmt_num(cal()$value$single$C, 4))
    output$vb_ou <- renderText(fmt_num(cal()$value$single$OU))
    output$vb_nri <- renderText(fmt_num(cal()$value$single$NRI))
    serve_plot(output, "cal_plot", function() {
      v <- cal()$value
      if (is.null(v$by_group)) r4lineups::make_calibration_gg(v$single)
      else r4lineups::make_calibration_by_condition_gg(v$by_group)
    }, "calibration.png")
    serve_table(output, "cal_table", function() {
      v <- cal()$value
      if (is.null(v$by_group)) v$single$calibration_data else v$by_group$condition_summary
    }, "calibration.csv")

    anri <- analysis(function(d) {
      common <- list(confidence_bins = d$bins, choosers_only = isTRUE(input$choosers),
                     lineup_size = d$lineup_size,
                     innocent_suspect = d$innocent_suspect, n_bootstrap = as.integer(input$n_boot),
                     seed = input$seed)
      if (is.null(d$groups)) {
        list(single = do.call(r4lineups::bootstrap_anri, c(list(data = d$data), common)))
      } else {
        df <- rbind(d$groups$data[[1]], d$groups$data[[2]])
        df[[d$groups$column]] <- factor(df[[d$groups$column]], levels = d$groups$labels)
        list(compare = do.call(r4lineups::compare_anri,
                               c(list(data = df, group_var = d$groups$column), common)))
      }
    })
    output$anri_notes <- renderUI(notes_ui(anri()$notes))
    serve_table(output, "anri_table", function() {
      v <- anri()$value
      if (!is.null(v$single)) {
        s <- v$single
        return(data.frame(quantity = c("ANRI", "NRI", sprintf("%d%% CI (ANRI)", round(100 * s$conf_level)),
                                       "N", "Bins", "Bootstrap R"),
                          value = c(fmt_num(s$anri, 4), fmt_num(s$nri, 4), fmt_ci(c(s$ci_lower, s$ci_upper), 4),
                                    s$n_total, s$n_bins, s$n_bootstrap)))
      }
      cmp <- v$compare
      data.frame(
        quantity = c(paste("ANRI", cmp$group_names), "Difference",
                     sprintf("%d%% CI of difference", round(100 * cmp$conf_level)), "Significant"),
        value = c(fmt_num(cmp$anri_group1, 4), fmt_num(cmp$anri_group2, 4), fmt_num(cmp$difference, 4),
                  fmt_ci(c(cmp$ci_lower, cmp$ci_upper), 4), cmp$significant)
      )
    }, "anri.csv")
    serve_plot(output, "anri_plot", function() {
      v <- anri()$value
      validate(need(!is.null(v$compare), "Choose two groups on the Data tab to compare ANRI."))
      r4lineups::plot_anri_comparison(v$compare)
    }, "anri_comparison.png")

    dpp <- analysis(function(d) {
      if (is.null(d$groups)) {
        list(single = r4lineups::make_dpp(d$data, lineup_size = d$lineup_size,
                                          innocent_suspect = d$innocent_suspect))
      } else {
        list(compare = r4lineups::compare_dpp(d$groups$data[[1]], d$groups$data[[2]],
                                              lineup_size = d$lineup_size,
                                              innocent_suspect = d$innocent_suspect,
                                              n_bootstrap = as.integer(input$n_boot),
                                              seed = input$seed))
      }
    })
    output$dpp_notes <- renderUI(notes_ui(dpp()$notes))
    serve_table(output, "dpp_table", function() {
      v <- dpp()$value
      d <- data()
      if (!is.null(v$single)) {
        s <- v$single
        pts <- s$dpp_by_point
        return(data.frame(
          quantity = c("Average DPP", paste("DPP at confidence >=", pts$confidence)),
          value = c(fmt_num(s$dpp), vapply(pts$dpp, fmt_num, character(1)))
        ))
      }
      cmp <- v$compare
      data.frame(quantity = c(paste("Average DPP", d$groups$labels), "Difference",
                              "Bootstrap SE", sprintf("%g%% CI", 100 * cmp$conf_level)),
                 value = c(fmt_num(cmp$dpp_a), fmt_num(cmp$dpp_b),
                           fmt_num(cmp$dpp_difference), fmt_num(cmp$se_difference),
                           sprintf("[%s, %s]", fmt_num(cmp$ci_difference[["lower"]]),
                                   fmt_num(cmp$ci_difference[["upper"]]))))
    }, "dpp.csv")
    serve_plot(output, "dpp_plot", function() {
      v <- dpp()$value
      if (!is.null(v$single)) return(r4lineups::plot_dpp(v$single))
      lab <- data()$groups$labels
      r4lineups::plot_dpp_comparison(v$compare, proc_a_label = lab[1], proc_b_label = lab[2])
    }, "dpp.png")

    util <- analysis(function(d) {
      um <- c(tp = input$u_tp, fn = input$u_fn, fp = input$u_fp, tn = input$u_tn)
      r4lineups::make_utility_curves(d$data, base_rate = input$base_rate, utility_matrix = um,
                                     lineup_size = d$lineup_size,
                                     innocent_suspect = d$innocent_suspect)
    })
    output$util_notes <- renderUI(notes_ui(util()$notes))
    serve_plot(output, "util_plot", function() r4lineups::plot_utility_curves(util()$value),
               "utility.png")
    serve_table(output, "util_table", function() util()$value$utility_data, "utility.csv")

    bayes <- analysis(function(d) {
      r4lineups::make_bayes_curves(d$data, response_categories = input$bayes_cat,
                                   confidence_bins = d$bins, lineup_size = d$lineup_size,
                                   innocent_suspect = d$innocent_suspect)
    })
    output$bayes_notes <- renderUI(notes_ui(bayes()$notes))
    serve_plot(output, "bayes_pp", function() r4lineups::plot_bayes_prior_posterior(bayes()$value),
               "bayes_prior_posterior.png")
    serve_plot(output, "bayes_ig", function() r4lineups::plot_bayes_information_gain(bayes()$value),
               "bayes_information_gain.png")

    # BREE compares suspect identifications regardless of confidence.
    bree <- analysis(function(d) {
      validate(need(!is.null(d$groups), "Choose two groups on the Data tab for the BREE curve."))
      r4lineups::make_bree_curve(d$groups$data[[1]], d$groups$data[[2]],
                                 lineup_size = d$lineup_size,
                                 innocent_suspect = d$innocent_suspect)
    })
    serve_plot(output, "bree_plot", function() r4lineups::plot_bree(bree()$value), "bree.png")

    rac <- analysis(function(d) {
      validate(need(d$has_rt, "RAC needs a response_time column in the data."))
      bins <- parse_bins(input$time_bins)
      if (is.null(bins)) {
        rt <- d$data$response_time
        bins <- unique(stats::quantile(rt, seq(0, 1, 0.2), na.rm = TRUE, names = FALSE))
      }
      r4lineups::make_racdata(d$data, lineup_size = d$lineup_size,
                              innocent_suspect = d$innocent_suspect, time_bins = bins)
    })
    output$rac_notes <- renderUI(notes_ui(rac()$notes))
    serve_plot(output, "rac_plot", function() r4lineups::make_rac_gg(rac()$value), "rac.png")
    serve_table(output, "rac_table", function() rac()$value$rac_data, "rac.csv")

    list(cal = cal, anri = anri, dpp = dpp, util = util, bayes = bayes, bree = bree, rac = rac)
  })
}
