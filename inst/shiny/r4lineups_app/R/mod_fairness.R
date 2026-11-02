# Lineup fairness from mock-witness choices: bias, effective size and
# functional size for one lineup.

fairness_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 330,
      lineup_input_ui(ns("lineup")),
      tags$hr(),
      bslib::layout_columns(
        numericInput(ns("R"), "Bootstrap R", value = 1000, min = 100, step = 100),
        numericInput(ns("seed"), "Seed", value = 2026, step = 1)
      ),
      checkboxInput(ns("unbiased"), "Small-sample corrected E' (unbiased sum of p squared)", value = FALSE),
      helpText("Plug-in E' is biased downward with few mock witnesses; the corrected form uses",
               "N(N-1) / sum o(o-1). See \"E' is biased downward in small samples\" in the core vignette."),
      run_button(ns("run"))
    ),
    uiOutput(ns("notes")),
    bslib::layout_column_wrap(
      width = 1 / 3, fill = FALSE,
      bslib::value_box("Suspect proportion", textOutput(ns("vb_prop")),
                       textOutput(ns("vb_prop_ci")), theme = "primary"),
      bslib::value_box("Effective size E'", textOutput(ns("vb_esize")),
                       textOutput(ns("vb_esize_ci")), theme = "light"),
      bslib::value_box("Functional size", textOutput(ns("vb_func")),
                       textOutput(ns("vb_func_ci")), theme = "light")
    ),
    bslib::navset_card_underline(
      id = ns("tabs"),
      bslib::nav_panel(
        "Lineup bias",
        bslib::layout_columns(
          col_widths = c(6, 6),
          plot_card(ns, "prop_plot", "Proportion choosing each member"),
          table_card(ns, "prop_table", "Member proportions (* = suspect)")
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          plot_card(ns, "bias_dist", "Bootstrap distribution of the suspect proportion"),
          table_card(ns, "bias_tests", "Suspect proportion against chance (1/k)")
        )
      ),
      bslib::nav_panel(
        "Effective size",
        bslib::layout_columns(
          col_widths = c(6, 6),
          plot_card(ns, "esize_dist", "Bootstrap distribution of E'"),
          table_card(ns, "esize_table", "Effective size estimates")
        )
      ),
      bslib::nav_panel(
        "Functional size",
        table_card(ns, "func_table", "Functional size (N / suspect choices)")
      )
    )
  )
}

fairness_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    lineup <- lineup_input_server("lineup")

    result <- reactive({
      L <- lineup()
      validate(need(!is.na(L$target), "Enter the suspect position."))
      R <- as.integer(input$R)
      set.seed(input$seed)
      unbiased <- isTRUE(input$unbiased)
      run <- capture_notes({
        # The corrected E' and its bootstrap come first, so that the latter
        # can be reproduced from the seed alone
        corrected <- if (unbiased) r4lineups::esize_T(L$tab, unbiased = TRUE)
        corrected_boot <- if (unbiased) {
          r4lineups::esize_boot_dist(L$vec, L$k, metric = "tredoux", R = R, unbiased = TRUE)
        }
        props <- r4lineups::allprop(L$vec, L$k)
        prop_ci <- r4lineups::lineup_boot_allprop(L$vec, L$k, R = R)
        bias_boot <- r4lineups::lineup_bias_boot_dist(L$vec, L$target, L$k, R = R)
        plugin <- r4lineups::esize_T(L$tab)
        esize_normal <- r4lineups::esize_T_ci_n(L$tab, 0.95)
        esize_boot <- r4lineups::esize_boot_dist(L$vec, L$k, metric = "tredoux", R = R)
        malpass <- r4lineups::esize_m(L$tab, L$k)
        malpass_boot <- r4lineups::esize_boot_dist(L$vec, L$k, metric = "malpass", R = R)
        func <- tryCatch(r4lineups::func_size_report(L$vec, L$target, L$k, R = R),
                         error = function(e) {
                           warning("Functional size: ", conditionMessage(e), call. = FALSE)
                           NULL
                         })
        # esize and esize_boot are the estimator shown in the value box and
        # distribution plot: the corrected one when it is requested
        list(props = props, prop_ci = prop_ci, bias_boot = bias_boot,
             esize = if (unbiased) corrected else plugin,
             esize_boot = if (unbiased) corrected_boot else esize_boot,
             plugin = plugin, plugin_boot = esize_boot, esize_normal = esize_normal,
             unbiased = unbiased, malpass = malpass, malpass_boot = malpass_boot,
             func = func)
      })
      c(run$value, list(notes = run$notes, L = L))
    }) |>
      bindEvent(input$run, ignoreNULL = FALSE)

    output$notes <- renderUI(notes_ui(result()$notes))

    suspect_count <- reactive(as.integer(result()$L$tab[result()$L$target]))

    output$vb_prop <- renderText(fmt_num(result()$props$prop[result()$L$target]))
    output$vb_prop_ci <- renderText({
      r <- result()
      sprintf("95%% CI %s · chance %s",
              fmt_ci(stats::quantile(r$bias_boot, c(0.025, 0.975))), fmt_num(1 / r$L$k))
    })
    output$vb_esize <- renderText(fmt_num(result()$esize, 2))
    output$vb_esize_ci <- renderText({
      r <- result()
      sprintf("Bootstrap range %s · k = %d; not an exact-fairness test",
              fmt_ci(stats::quantile(r$esize_boot, c(0.025, 0.975)), 2), r$L$k)
    })
    output$vb_func <- renderText({
      f <- result()$func
      if (is.null(f)) "—" else fmt_num(f$estimate, 2)
    })
    output$vb_func_ci <- renderText({
      f <- result()$func
      if (is.null(f)) "No suspect choices" else sprintf("95%% CI %s (percentile)", fmt_ci(f$percentile, 2))
    })

    prop_df <- reactive({
      r <- result()
      data.frame(
        position = ifelse(seq_len(r$L$k) == r$L$target, paste0(seq_len(r$L$k), "*"),
                          as.character(seq_len(r$L$k))),
        count = as.integer(r$L$tab),
        proportion = r$props$prop,
        ci_low = r$prop_ci$ci_low,
        ci_high = r$prop_ci$ci_high
      )
    })

    serve_table(output, "prop_table", prop_df, "member_proportions.csv")
    serve_plot(output, "prop_plot", function() {
      df <- prop_df()
      df$member <- factor(seq_len(nrow(df)))
      df$suspect <- grepl("*", df$position, fixed = TRUE)
      ggplot2::ggplot(df, ggplot2::aes(member, proportion, fill = suspect)) +
        ggplot2::geom_col(width = 0.7) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
        ggplot2::geom_hline(yintercept = 1 / result()$L$k, linetype = "dashed") +
        ggplot2::scale_fill_manual(values = c(`FALSE` = "#9aa9b8", `TRUE` = "#2c5d8a"),
                                   labels = c("Filler", "Suspect"), name = NULL) +
        ggplot2::labs(x = "Lineup position", y = "Proportion of mock witnesses",
                      caption = "Dashed line: chance (1/k)\nError bars: 95% bootstrap intervals") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "top")
    }, "member_proportions.png")

    bias_tests <- reactive({
      r <- result()
      bt <- stats::binom.test(suspect_count(), r$L$n, p = 1 / r$L$k, alternative = "greater")
      ci_exact <- stats::binom.test(suspect_count(), r$L$n)$conf.int
      data.frame(
        quantity = c("Suspect choices", "Mock witnesses", "Suspect proportion", "Chance (1/k)",
                     "Exact 95% CI", "Bootstrap 95% CI", "Binomial p (proportion > chance)"),
        value = c(suspect_count(), r$L$n, fmt_num(suspect_count() / r$L$n), fmt_num(1 / r$L$k),
                  fmt_ci(ci_exact), fmt_ci(stats::quantile(r$bias_boot, c(0.025, 0.975))),
                  format.pval(bt$p.value, digits = 3))
      )
    })
    serve_table(output, "bias_tests", bias_tests, "suspect_bias.csv")
    serve_plot(output, "bias_dist", function() {
      r4lineups::plot_lineup_bias_distribution(result()$bias_boot, target_pos = result()$L$target)
    }, "suspect_bias_distribution.png")

    esize_df <- reactive({
      r <- result()
      boot_ci <- function(x) fmt_ci(stats::quantile(x, c(0.025, 0.975)))
      plugin_rows <- data.frame(
        measure = c(if (r$unbiased) "E' (Tredoux, 1998), plug-in" else "E' (Tredoux, 1998)",
                    "E' plug-in normal-theory 95% CI", "E' plug-in bootstrap 95% CI"),
        value = c(fmt_num(r$plugin, 3), fmt_ci(c(r$esize_normal$ci_low, r$esize_normal$ci_high)),
                  boot_ci(r$plugin_boot))
      )
      corrected_rows <- if (r$unbiased) {
        data.frame(measure = c("E' (Tredoux, 1998), small-sample corrected",
                               "E' corrected bootstrap 95% CI"),
                   value = c(fmt_num(r$esize, 3), boot_ci(r$esize_boot)))
      }
      rbind(
        corrected_rows,
        plugin_rows,
        data.frame(measure = c("Effective size (Malpass, 1981; Tredoux adjustment)",
                               "Malpass bootstrap 95% CI", "Nominal size k"),
                   value = c(fmt_num(r$malpass, 3), boot_ci(r$malpass_boot), r$L$k))
      )
    })
    serve_table(output, "esize_table", esize_df, "effective_size.csv")
    serve_plot(output, "esize_dist", function() {
      r4lineups::plot_esize_distribution(
        result()$esize_boot,
        metric = if (result()$unbiased) "E' (Tredoux), corrected" else "E' (Tredoux)"
      )
    }, "effective_size_distribution.png")

    func_df <- reactive({
      f <- result()$func
      validate(need(!is.null(f), "Functional size is undefined when nobody chooses the suspect."))
      data.frame(
        quantity = c("Functional size", "Normal-theory 95% CI", "Bootstrap percentile 95% CI",
                     "Bootstrap BCa 95% CI", "Exact (Clopper-Pearson) 95% CI", "Bootstrap R",
                     "Bootstrap samples with no suspect choices"),
        value = c(fmt_num(f$estimate, 3), fmt_ci(f$normal), fmt_ci(f$percentile), fmt_ci(f$bca),
                  fmt_ci(f$exact), f$R, f$n_infinite)
      )
    })
    serve_table(output, "func_table", func_df, "functional_size.csv")

    result
  })
}

# Diagnosticity ratios for one or more target-present / target-absent pairs,
# with a homogeneity test when there are several pairs.

# Three pairs from the core vignette: the suspect (position 1) is chosen less
# often in TP and more often in TA lineups from pair to pair.
diag_example <- function() {
  choose_pos1 <- function(n1) c(rep(1, n1), rep(2:6, length.out = 100 - n1))
  lines <- function(x) paste(vapply(x, paste, character(1), collapse = ", "), collapse = "\n")
  list(tp = lines(lapply(c(60, 45, 32), choose_pos1)),
       ta = lines(lapply(c(10, 15, 20), choose_pos1)))
}

diagnosticity_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 360,
      textAreaInput(ns("tp"), "Target-present lineups (one per line)", rows = 4,
                    value = diag_example()$tp),
      textAreaInput(ns("ta"), "Target-absent lineups (one per line, same order)", rows = 4,
                    value = diag_example()$ta),
      textInput(ns("pos"), "Suspect position per pair", value = "1"),
      textInput(ns("k"), "Lineup size per pair", value = "6"),
      helpText("Give one value to use it for every pair."),
      actionLink(ns("example"), "Load example (three pairs, from the vignette)"),
      tags$hr(),
      bslib::layout_columns(
        numericInput(ns("R"), "Bootstrap R", value = 500, min = 50, step = 50),
        numericInput(ns("seed"), "Seed", value = 2026, step = 1)
      ),
      checkboxInput(ns("boot"), "Bootstrap the homogeneity test", value = FALSE),
      run_button(ns("run"))
    ),
    uiOutput(ns("notes")),
    table_card(ns, "pairs", "Diagnosticity ratio for each pair"),
    table_card(ns, "homog", "Homogeneity of diagnosticity ratios (Tredoux, 1998)")
  )
}

diagnosticity_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$example, {
      updateTextAreaInput(session, "tp", value = diag_example()$tp)
      updateTextAreaInput(session, "ta", value = diag_example()$ta)
      updateTextInput(session, "pos", value = "1")
      updateTextInput(session, "k", value = "6")
    })

    result <- reactive({
      tp <- parse_lines(input$tp)
      ta <- parse_lines(input$ta)
      validate(need(length(tp) > 0, "Enter target-present lineups, or load the example."),
               need(length(tp) == length(ta), "Enter the same number of TP and TA lineups."))
      n <- length(tp)
      pos <- rep_len(parse_numeric_vector(input$pos), n)
      k <- rep_len(parse_numeric_vector(input$k), n)
      validate(need(length(pos) == n && length(k) == n, "Give a suspect position and lineup size."))
      set.seed(input$seed)
      run <- capture_notes({
        pairs <- do.call(rbind, lapply(seq_len(n), function(i) {
          data.frame(
            pair = i, n_tp = length(tp[[i]]), n_ta = length(ta[[i]]),
            suspect_ids_tp = sum(tp[[i]] == pos[i]), suspect_ids_ta = sum(ta[[i]] == pos[i]),
            dr_wells_lindsay = r4lineups::diag_ratio_W(tp[[i]], ta[[i]], pos[i], pos[i], k[i], k[i]),
            dr_tredoux = r4lineups::diag_ratio_T(tp[[i]], ta[[i]], pos[i], pos[i], k[i], k[i])
          )
        }))
        homog <- NULL
        if (n >= 2) {
          homog <- if (isTRUE(input$boot)) {
            r4lineups::homog_diag_boot(tp, ta, k = k, R = as.integer(input$R), pos_list = pos,
                                       seed = input$seed)
          } else {
            r4lineups::homog_diag(tp, ta, pos, k)
          }
        }
        list(pairs = pairs, homog = homog)
      })
      pair_ci <- run$value$homog$parameters
      if (!is.null(pair_ci)) {
        run$value$pairs$dr_ci_lower <- pair_ci$lower
        run$value$pairs$dr_ci_upper <- pair_ci$upper
      }
      c(run$value, list(notes = run$notes))
    }) |>
      bindEvent(input$run, ignoreNULL = FALSE)

    output$notes <- renderUI(notes_ui(result()$notes))
    serve_table(output, "pairs", function() result()$pairs, "diagnosticity_pairs.csv")
    serve_table(output, "homog", function() {
      h <- result()$homog
      validate(need(!is.null(h), "The homogeneity test needs at least two lineup pairs."))
      data.frame(
        quantity = c("Mean diagnosticity ratio", "Chi-square (Q)", "df", "p",
                     "Heterogeneous", if (!is.null(h$mean_ci)) "Bootstrap CI for mean"),
        value = c(fmt_num(h$mean_diagnosticity), fmt_num(h$chi_square), h$df,
                  format.pval(h$p_value, digits = 3), h$heterogeneous,
                  if (!is.null(h$mean_ci)) fmt_ci(h$mean_ci))
      )
    }, "diagnosticity_homogeneity.csv")

    result
  })
}
