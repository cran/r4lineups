# Server-side tests for the bundled Shiny app. Each module is checked against
# a direct call to the package function it wraps, and its outputs must render.

skip_on_cran()
skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")

app_dir <- system.file("shiny/r4lineups_app", package = "r4lineups")
app_env <- new.env(parent = asNamespace("shiny"))
for (f in list.files(file.path(app_dir, "R"), pattern = "\\.R$", full.names = TRUE)) {
  sys.source(f, envir = app_env)
}
mod <- function(name) get(name, envir = app_env)

# Renders the named outputs, failing the test if any errors.
expect_outputs_render <- function(output, ids) {
  for (id in ids) {
    err <- tryCatch({
      suppressWarnings(output[[id]])
      NULL
    }, error = function(e) conditionMessage(e))
    expect_null(err, label = id)
  }
}

trial_data <- function(df = lineup_example, lineup_size = 6, bins = c(0, 60, 80, 100),
                       groups = NULL, innocent_suspect = "auto") {
  shiny::reactive(list(data = df, label = "test", lineup_size = lineup_size,
                       innocent_suspect = innocent_suspect, bins = bins,
                       groups = groups, has_rt = "response_time" %in% names(df)))
}

two_groups <- function() {
  df <- mod("example_trial_data")("two_procedures")
  list(column = "procedure", labels = c("A", "B"),
       data = list(A = df[df$procedure == "A", ], B = df[df$procedure == "B", ]))
}

test_that("data module loads the examples and finds the grouping column", {
  shiny::testServer(mod("data_server"), {
    session$setInputs(source = "two_procedures", lineup_size = 6, bins = "0, 60, 80, 100",
                      group_col = "procedure", level1 = "A", level2 = "B")
    d <- session$returned()
    expect_equal(nrow(d$data), 1200)
    expect_true(d$has_rt)
    expect_equal(d$groups$labels, c("A", "B"))
    expect_equal(d$bins, c(0, 60, 80, 100))
    expect_equal(d$innocent_suspect, "auto")
    session$setInputs(innocent_suspect = "designated")
    expect_equal(session$returned()$innocent_suspect, "designated")
  })
})

test_that("data module derives identification from lineup_member", {
  shiny::testServer(mod("data_server"), {
    session$setInputs(source = "lineup_example", lineup_size = 6, bins = "", group_col = "")
    expect_equal(nrow(session$returned()$data), nrow(lineup_example))
  })
  raw <- lineup_example[, c("target_present", "lineup_member", "suspect_pos", "confidence")]
  derived <- classify_lineup_choices(raw, k = 6)
  expect_equal(derived$identification, lineup_example$identification)
})

test_that("fairness module matches the package functions", {
  vec <- nortje2012$lineup_1
  shiny::testServer(mod("fairness_server"), {
    session$setInputs(`lineup-type` = "vector", `lineup-values` = paste(vec, collapse = ","),
                      `lineup-k` = 8, `lineup-target` = 3, R = 200, seed = 1, unbiased = FALSE,
                      run = 1)
    r <- session$returned()
    tab <- table(factor(vec, levels = 1:8))
    expect_equal(r$esize, esize_T(tab))
    expect_equal(r$malpass, esize_m(tab, 8))
    expect_equal(r$props$prop, allprop(vec, 8)$prop)
    expect_equal(r$func$estimate, func_size(vec, 3))
    expect_length(r$bias_boot, 200)
    expect_outputs_render(output, c("vb_prop", "vb_prop_ci", "vb_esize", "vb_func", "prop_plot",
                                    "prop_table", "bias_dist", "bias_tests", "esize_dist",
                                    "esize_table", "func_table"))
  })
})

test_that("fairness module accepts counts", {
  shiny::testServer(mod("fairness_server"), {
    session$setInputs(`lineup-type` = "table", `lineup-values` = "2, 5, 1, 0, 3, 1",
                      `lineup-k` = 6, `lineup-target` = 2, R = 100, seed = 1, unbiased = TRUE,
                      run = 1)
    expect_equal(session$returned()$esize, esize_T(c(2, 5, 1, 0, 3, 1), unbiased = TRUE))
  })
})

test_that("fairness module bootstraps the corrected E' when it reports it", {
  vec <- nortje2012$lineup_1
  shiny::testServer(mod("fairness_server"), {
    session$setInputs(`lineup-type` = "vector", `lineup-values` = paste(vec, collapse = ","),
                      `lineup-k` = 8, `lineup-target` = 3, R = 200, seed = 1, unbiased = TRUE,
                      run = 1)
    r <- session$returned()
    tab <- table(factor(vec, levels = 1:8))
    set.seed(1)
    ref <- esize_boot_dist(vec, 8, metric = "tredoux", R = 200, unbiased = TRUE)
    expect_equal(r$esize, esize_T(tab, unbiased = TRUE))
    expect_equal(r$esize_boot, ref)
    expect_equal(r$plugin, esize_T(tab))
    ci <- stats::quantile(ref, c(0.025, 0.975))
    expect_match(output$vb_esize_ci, format(round(ci[[1]], 2), nsmall = 2), fixed = TRUE)
    expect_outputs_render(output, c("esize_table", "esize_dist"))
  })
})

test_that("diagnosticity module matches diag_ratio_T and homog_diag", {
  choose_pos1 <- function(n1) c(rep(1, n1), rep(2:6, length.out = 100 - n1))
  tp <- lapply(c(60, 45, 32), choose_pos1)
  ta <- lapply(c(10, 15, 20), choose_pos1)
  lines <- function(x) paste(vapply(x, paste, character(1), collapse = ","), collapse = "\n")
  shiny::testServer(mod("diagnosticity_server"), {
    session$setInputs(tp = lines(tp), ta = lines(ta), pos = "1", k = "6", R = 50, seed = 1,
                      boot = FALSE, run = 1)
    r <- session$returned()
    expect_equal(r$pairs$dr_tredoux, mapply(diag_ratio_T, tp, ta, 1, 1, 6, 6))
    h <- suppressMessages(utils::capture.output(ref <- homog_diag(tp, ta, c(1, 1, 1), c(6, 6, 6))))
    expect_equal(r$homog$p_value, ref$p_value)
    expect_outputs_render(output, c("pairs", "homog"))
  })
})

test_that("Multi-d' module reproduces make_multid from counts", {
  shiny::testServer(mod("multid_server"), args = list(data = trial_data()), {
    session$setInputs(source = "counts", label1 = "one", G1 = 45, Fp1 = 24, Rp1 = 31, I1 = 17,
                      Fa1 = 26, Ra1 = 57, compare = FALSE, lineup_size = 6,
                      filler_rate = "total", correction = "none", n_boot = 0, seed = 1, run = 1)
    ref <- make_multid(counts = c(G = 45, Fp = 24, Rp = 31, I = 17, Fa = 26, Ra = 57),
                       correction = "none")
    expect_equal(session$returned()$fits[[1]]$indices$estimate, ref$indices$estimate)
    expect_outputs_render(output, c("indices", "decomp"))
  })
})

test_that("Multi-d' module compares two groups from the Data tab", {
  shiny::testServer(mod("multid_server"), args = list(data = trial_data(groups = two_groups())), {
    session$setInputs(source = "data", by_conf = TRUE, filler_rate = "total",
                      correction = "loglinear", n_boot = 50, seed = 1, run = 1)
    r <- session$returned()
    expect_length(r$fits, 2)
    expect_s3_class(r$comparison, "multid_comparison")
    expect_outputs_render(output, c("indices", "index_plot", "conf_plot", "comparison"))
  })
})

test_that("ROC module honours the lineup size from the Data tab", {
  shiny::testServer(mod("roc_server"), args = list(data = trial_data(lineup_size = 8)), {
    session$setInputs(order = "apriori", n_boot = 100, seed = 1, run = 1)
    r <- session$returned()
    expect_equal(r$roc$pauc, make_rocdata(lineup_example, lineup_size = 8)$pauc)
    expect_equal(r$roc$lineup_size, 8)
    expect_equal(r$cac$lineup_size, 8)
    expect_null(r$pauc)
    expect_outputs_render(output, c("vb_pauc", "roc_plot", "roc_table", "cac_plot", "cac_table",
                                    "fullroc_plot", "fullroc_table"))
  })
})

test_that("ROC module honours the innocent-suspect setting from the Data tab", {
  # No target-absent suspect IDs and no suspect_pos column
  df <- lineup_example[, c("target_present", "identification", "confidence")]
  df$identification[!df$target_present & df$identification == "suspect"] <- "filler"
  shiny::testServer(mod("roc_server"),
                    args = list(data = trial_data(df, innocent_suspect = "designated")), {
    session$setInputs(order = "apriori", n_boot = 100, seed = 1, run = 1)
    r <- session$returned()
    expect_equal(r$roc$innocent_suspect_method, "designated")
    expect_equal(max(r$roc$roc_data$false_id_rate), 0)
    expect_equal(r$cac$innocent_suspect_method, "designated")
    expect_equal(r$fullroc$innocent_suspect_method, "designated")
  })
})

test_that("ROC module runs the pAUC comparison for two groups", {
  shiny::testServer(mod("roc_server"), args = list(data = trial_data(groups = two_groups())), {
    session$setInputs(order = "apriori", n_boot = 100, seed = 1, run = 1)
    r <- session$returned()
    expect_s3_class(r$pauc, "pauc_comparison")
    expect_outputs_render(output, c("roc_plot", "pauc_table"))
  })
})

test_that("calibration module matches make_calibration and renders every panel", {
  df <- mod("example_trial_data")("two_procedures")
  shiny::testServer(mod("calibration_server"), args = list(data = trial_data(df, groups = two_groups())), {
    session$setInputs(choosers = TRUE, n_boot = 100, seed = 1, base_rate = 0.5, u_tp = 1,
                      u_fn = -0.5, u_fp = -2, u_tn = 0.5, bayes_cat = "confidence",
                      time_bins = "", run = 1)
    ref <- make_calibration(df, c(0, 60, 80, 100), show_plot = FALSE)
    expect_equal(session$returned$cal()$value$single$C, ref$C)
    expect_outputs_render(output, c("vb_c", "cal_plot", "cal_table", "anri_table", "anri_plot",
                                    "dpp_table", "dpp_plot", "util_plot", "util_table",
                                    "bayes_pp", "bayes_ig", "bree_plot", "rac_plot", "rac_table"))
  })
})

test_that("calibration module's DPP matches make_dpp and compare_dpp", {
  shiny::testServer(mod("calibration_server"), args = list(data = trial_data()), {
    session$setInputs(choosers = TRUE, n_boot = 100, seed = 1, base_rate = 0.5, u_tp = 1,
                      u_fn = -0.5, u_fp = -2, u_tn = 0.5, bayes_cat = "confidence",
                      time_bins = "", run = 1)
    expect_equal(session$returned$dpp()$value$single$dpp, make_dpp(lineup_example)$dpp)
    expect_outputs_render(output, c("dpp_table", "dpp_plot"))
  })
  df <- mod("example_trial_data")("two_procedures")
  g <- two_groups()
  shiny::testServer(mod("calibration_server"), args = list(data = trial_data(df, groups = g)), {
    session$setInputs(choosers = TRUE, n_boot = 100, seed = 1, base_rate = 0.5, u_tp = 1,
                      u_fn = -0.5, u_fp = -2, u_tn = 0.5, bayes_cat = "confidence",
                      time_bins = "", run = 1)
    ref <- compare_dpp(g$data[[1]], g$data[[2]], n_bootstrap = 100, seed = 1)
    cmp <- session$returned$dpp()$value$compare
    expect_equal(cmp$dpp_difference, ref$dpp_difference)
    expect_equal(cmp$ci_difference, ref$ci_difference)
  })
})

test_that("EIG module matches make_eig", {
  shiny::testServer(mod("eig_server"), args = list(data = trial_data()), {
    session$setInputs(prior = 0.5, run = 1)
    ref <- make_eig(lineup_example, confidence_bins = c(0, 60, 80, 100), show_plot = FALSE)
    expect_equal(session$returned()$eig$eig, ref$eig)
    expect_outputs_render(output, c("vb_eig", "eig_plot", "eig_post", "eig_table", "ppv_plot",
                                    "ppv_eff", "ppv_table"))
  })
})

test_that("Bayesian module matches esize_T_bayes under the same seed", {
  vec <- nortje2012$lineup_1
  shiny::testServer(mod("bayes_server"), args = list(data = trial_data()), {
    session$setInputs(analysis = "esize", `lineup-type` = "vector",
                      `lineup-values` = paste(vec, collapse = ","), `lineup-k` = 8,
                      `lineup-target` = 3, alpha = 0.5, S = 1000, threshold = NA, seed = 7,
                      run = 1)
    set.seed(7)
    ref <- esize_T_bayes(table(factor(vec, levels = 1:8)), alpha = 0.5, S = 1000)
    expect_equal(session$returned()$summary$posterior_mean, ref$posterior_mean)
    expect_outputs_render(output, c("summary", "plot", "draws"))

    for (i in 1:3) {
      a <- c("func", "calibration", "sdt")[i]
      session$setInputs(analysis = a, choosers = TRUE, cal_metric = "C", label_A = "A",
                        hits_A = 45, misses_A = 55, fas_A = 12, crs_A = 88, label_B = "B",
                        hits_B = 38, misses_B = 62, fas_B = 10, crs_B = 90,
                        run = i + 1)
      expect_outputs_render(output, c("summary", "plot"))
    }
  })
})

test_that("the app's UI builds", {
  app <- shiny::shinyAppDir(app_dir)
  expect_s3_class(app, "shiny.appobj")
})
