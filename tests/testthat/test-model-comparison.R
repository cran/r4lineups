# Tests for model comparison functions

# Create test data
create_test_data <- function(n_tp = 100, n_ta = 100) {
  set.seed(123)
  data.frame(
    target_present = rep(c(TRUE, FALSE), each = n_tp),
    identification = c(
      sample(c("suspect", "filler", "reject"), n_tp, replace = TRUE,
             prob = c(0.6, 0.2, 0.2)),
      sample(c("suspect", "filler", "reject"), n_ta, replace = TRUE,
             prob = c(0.2, 0.3, 0.5))
    ),
    confidence = sample(c(20, 40, 60, 80, 100), n_tp + n_ta, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

test_that("compare_models works with all models", {
  test_data <- create_test_data()

  comparison <- compare_models(
    test_data,
    models = c("2ht", "eig", "fullroc"),
    lineup_size = 6,
    show_warnings = FALSE
  )

  expect_s3_class(comparison, "model_comparison")
  expect_type(comparison$fitted_models, "list")
  expect_true("comparison_table" %in% names(comparison))
  expect_true(nrow(comparison$comparison_table) > 0)
})

test_that("compare_models works with subset of models", {
  test_data <- create_test_data()

  comparison <- compare_models(
    test_data,
    models = c("eig", "fullroc"),
    lineup_size = 6,
    show_warnings = FALSE
  )

  expect_equal(length(comparison$models_fit), 2)
  expect_true("eig" %in% comparison$models_fit)
  expect_true("fullroc" %in% comparison$models_fit)
  expect_false("2ht" %in% comparison$models_fit)
})

test_that("compare_models handles 2ht model correctly", {
  test_data <- create_test_data(n_tp = 150, n_ta = 150)

  comparison <- compare_models(
    test_data,
    models = "2ht",
    lineup_size = 6,
    show_warnings = FALSE
  )

  if ("2ht" %in% comparison$models_fit) {
    model_2ht <- comparison$fitted_models$`2ht`

    expect_true("parameters" %in% names(model_2ht))
    expect_equal(length(model_2ht$parameters), 4)
    expect_true(all(c("dP", "dA", "b", "g") %in% names(model_2ht$parameters)))
    expect_true(all(model_2ht$parameters >= 0 & model_2ht$parameters <= 1))
    expect_true("aic" %in% names(model_2ht))
    expect_true("bic" %in% names(model_2ht))
  }
})

test_that("compare_models handles EIG model correctly", {
  test_data <- create_test_data()

  comparison <- compare_models(
    test_data,
    models = "eig",
    prior_guilt = 0.5,
    show_warnings = FALSE
  )

  expect_true("eig" %in% comparison$models_fit)
  model_eig <- comparison$fitted_models$eig

  expect_true("eig" %in% names(model_eig))
  expect_true(model_eig$eig >= 0)
  expect_true(model_eig$eig <= 1)  # Max 1 bit with prior = 0.5
  expect_true("response_data" %in% names(model_eig))
})

test_that("compare_models handles Full ROC correctly", {
  test_data <- create_test_data()

  comparison <- compare_models(
    test_data,
    models = "fullroc",
    lineup_size = 6,
    show_warnings = FALSE
  )

  expect_true("fullroc" %in% comparison$models_fit)
  model_fullroc <- comparison$fitted_models$fullroc

  expect_true("auc" %in% names(model_fullroc))
  expect_true(model_fullroc$auc >= 0.5)
  expect_true(model_fullroc$auc <= 1.0)
  expect_true("roc_data" %in% names(model_fullroc))
})

test_that("print.model_comparison works", {
  test_data <- create_test_data()
  comparison <- compare_models(test_data, models = "eig", show_warnings = FALSE)

  expect_output(print(comparison), "Lineup Model Comparison")
  expect_output(print(comparison), "Models fit")
})

test_that("summary.model_comparison works", {
  test_data <- create_test_data()
  comparison <- compare_models(test_data, models = "eig", show_warnings = FALSE)

  expect_output(summary(comparison), "Model Comparison Summary")
  expect_output(summary(comparison), "Dataset")
})

test_that("format_comparison_table works", {
  test_data <- create_test_data()
  comparison <- compare_models(test_data, models = "eig", show_warnings = FALSE)

  table_console <- format_comparison_table(comparison, format = "console")
  expect_true(is.data.frame(table_console))
  expect_true(nrow(table_console) > 0)
})

test_that("compare_models validates input data", {
  # Missing columns
  bad_data <- data.frame(x = 1:10, y = 1:10)

  expect_error(
    compare_models(bad_data, models = "eig"),
    "Missing required columns"
  )
})

test_that("compare_models handles invalid model names", {
  test_data <- create_test_data()

  expect_error(
    compare_models(test_data, models = "invalid_model"),
    "Invalid model names"
  )
})

test_that("compare_models with confidence bins", {
  test_data <- create_test_data()

  comparison <- compare_models(
    test_data,
    models = c("eig", "fullroc"),
    confidence_bins = c(0, 50, 100),
    show_warnings = FALSE
  )

  expect_true("eig" %in% comparison$models_fit)
  expect_true("fullroc" %in% comparison$models_fit)
})

test_that("compare_models with custom prior", {
  test_data <- create_test_data()

  comparison <- compare_models(
    test_data,
    models = "eig",
    prior_guilt = 0.3,
    show_warnings = FALSE
  )

  expect_equal(comparison$fitted_models$eig$prior_guilt, 0.3)
})

test_that("markdown output is one line per row and carries no row names", {
  # Regression test. format_comparison_table(format = "markdown") returns a
  # knitr_kable object, which is a character vector with one element per line.
  # The vignette printed it with cat(x) rather than cat(x, sep = "\n"), which
  # joined every line with a space and collapsed the table into a single
  # paragraph. kable was also called without row.names = FALSE, so the row
  # names inherited from the named parameter vectors ("dP", "dA", ...) appeared
  # as a spurious unlabelled first column.
  test_data <- create_test_data()
  comparison <- compare_models(test_data, models = "eig", show_warnings = FALSE)

  md <- format_comparison_table(comparison, format = "markdown")

  expect_gt(length(md), 2)                      # header, separator, >=1 row
  expect_true(all(grepl("^\\|", md)))           # every element a table line
  expect_match(md[2], "^\\|:?-")                # second line is the separator

  # No leading empty header cell, which is how the row-name column showed up
  expect_false(grepl("^\\|\\s+\\|", md[1]))
})
