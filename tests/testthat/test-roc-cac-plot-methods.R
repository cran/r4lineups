testthat::test_that("plot() dispatches for every ROC and CAC object", {
  data(lineup_example, envir = environment())

  objects <- list(
    roc_data     = make_rocdata(lineup_example),
    roc          = make_roc(lineup_example, show_plot = FALSE),
    cac_data     = make_cacdata(lineup_example),
    cac          = make_cac(lineup_example, show_plot = FALSE),
    fullroc_data = make_fullroc_data(lineup_example),
    fullroc      = make_fullroc(lineup_example, show_plot = FALSE)
  )

  for (nm in names(objects)) {
    testthat::expect_s3_class(plot(objects[[nm]]), "ggplot")
  }
})

testthat::test_that("data builders carry a class so plot() can find them", {
  data(lineup_example, envir = environment())

  testthat::expect_s3_class(make_rocdata(lineup_example), "lineup_roc_data")
  testthat::expect_s3_class(make_cacdata(lineup_example), "lineup_cac_data")
  testthat::expect_s3_class(make_fullroc_data(lineup_example),
                            "lineup_fullroc_data")
})

testthat::test_that("plot_fullroc labels the ordering method for either input", {
  data(lineup_example, envir = environment())

  # make_fullroc_data() records $order_used; make_fullroc() records
  # $summary$order_method. Both must produce a labelled subtitle rather than a
  # bare "Ordering method: ".
  from_data <- plot_fullroc(make_fullroc_data(lineup_example))
  from_wrap <- plot_fullroc(make_fullroc(lineup_example, show_plot = FALSE))

  testthat::expect_equal(from_data$labels$subtitle,
                         "Ordering method: diagnosticity")
  testthat::expect_equal(from_wrap$labels$subtitle,
                         "Ordering method: diagnosticity")
})

testthat::test_that("plot_fullroc respects a non-default ordering", {
  data(lineup_example, envir = environment())

  p <- plot_fullroc(make_fullroc(lineup_example, order = "apriori",
                                 show_plot = FALSE))
  testthat::expect_equal(p$labels$subtitle, "Ordering method: apriori")
})

testthat::test_that("plot_fullroc rejects objects it cannot label", {
  testthat::expect_error(plot_fullroc(list(roc_data = data.frame(), auc = 1)),
                         "make_fullroc")
})

testthat::test_that("ROC plot names the variant and the data source", {
  data(lineup_example, envir = environment())

  p <- plot(make_rocdata(lineup_example))
  testthat::expect_match(p$labels$title, "Partial ROC")
  testthat::expect_match(p$labels$caption, "lineup_example")

  # The label survives the make_roc() wrapper too.
  p2 <- plot(make_roc(lineup_example, show_plot = FALSE))
  testthat::expect_match(p2$labels$caption, "lineup_example")
})

testthat::test_that("returned plots are ordinary ggplots and compose", {
  data(lineup_example, envir = environment())

  p <- plot(make_rocdata(lineup_example)) + ggplot2::theme_minimal()
  testthat::expect_s3_class(p, "ggplot")
})
