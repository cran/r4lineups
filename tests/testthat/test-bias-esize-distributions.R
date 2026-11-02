testthat::test_that("lineup_bias_boot_dist returns expected length", {
  set.seed(1)
  vec <- sample(1:6, 200, replace = TRUE)
  dist <- lineup_bias_boot_dist(vec, target_pos = 3, k = 6, R = 200)
  testthat::expect_length(dist, 200)
})

testthat::test_that("esize_boot_dist returns expected length", {
  set.seed(1)
  vec <- sample(1:6, 200, replace = TRUE)
  dist_t <- esize_boot_dist(vec, k = 6, metric = "tredoux", R = 200)
  dist_m <- esize_boot_dist(vec, k = 6, metric = "malpass", R = 200)
  testthat::expect_length(dist_t, 200)
  testthat::expect_length(dist_m, 200)
})

testthat::test_that("bootstrap distributions record the resampled sample size", {
  set.seed(1)
  vec <- sample(1:6, 200, replace = TRUE)
  testthat::expect_equal(
    attr(lineup_bias_boot_dist(vec, target_pos = 3, k = 6, R = 50), "n"), 200
  )
  testthat::expect_equal(
    attr(esize_boot_dist(vec, k = 6, R = 50), "n"), 200
  )
})

testthat::test_that("boot_lattice_binwidth recovers the lattice spacing", {
  # Values on a 1/n lattice: spacing is returned.
  lattice <- c(0, 1, 2, 3, 5, 7) / 94
  testthat::expect_equal(boot_lattice_binwidth(lattice), 1 / 94)

  # An explicit n fixes the spacing exactly, even when the observed values
  # happen to skip a lattice position.
  testthat::expect_equal(
    boot_lattice_binwidth(c(0, 2, 4, 6) / 94, n = 94), 1 / 94
  )
})

testthat::test_that("boot_lattice_binwidth declines non-lattice input", {
  set.seed(1)
  # Continuous values: no lattice to bin on.
  testthat::expect_null(boot_lattice_binwidth(stats::runif(1000)))
  # Too few distinct values to establish a spacing.
  testthat::expect_null(boot_lattice_binwidth(c(0.1, 0.2)))
  # On a lattice, but far too fine to draw one bar per attainable value.
  testthat::expect_null(boot_lattice_binwidth(seq_len(500) / 10000))
})

testthat::test_that("bias plot bins a discrete proportion on its lattice", {
  set.seed(1)
  vec <- sample(1:8, 94, replace = TRUE)
  dist <- lineup_bias_boot_dist(vec, target_pos = 2, k = 8, R = 500)

  layer_data <- ggplot2::ggplot_build(
    plot_lineup_bias_distribution(dist, target_pos = 2)
  )$data[[1]]

  # One bar per attainable proportion, so the bin width is exactly 1/n.
  testthat::expect_equal(unique(round(diff(layer_data$x), 8)),
                         round(1 / 94, 8))

  # Every bar covers exactly one attainable value, so no bar is empty purely
  # because it fell between two of them.
  attainable <- sort(unique(as.vector(dist)))
  testthat::expect_equal(sum(layer_data$count > 0), length(attainable))
})

testthat::test_that("bias plot honours explicit bins and binwidth", {
  set.seed(1)
  vec <- sample(1:8, 94, replace = TRUE)
  dist <- lineup_bias_boot_dist(vec, target_pos = 2, k = 8, R = 500)

  by_bins <- ggplot2::ggplot_build(
    plot_lineup_bias_distribution(dist, bins = 15)
  )$data[[1]]
  testthat::expect_equal(nrow(by_bins), 15)

  by_width <- ggplot2::ggplot_build(
    plot_lineup_bias_distribution(dist, binwidth = 0.02)
  )$data[[1]]
  testthat::expect_equal(unique(round(diff(by_width$x), 8)), 0.02)
})

testthat::test_that("esize plot falls back to fixed bins when near-continuous", {
  set.seed(1)
  vec <- sample(1:8, 94, replace = TRUE)
  dist <- esize_boot_dist(vec, k = 8, metric = "tredoux", R = 500)

  testthat::expect_null(boot_lattice_binwidth(as.vector(dist)))
  layer_data <- ggplot2::ggplot_build(
    plot_esize_distribution(dist)
  )$data[[1]]
  testthat::expect_equal(nrow(layer_data), 30)
})

test_that("esize_boot_dist can bootstrap the corrected E'", {
  set.seed(3)
  vec <- sample(1:6, 60, replace = TRUE)
  set.seed(4)
  corrected <- esize_boot_dist(vec, k = 6, R = 200, unbiased = TRUE)
  set.seed(4)
  plugin <- esize_boot_dist(vec, k = 6, R = 200)
  expect_length(corrected, 200)
  expect_equal(attr(corrected, "n_undefined"), 0)
  # The correction always raises E' (n(n-1)/sum(o(o-1)) > n^2/sum(o^2))
  expect_true(all(corrected > plugin))
  expect_error(esize_boot_dist(vec, k = 6, metric = "malpass", unbiased = TRUE),
               "tredoux")
  # Two choices on different members: every resample with both is unbounded
  set.seed(5)
  expect_warning(few <- esize_boot_dist(c(1, 2), k = 6, R = 100, unbiased = TRUE),
                 "unbounded")
  expect_length(few, 100)
  expect_equal(sum(is.infinite(few)), attr(few, "n_undefined"))
})
