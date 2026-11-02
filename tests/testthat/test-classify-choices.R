test_that("classify_lineup_choices derives the three outcomes correctly", {
  trials <- data.frame(
    lineup_member = c(3L, 0L, 5L, 3L, 1L),
    suspect_pos   = 3L
  )

  out <- classify_lineup_choices(trials)

  expect_equal(out$identification,
               c("suspect", "reject", "filler", "suspect", "filler"))
  expect_equal(nrow(out), nrow(trials))
})

test_that("NA choices are treated as rejections", {
  trials <- data.frame(lineup_member = c(2, NA, 0), suspect_pos = 2)
  expect_equal(classify_lineup_choices(trials)$identification,
               c("suspect", "reject", "reject"))
})

test_that("suspect position may be given as a scalar or a column", {
  trials <- data.frame(lineup_member = c(2L, 4L, 0L))

  by_scalar <- classify_lineup_choices(trials, suspect_col = 2)
  by_column <- classify_lineup_choices(
    cbind(trials, suspect_pos = 2L)
  )

  expect_equal(by_scalar$identification, c("suspect", "filler", "reject"))
  expect_equal(by_scalar$identification, by_column$identification)
})

test_that("a suspect that moves between lineups is handled per row", {
  trials <- data.frame(
    lineup_member = c(1L, 1L, 4L),
    suspect_pos   = c(1L, 2L, 4L)
  )
  expect_equal(classify_lineup_choices(trials)$identification,
               c("suspect", "filler", "suspect"))
})

test_that("null foils are reported when k is supplied", {
  trials <- data.frame(lineup_member = c(1L, 2L, 2L, 0L), suspect_pos = 1L)

  out <- classify_lineup_choices(trials, k = 6)

  # Positions 3-6 were never chosen; the rejection (0) is not a position
  expect_equal(attr(out, "null_foils"), 3:6)
})

test_that("k is not reported when not requested", {
  trials <- data.frame(lineup_member = c(1L, 2L), suspect_pos = 1L)
  expect_null(attr(classify_lineup_choices(trials), "null_foils"))
})

test_that("invalid input is rejected rather than silently coerced", {
  ok <- data.frame(lineup_member = c(1L, 2L), suspect_pos = 1L)

  expect_error(classify_lineup_choices(list(a = 1)), "must be a data frame")
  expect_error(classify_lineup_choices(ok[0, ]), "no rows")
  expect_error(classify_lineup_choices(data.frame(x = 1)), "not found in data")
  expect_error(
    classify_lineup_choices(data.frame(lineup_member = "a", suspect_pos = 1)),
    "must be numeric"
  )
  expect_error(
    classify_lineup_choices(data.frame(lineup_member = -1, suspect_pos = 1)),
    "negative positions"
  )
  expect_error(classify_lineup_choices(ok, k = 1), "at least 2")
  expect_error(classify_lineup_choices(ok, k = 1.5), "at least 2")
  expect_error(
    classify_lineup_choices(data.frame(lineup_member = 9L, suspect_pos = 1L), k = 6),
    "greater than k"
  )
  expect_error(
    classify_lineup_choices(data.frame(lineup_member = 1L, suspect_pos = 0L)),
    "at least 1"
  )
})

test_that("an existing outcome column is protected unless overwrite is set", {
  trials <- data.frame(lineup_member = 1L, suspect_pos = 1L,
                       identification = "stale")

  expect_error(classify_lineup_choices(trials), "already exists")
  expect_equal(
    classify_lineup_choices(trials, overwrite = TRUE)$identification,
    "suspect"
  )
})

test_that("the outcome column can be written elsewhere", {
  trials <- data.frame(lineup_member = 1L, suspect_pos = 1L)
  out <- classify_lineup_choices(trials, into = "outcome")

  expect_equal(out$outcome, "suspect")
  expect_false("identification" %in% names(out))
})


# --- The data model guard -------------------------------------------------
# lineup_example gained member-level columns without any existing value
# moving. These tests are what makes that claim checkable.

test_that("lineup_example records member-level choices", {
  data(lineup_example, envir = environment())

  expect_true(all(c("lineup_member", "suspect_pos") %in% names(lineup_example)))
  expect_type(lineup_example$lineup_member, "integer")
  expect_true(all(lineup_example$lineup_member %in% 0:6))
  expect_true(all(lineup_example$suspect_pos == 1L))
})

test_that("lineup_example identification is exactly recoverable from raw choices", {
  data(lineup_example, envir = environment())

  round_trip <- classify_lineup_choices(
    lineup_example[, c("lineup_member", "suspect_pos")],
    k = 6
  )

  expect_identical(round_trip$identification, lineup_example$identification)
})

test_that("lineup_example foils are not interchangeable", {
  data(lineup_example, envir = environment())

  foils <- lineup_example$lineup_member[
    lineup_example$identification == "filler"
  ]
  counts <- table(factor(foils, levels = 2:6))

  # Every foil was chosen at least once, but at markedly different rates
  expect_true(all(counts > 0))
  expect_gt(max(counts) / min(counts), 2)
})

test_that("simulate_lineup_data emits the chosen member", {
  sim <- simulate_lineup_data(n_tp = 40, n_ta = 40, d_prime = 1.5,
                              lineup_size = 6, conf_levels = 3, seed = 1)

  expect_true(all(c("lineup_member", "suspect_pos") %in% names(sim)))
  expect_true(all(sim$lineup_member %in% 0:6))

  # Rejections carry no member; identifications always do
  expect_true(all(sim$lineup_member[sim$identification == "reject"] == 0L))
  expect_true(all(sim$lineup_member[sim$identification != "reject"] > 0L))

  # And the derived label agrees with the raw choice
  round_trip <- classify_lineup_choices(
    as.data.frame(sim)[, c("lineup_member", "suspect_pos")],
    k = 6
  )
  expect_identical(round_trip$identification, sim$identification)
})
