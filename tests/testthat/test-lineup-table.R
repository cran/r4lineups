testthat::test_that("lineup_table keeps unchosen members in the table", {
  # Position 4 is never chosen, so plain table() drops it entirely.
  lineup_vec <- rep(c(5, 3, 2, 1, 8, 6, 7), each = 4)
  testthat::expect_length(table(lineup_vec), 7)

  lt <- lineup_table(lineup_vec, k = 8)
  testthat::expect_length(lt, 8)
  testthat::expect_equal(as.integer(lt), c(4, 4, 4, 0, 4, 4, 4, 4))
  testthat::expect_equal(names(lt), as.character(1:8))
})

testthat::test_that("lineup_table inherits from table so consumers still work", {
  lineup_vec <- rep(c(5, 3, 2, 1, 8, 6, 7), each = 4)
  lt <- lineup_table(lineup_vec, k = 8)

  testthat::expect_s3_class(lt, "lineup_table")
  testthat::expect_s3_class(lt, "table")

  # Same numbers as the existing idiom, just with the null foil visible.
  testthat::expect_equal(esize_m(lt, k = 8), esize_m(table(lineup_vec), k = 8))
  testthat::expect_equal(esize_T(lt), esize_T(table(lineup_vec)))
  testthat::expect_true(datacheck3(lt, 8))
})

testthat::test_that("lineup_table reports null foils and the suspect", {
  lineup_vec <- rep(c(5, 3, 2, 1, 8, 6, 7), each = 4)
  out <- utils::capture.output(print(lineup_table(lineup_vec, k = 8,
                                                  suspect_pos = 3)))
  out <- paste(out, collapse = "\n")

  testthat::expect_match(out, "1 null foil \\(position 4\\)")
  testthat::expect_match(out, "nominated suspect")
  testthat::expect_match(out, "3\\*")
})

testthat::test_that("lineup_table says nothing about null foils when there are none", {
  lineup_vec <- rep(1:8, each = 4)
  out <- paste(utils::capture.output(print(lineup_table(lineup_vec, k = 8))),
               collapse = "\n")

  testthat::expect_false(grepl("null foil", out))
  testthat::expect_false(grepl("nominated suspect", out))
})

testthat::test_that("lineup_table names every null foil when several are unchosen", {
  lineup_vec <- rep(c(1, 2, 5), each = 4)
  out <- paste(utils::capture.output(print(lineup_table(lineup_vec, k = 6))),
               collapse = "\n")

  testthat::expect_match(out, "3 null foils \\(positions 3, 4, 6\\)")
})

testthat::test_that("lineup_table validates its arguments", {
  lineup_vec <- rep(c(5, 3, 2, 1, 8, 6, 7), each = 4)

  # Choices outside 1..k
  testthat::expect_error(lineup_table(lineup_vec, k = 4))
  # Suspect position outside the lineup
  testthat::expect_error(lineup_table(lineup_vec, k = 8, suspect_pos = 99),
                         "suspect_pos")
  testthat::expect_error(lineup_table(lineup_vec, k = 8, suspect_pos = c(1, 2)),
                         "suspect_pos")
})
