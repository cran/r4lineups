roc_area <- r4lineups:::.roc_area

test_that("vertical segments keep both endpoints", {
  # Vertical at the origin, then a slope from (0, 0.5) to (0.1, 1)
  expect_equal(roc_area(c(0, 0, 0.1), c(0, 0.5, 1)), 0.075)
  # Vertical at an intermediate false-ID rate
  expect_equal(roc_area(c(0, 0.1, 0.1, 0.2), c(0, 0.2, 0.6, 0.8)),
               0.1 * 0.1 + 0.1 * 0.7)
  # Input order does not matter
  expect_equal(roc_area(c(0.1, 0, 0.2, 0.1), c(0.6, 0, 0.8, 0.2)),
               0.1 * 0.1 + 0.1 * 0.7)
})

test_that("the cutoff is handled exactly and inside sloping segments", {
  far <- c(0, 0.1, 0.1, 0.3)
  hr <- c(0, 0.4, 0.6, 1)
  # Cutoff exactly at a vertical segment: area up to 0.1 only
  expect_equal(roc_area(far, hr, 0.1), 0.1 * 0.2)
  # Cutoff inside the sloping segment from (0.1, 0.6) to (0.3, 1)
  expect_equal(roc_area(far, hr, 0.2), 0.02 + 0.1 * (0.6 + 0.8) / 2)
  expect_equal(roc_area(far, hr, 0), 0)
  expect_equal(roc_area(far, hr, Inf), roc_area(far, hr, 0.3))
})

test_that("compare_pauc and make_rocdata agree on the reviewer's example", {
  d <- data.frame(
    target_present = rep(c(TRUE, FALSE), each = 10),
    identification = c(rep("suspect", 10), "suspect", rep("reject", 9)),
    confidence = c(rep(90, 5), rep(50, 5), 70, rep(30, 9))
  )
  roc <- make_rocdata(d)
  expect_equal(roc$pauc, 0.05)
  cmp <- suppressMessages(compare_pauc(d, d, n_bootstrap = 20, seed = 1))
  expect_equal(cmp$pauc1, 0.05)
  expect_equal(cmp$pauc1, roc$pauc)
})
