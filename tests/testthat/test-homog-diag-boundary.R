test_that("homog_diag() stops on pairs with non-finite or zero variance", {
  all_suspect <- list(rep(1, 30), rep(1, 30))
  expect_error(
    capture.output(homog_diag(all_suspect, all_suspect, c(1, 1), c(6, 6))),
    "undefined"
  )
  ok_tp <- list(c(rep(3, 30), rep(1:6, each = 5)), c(rep(2, 24), rep(1:6, each = 6)))
  ok_ta <- list(c(rep(3, 10), rep(1:6, each = 8)), c(rep(2, 12), rep(1:6, each = 8)))
  capture.output(res <- homog_diag(ok_tp, ok_ta, c(3, 2), c(6, 6)))
  expect_equal(round(res$mean_diagnosticity, 3), 1.679)
})
