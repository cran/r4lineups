test_that("correction = 'none' returns infinite scores at boundary rates, with a warning", {
  expect_warning(s <- sdt_summary_from_counts(20, 0, 0, 20, correction = "none"),
                 "infinite")
  expect_equal(s$dprime, Inf)
  expect_equal(s$hit_rate, 1)
  # Interior rates are unchanged and do not warn.
  expect_no_warning(s2 <- sdt_summary_from_counts(70, 20, 30, 80, correction = "none"))
  expect_equal(s2$dprime, qnorm(.7) - qnorm(.2))
  # Corrected routes stay finite.
  expect_true(is.finite(sdt_summary_from_counts(20, 0, 0, 20)$dprime))
})

test_that("make_multid() warns on boundary rates with correction = 'none'", {
  expect_warning(
    m <- make_multid(counts = c(G = 50, Fp = 50, Rp = 0, I = 10, Fa = 40, Ra = 50),
                     correction = "none", n_bootstrap = 0),
    "infinite"
  )
  expect_false(is.finite(m$indices$estimate[m$indices$index == "RaRp"]))
})
