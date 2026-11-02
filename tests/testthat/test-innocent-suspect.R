# A designated innocent suspect who is never chosen is still designated
zero_designated <- function(suspect_pos = TRUE) {
  tp <- data.frame(target_present = TRUE,
                   identification = c(rep("suspect", 60), rep("filler", 20),
                                      rep("reject", 20)),
                   confidence = 80)
  ta <- data.frame(target_present = FALSE,
                   identification = c(rep("filler", 60), rep("reject", 40)),
                   confidence = 80)
  d <- rbind(tp, ta)
  if (suspect_pos) d$suspect_pos <- 1
  d
}

test_that("a suspect_pos column marks a designated suspect with zero TA IDs", {
  r <- make_rocdata(zero_designated())
  expect_equal(r$innocent_suspect_method, "designated")
  expect_equal(max(r$roc_data$false_id_rate), 0)
})

test_that("without suspect_pos, auto estimates from fillers and says so", {
  d <- zero_designated(suspect_pos = FALSE)
  expect_message(r <- make_rocdata(d), "no designated innocent suspect")
  expect_equal(r$innocent_suspect_method, "estimated_from_fillers")
  expect_equal(max(r$roc_data$false_id_rate), 0.1)
})

test_that("the design can be set explicitly", {
  d <- zero_designated(suspect_pos = FALSE)
  r <- expect_silent(make_rocdata(d, innocent_suspect = "designated"))
  expect_equal(max(r$roc_data$false_id_rate), 0)
  r <- make_rocdata(zero_designated(), innocent_suspect = "estimated")
  expect_equal(r$innocent_suspect_method, "estimated_from_fillers")
  expect_equal(max(r$roc_data$false_id_rate), 0.1)
  expect_error(make_rocdata(d, innocent_suspect = "maybe"), "innocent_suspect")
})

test_that("the argument reaches the other entry points", {
  d <- zero_designated(suspect_pos = FALSE)
  expect_equal(make_cacdata(d, innocent_suspect = "designated")$innocent_suspect_method,
               "designated")
  expect_equal(make_fullroc_data(d, innocent_suspect = "designated")$innocent_suspect_method,
               "designated")
  expect_equal(make_utility_curves(d, innocent_suspect = "designated")$innocent_suspect_method,
               "designated")
  expect_equal(ppv_by_confidence(d, innocent_suspect = "designated",
                                 quiet = TRUE)$innocent_suspect_method,
               "designated")
  m <- make_multid(d, innocent_suspect = "designated")
  m_est <- suppressMessages(make_multid(d))
  expect_false(isTRUE(all.equal(m$indices, m_est$indices)))
})

# One TA suspect ID among ten TA rows: many resamples contain none, and must
# not switch to the filler estimate.
rare_suspect <- function() {
  data.frame(
    target_present = rep(c(TRUE, FALSE), each = 10),
    identification = c(rep("suspect", 6), rep("filler", 2), rep("reject", 2),
                       "suspect", rep("filler", 5), rep("reject", 4)),
    confidence = c(90, 80, 70, 60, 50, 40, 70, 60, 50, 40,
                   60, 90, 80, 70, 60, 50, 40, 30, 20, 10)
  )
}

test_that("bootstraps resolve the design once on the full data", {
  d <- rare_suspect()
  expect_no_message(
    compare_pauc(d, d, n_bootstrap = 50, seed = 1, max_false_id_rate = 0.1),
    message = "no designated innocent suspect"
  )
  expect_no_message(
    fit_sdt_roc(d, lineup_size = 6, n_bootstrap = 50, seed = 1),
    message = "no designated innocent suspect"
  )
})
