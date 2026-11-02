# Tests for data standardization functions

# Helper to create valid test data
create_valid_data <- function() {
  data.frame(
    target_present = c(TRUE, TRUE, FALSE, FALSE),
    identification = c("suspect", "filler", "reject", "suspect"),
    confidence = c(5, 3, 2, 4),
    stringsAsFactors = FALSE
  )
}


# ============================================================================
# validate_lineup_data() tests
# ============================================================================

test_that("validate_lineup_data accepts valid data", {
  data <- create_valid_data()

  result <- validate_lineup_data(data, strict = FALSE)
  expect_true(result$valid)
  expect_equal(result$messages, "Data is valid")
})


test_that("validate_lineup_data detects missing columns", {
  data <- data.frame(
    target_present = c(TRUE, FALSE),
    confidence = c(5, 3)
  )

  result <- validate_lineup_data(data, strict = FALSE)
  expect_false(result$valid)
  expect_true(any(grepl("Missing required columns.*identification", result$messages)))
})


test_that("validate_lineup_data checks target_present type", {
  data <- create_valid_data()
  data$target_present <- c("yes", "yes", "no", "no")

  result <- validate_lineup_data(data, strict = FALSE)
  expect_false(result$valid)
  expect_true(any(grepl("target_present must be logical", result$messages)))
})


test_that("validate_lineup_data accepts 0/1 for target_present", {
  data <- create_valid_data()
  data$target_present <- c(1, 1, 0, 0)

  result <- validate_lineup_data(data, strict = FALSE)
  expect_true(result$valid)
})


test_that("validate_lineup_data detects invalid identification values", {
  data <- create_valid_data()
  data$identification[1] <- "invalid"

  result <- validate_lineup_data(data, strict = FALSE)
  expect_false(result$valid)
  expect_true(any(grepl("identification contains invalid values", result$messages)))
})


test_that("validate_lineup_data checks for missing values", {
  data <- create_valid_data()
  data$target_present[1] <- NA

  result <- validate_lineup_data(data, strict = FALSE)
  expect_false(result$valid)
  expect_true(any(grepl("target_present contains missing values", result$messages)))
})


test_that("validate_lineup_data warns about missing trial types", {
  # Only target-present trials
  data <- data.frame(
    target_present = c(TRUE, TRUE, TRUE),
    identification = c("suspect", "filler", "reject"),
    confidence = c(5, 3, 2)
  )

  result <- validate_lineup_data(data, strict = FALSE)
  expect_true(any(grepl("No target-absent trials", result$warnings)))
})


test_that("validate_lineup_data checks confidence is numeric", {
  data <- create_valid_data()
  data$confidence <- c("high", "medium", "low", "high")

  result <- validate_lineup_data(data, strict = FALSE)
  expect_false(result$valid)
  expect_true(any(grepl("confidence must be numeric", result$messages)))
})


test_that("validate_lineup_data detects negative confidence", {
  data <- create_valid_data()
  data$confidence[1] <- -1

  result <- validate_lineup_data(data, strict = FALSE)
  expect_false(result$valid)
  expect_true(any(grepl("confidence contains negative values", result$messages)))
})


test_that("validate_lineup_data strict mode stops on error", {
  data <- data.frame(
    target_present = c(TRUE, FALSE),
    confidence = c(5, 3)
  )

  expect_error(
    validate_lineup_data(data, strict = TRUE),
    "Missing required columns"
  )
})


test_that("validate_lineup_data works without confidence", {
  data <- data.frame(
    target_present = c(TRUE, TRUE, FALSE, FALSE),
    identification = c("suspect", "filler", "reject", "suspect")
  )

  result <- validate_lineup_data(data, require_confidence = FALSE, strict = FALSE)
  expect_true(result$valid)
})


test_that("validate_lineup_data warns about small sample size", {
  data <- data.frame(
    target_present = c(TRUE, FALSE),
    identification = c("suspect", "filler"),
    confidence = c(5, 3)
  )

  result <- validate_lineup_data(data, strict = FALSE)
  expect_true(any(grepl("Very small sample size", result$warnings)))
})


test_that("validate_lineup_data checks response_time when required", {
  data <- create_valid_data()

  result <- validate_lineup_data(data, require_response_time = TRUE, strict = FALSE)
  expect_false(result$valid)
  expect_true(any(grepl("Missing required columns.*response_time", result$messages)))
})


test_that("validate_lineup_data accepts factor identification", {
  data <- create_valid_data()
  data$identification <- factor(data$identification,
                                levels = c("suspect", "filler", "reject"))

  result <- validate_lineup_data(data, strict = FALSE)
  expect_true(result$valid)
})


# ============================================================================
# standardize_lineup_data() tests
# ============================================================================

test_that("standardize_lineup_data works with standard column names", {
  data <- create_valid_data()

  result <- standardize_lineup_data(data, validate = FALSE)

  expect_s3_class(result, "lineup_data")
  expect_true("target_present" %in% names(result))
  expect_true("identification" %in% names(result))
  expect_true("confidence" %in% names(result))
})


test_that("standardize_lineup_data renames columns", {
  data <- data.frame(
    tp = c(1, 1, 0, 0),
    response = c("suspect", "filler", "reject", "suspect"),
    conf = c(5, 3, 2, 4)
  )

  result <- suppressMessages(
    standardize_lineup_data(
      data,
      target_present_col = "tp",
      identification_col = "response",
      confidence_col = "conf",
      validate = FALSE
    )
  )

  expect_true("target_present" %in% names(result))
  expect_true("identification" %in% names(result))
  expect_true("confidence" %in% names(result))
  expect_true(is.logical(result$target_present))
})


test_that("standardize_lineup_data recodes identification values", {
  data <- data.frame(
    target_present = c(TRUE, TRUE, FALSE, FALSE),
    identification = c("target", "foil", "none", "target"),
    confidence = c(5, 3, 2, 4)
  )

  result <- standardize_lineup_data(
    data,
    recode_identification = c(
      "target" = "suspect",
      "foil" = "filler",
      "none" = "reject"
    ),
    validate = FALSE
  )

  expect_true(all(result$identification %in% c("suspect", "filler", "reject")))
  expect_equal(sum(result$identification == "suspect"), 2)
})


test_that("standardize_lineup_data applies default recoding", {
  data <- data.frame(
    target_present = c(TRUE, TRUE, FALSE, FALSE),
    identification = c("culprit", "distractor", "not present", "target"),
    confidence = c(5, 3, 2, 4)
  )

  result <- standardize_lineup_data(data, validate = FALSE)

  expect_true(all(result$identification %in% c("suspect", "filler", "reject")))
})


test_that("standardize_lineup_data generates participant_id if missing", {
  data <- create_valid_data()

  result <- standardize_lineup_data(data, validate = FALSE)

  expect_true("participant_id" %in% names(result))
  expect_equal(length(unique(result$participant_id)), nrow(result))
})


test_that("standardize_lineup_data preserves participant_id if present", {
  data <- create_valid_data()
  data$participant_id <- c("A", "B", "C", "D")

  result <- standardize_lineup_data(data, validate = FALSE)

  expect_equal(result$participant_id, c("A", "B", "C", "D"))
})


test_that("standardize_lineup_data handles response_time", {
  data <- create_valid_data()
  data$rt <- c(2000, 3000, 1500, 2500)

  result <- suppressMessages(
    standardize_lineup_data(data, response_time_col = "rt", validate = FALSE)
  )

  expect_true("response_time" %in% names(result))
  expect_equal(result$response_time, c(2000, 3000, 1500, 2500))
})


test_that("standardize_lineup_data auto-detects common column names", {
  data <- data.frame(
    tp = c(TRUE, TRUE, FALSE, FALSE),
    choice = c("suspect", "filler", "reject", "suspect"),
    cert = c(5, 3, 2, 4)
  )

  result <- suppressMessages(
    standardize_lineup_data(data, validate = FALSE)
  )

  expect_true(all(c("target_present", "identification", "confidence") %in% names(result)))
})


test_that("standardize_lineup_data validates by default", {
  # Invalid data
  data <- data.frame(
    target_present = c(TRUE, FALSE),
    identification = c("invalid", "filler"),
    confidence = c(5, 3)
  )

  expect_warning(
    standardize_lineup_data(data),
    "Standardized data failed validation"
  )
})


test_that("standardize_lineup_data can skip validation", {
  data <- data.frame(
    target_present = c(TRUE, FALSE),
    identification = c("invalid", "filler"),
    confidence = c(5, 3)
  )

  expect_silent(
    standardize_lineup_data(data, validate = FALSE)
  )
})


test_that("standardize_lineup_data stops if data not a data frame", {
  expect_error(
    standardize_lineup_data(c(1, 2, 3)),
    "data must be a data frame"
  )
})


test_that("standardize_lineup_data stops if key columns missing", {
  data <- data.frame(
    some_col = c(1, 2, 3),
    another_col = c("a", "b", "c")
  )

  expect_error(
    standardize_lineup_data(data),
    "Cannot find target_present column"
  )
})


# ============================================================================
# print.lineup_data() tests
# ============================================================================

test_that("print method works for lineup_data", {
  data <- create_valid_data()
  class(data) <- c("lineup_data", "data.frame")

  output <- capture.output(print(data))

  expect_true(any(grepl("Standardized Lineup Data", output)))
  expect_true(any(grepl("Target-present trials", output)))
  expect_true(any(grepl("Identifications", output)))
})


# ============================================================================
# create_example_lineup_data() tests
# ============================================================================

test_that("create_example_lineup_data generates valid data", {
  data <- create_example_lineup_data(n_trials = 100, seed = 123)

  expect_s3_class(data, "lineup_data")
  expect_equal(nrow(data), 100)

  validation <- validate_lineup_data(data, strict = FALSE)
  expect_true(validation$valid)
})


test_that("create_example_lineup_data respects n_trials", {
  data <- create_example_lineup_data(n_trials = 50, seed = 123)
  expect_equal(nrow(data), 50)

  data <- create_example_lineup_data(n_trials = 200, seed = 123)
  expect_equal(nrow(data), 200)
})


test_that("create_example_lineup_data respects prop_target_present", {
  data <- create_example_lineup_data(
    n_trials = 1000,
    prop_target_present = 0.7,
    seed = 456
  )

  prop_tp <- mean(data$target_present)
  # Should be close to 0.7 (within 5% with large sample)
  expect_true(abs(prop_tp - 0.7) < 0.05)
})


test_that("create_example_lineup_data includes confidence by default", {
  data <- create_example_lineup_data(n_trials = 50, seed = 123)

  expect_true("confidence" %in% names(data))
  expect_true(all(data$confidence >= 1))
  expect_true(all(data$confidence <= 5))
})


test_that("create_example_lineup_data can exclude confidence", {
  data <- create_example_lineup_data(
    n_trials = 50,
    include_confidence = FALSE,
    seed = 123
  )

  expect_false("confidence" %in% names(data))
})


test_that("create_example_lineup_data includes response_time when requested", {
  data <- create_example_lineup_data(
    n_trials = 50,
    include_response_time = TRUE,
    seed = 123
  )

  expect_true("response_time" %in% names(data))
  expect_true(all(data$response_time > 0))
})


test_that("create_example_lineup_data is reproducible with seed", {
  data1 <- create_example_lineup_data(n_trials = 50, seed = 789)
  data2 <- create_example_lineup_data(n_trials = 50, seed = 789)

  expect_equal(data1$target_present, data2$target_present)
  expect_equal(data1$identification, data2$identification)
  expect_equal(data1$confidence, data2$confidence)
})


test_that("create_example_lineup_data has all identification types", {
  data <- create_example_lineup_data(n_trials = 200, seed = 999)

  ids <- unique(data$identification)
  expect_true("suspect" %in% ids)
  expect_true("filler" %in% ids)
  expect_true("reject" %in% ids)
})


test_that("create_example_lineup_data has participant_id", {
  data <- create_example_lineup_data(n_trials = 50, seed = 123)

  expect_true("participant_id" %in% names(data))
  expect_equal(length(unique(data$participant_id)), 50)
})


# ============================================================================
# Integration tests with r4lineups functions
# ============================================================================

test_that("standardized data works with make_roc", {
  data <- create_example_lineup_data(n_trials = 200, seed = 111)

  expect_silent({
    roc <- make_roc(data, lineup_size = 6, show_plot = FALSE)
  })

  expect_true("pauc" %in% names(roc))
})


test_that("standardized data works with make_cac", {
  data <- create_example_lineup_data(n_trials = 200, seed = 222)

  # The simulated arms are unequal, so count-based accuracy carries a note
  expect_message({
    cac <- make_cac(data, lineup_size = 6, show_plot = FALSE)
  }, "differ in size")
  expect_silent({
    cac <- make_cac(data, lineup_size = 6, show_plot = FALSE, base_rate = 0.5)
  })

  expect_true("overall_accuracy" %in% names(cac))
})


test_that("standardized data works with fit_sdt_roc", {
  data <- create_example_lineup_data(n_trials = 200, seed = 333)

  expect_silent({
    sdt <- fit_sdt_roc(data, lineup_size = 6, bootstrap = FALSE)
  })

  expect_s3_class(sdt, "sdt_roc_fit")
})


test_that("validation catches data incompatible with r4lineups", {
  # Data with only one trial type
  data <- data.frame(
    target_present = rep(TRUE, 10),
    identification = rep("suspect", 10),
    confidence = rep(5, 10)
  )

  validation <- validate_lineup_data(data, strict = FALSE)
  expect_true(any(grepl("No target-absent trials", validation$warnings)))
})


test_that("standardize handles real-world messy data", {
  # Simulate messy real-world data
  messy_data <- data.frame(
    Culprit_Present = c("Yes", "Yes", "No", "No", "Yes"),
    ParticipantChoice = c("Target", "Foil", "None", "Target", "distractor"),
    Certainty = c("5", "3", "2", "4", "3"),
    SubjectID = c("S01", "S02", "S03", "S04", "S05")
  )

  # Convert to usable format
  messy_data$Culprit_Present <- messy_data$Culprit_Present == "Yes"
  messy_data$Certainty <- as.numeric(messy_data$Certainty)

  result <- suppressMessages(
    standardize_lineup_data(
      messy_data,
      target_present_col = "Culprit_Present",
      identification_col = "ParticipantChoice",
      confidence_col = "Certainty",
      participant_id_col = "SubjectID",
      validate = FALSE
    )
  )

  expect_s3_class(result, "lineup_data")
  expect_true(all(result$identification %in% c("suspect", "filler", "reject")))
  expect_equal(result$participant_id, c("S01", "S02", "S03", "S04", "S05"))
})

test_that("standardize_lineup_data keeps the columns that fix the design", {
  # Codex review 2, finding 2: dropping suspect_pos switched a designated
  # suspect who was never chosen to the filler estimate (false-ID rate 0 -> .10).
  data <- data.frame(
    target_present = rep(c(TRUE, FALSE), each = 100),
    identification = c(rep(c("suspect", "filler", "reject"), c(60, 20, 20)),
                       rep(c("filler", "reject"), c(60, 40))),
    confidence = rep(c(90, 70, 50, 30), 50),
    suspect_pos = 1,
    lineup_member = c(rep(c(1, 2, 0), c(60, 20, 20)), rep(c(2, 0), c(60, 40))),
    condition = "a"
  )
  std <- standardize_lineup_data(data, validate = FALSE)
  expect_equal(std$suspect_pos, data$suspect_pos)
  expect_equal(std$lineup_member, data$lineup_member)
  expect_equal(std$condition, data$condition)

  before <- make_rocdata(data)
  after <- make_rocdata(std)
  expect_equal(after$innocent_suspect_method, "designated")
  expect_equal(after$roc_data, before$roc_data)
  expect_equal(max(after$roc_data$false_id_rate), 0)

  # Member choices drive the effective-size correction.
  ppv_before <- suppressMessages(ppv_by_confidence(
    data, correction = "effective", innocent_suspect = "estimated"))
  ppv_after <- suppressMessages(ppv_by_confidence(
    std, correction = "effective", innocent_suspect = "estimated"))
  expect_equal(ppv_after, ppv_before)
})
