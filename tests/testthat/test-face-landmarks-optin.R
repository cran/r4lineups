# Opt-in: needs the Python face stack (install_r4lineups_python()) and, on
# first run, network access to download the MediaPipe model. Run with
# R4LINEUPS_RUN_FACE=true and RETICULATE_PYTHON pointing at that environment.
test_that("get_face_landmarks returns the 478-point mesh", {
  skip_if_not(identical(Sys.getenv("R4LINEUPS_RUN_FACE"), "true"),
              "Set R4LINEUPS_RUN_FACE=true to run the face-landmark test")
  skip_on_cran()
  img <- test_path("..", "..", "vignettes", "lineupimage2.png")
  skip_if_not(file.exists(img), "Example image not available")
  expect_false(identical(attr(check_python_deps(verbose = FALSE), "face_mesh"),
                         "unavailable"))
  mesh <- get_face_landmarks(img)
  expect_true(mesh$face_detected)
  expect_equal(dim(mesh$landmarks), c(478L, 3L))
  expect_equal(colnames(mesh$landmarks), c("x", "y", "z"))
  expect_true(all(mesh$landmarks[, 1:2] >= 0 & mesh$landmarks[, 1:2] <= 1))
  expect_equal(dim(get_face_landmarks(img, return_3d = FALSE)$landmarks), c(478L, 2L))
})
