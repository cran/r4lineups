# Package-level variables to store Python modules
.faceR_env <- new.env(parent = emptyenv())

#' @importFrom reticulate import py_available py_module_available
.onLoad <- function(libname, pkgname) {

  # Delay-load Python modules - they'll be loaded on first use
  .faceR_env$deepface <- NULL
  .faceR_env$cv2 <- NULL
  .faceR_env$numpy <- NULL
  .faceR_env$mediapipe <- NULL
  .faceR_env$python_ready <- FALSE
}

#' Initialize Python modules
#' 
#' Internal function to load Python dependencies on first use
#' @keywords internal
init_python <- function() {

  # Ensure legacy Keras behavior for DeepFace compatibility
  if (Sys.getenv("TF_USE_LEGACY_KERAS") == "") {
    Sys.setenv(TF_USE_LEGACY_KERAS = "1")
  }

  if (isTRUE(.faceR_env$python_ready)) {
    return(invisible(TRUE))
  }
  
  if (reticulate::py_available(initialize = FALSE)) {
    warning(
      "Python is already initialized. If you see KerasTensor errors, ",
      "restart R and set TF_USE_LEGACY_KERAS=1 before loading r4lineups.",
      call. = FALSE
    )
  }

  if (!reticulate::py_available(initialize = TRUE)) {
    stop(
      "Python is not available. Please install Python and ensure it's on your PATH.\n",
      "See ?install_r4lineups_python for setup instructions.",
      call. = FALSE
    )
  }
  
  # Check for required modules

  required <- c("deepface", "cv2", "numpy")
  missing <- character()
  

  for (mod in required) {
    mod_name <- if (mod == "cv2") "cv2" else mod
    if (!reticulate::py_module_available(mod_name)) {
      missing <- c(missing, mod)
    }
  }
  
  if (length(missing) > 0) {
    stop(
      "Missing required Python packages: ", paste(missing, collapse = ", "), "\n",
      "Run install_r4lineups_python() to install them, or manually:\n",
      "  pip install deepface opencv-python numpy tf-keras\n",
      "These require Python ", .tf_python_min, " to ", .tf_python_max,
      " (TensorFlow wheel availability); see ?install_r4lineups_python.",
      call. = FALSE
    )
  }
  
  # Import modules
  # Attempt to import tf_keras if available (ensures legacy Keras path)
  if (reticulate::py_module_available("tf_keras")) {
    reticulate::import("tf_keras", delay_load = FALSE)
  }
  .faceR_env$deepface <- reticulate::import("deepface.DeepFace", delay_load = FALSE)
  .faceR_env$DeepFace <- .faceR_env$deepface
  .faceR_env$cv2 <- reticulate::import("cv2", delay_load = FALSE)
  .faceR_env$numpy <- reticulate::import("numpy", delay_load = FALSE)
  
  # MediaPipe is optional
  if (reticulate::py_module_available("mediapipe"))

{
    .faceR_env$mediapipe <- reticulate::import("mediapipe", delay_load = FALSE)
  }
  
  .faceR_env$python_ready <- TRUE
  invisible(TRUE)
}

#' Get deepface module
#' @keywords internal
get_deepface <- function() {
  init_python()
  .faceR_env$deepface
}

#' Get cv2 module  
#' @keywords internal
get_cv2 <- function() {
  init_python()
  .faceR_env$cv2
}

#' Get numpy module
#' @keywords internal
get_numpy <- function() {
init_python()
  .faceR_env$numpy
}

#' Get mediapipe module (may be NULL if not installed)
#' @keywords internal
get_mediapipe <- function() {
  init_python()
  .faceR_env$mediapipe
}
