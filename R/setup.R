#' Install Python Dependencies for r4lineups
#'
#' Installs the required Python packages using pip. This function should be run
#' once after installing the r4lineups package.
#'
#' @param method Installation method. Default is "auto" which uses pip.
#' @param envname Name of virtual environment to use (optional). If NULL,
#'   installs to the system Python or currently active environment.
#' @param pip_options Additional options to pass to pip (e.g., "--user").
#' @param gpu Logical. If TRUE, installs tensorflow with GPU support.
#'   Default is FALSE.
#' @param python_version Python version to build the virtual environment with,
#'   e.g. \code{"3.11"}. Defaults to \code{NULL}, which leaves the choice to
#'   reticulate but still checks the result against the supported range.
#'
#' @details
#' This function installs:
#' \itemize{
#'   \item deepface - Main face analysis library
#'   \item opencv-python - Image processing
#'   \item numpy - Numerical computing
#'   \item tf-keras - TensorFlow/Keras backend
#'   \item mediapipe - For 478-point face mesh (optional but recommended)
#'   \item retinaface - Best face detector
#' }
#'
#' \strong{Python version.} TensorFlow publishes wheels for a limited range of
#' Python versions (currently 3.9 to 3.12). On a machine whose default
#' interpreter is newer than that, pip finds no TensorFlow distribution at all
#' and the install fails partway through with
#' \code{No matching distribution found for tensorflow}, after downloading the
#' other packages. This function therefore checks the interpreter before
#' installing anything, and stops with an explanatory message if it is outside
#' the supported range. Pass \code{python_version} to select a suitable
#' interpreter, which requires \code{envname} so that a virtual environment can
#' be built from it.
#'
#' @return \code{TRUE}, invisibly.
#'
#' @examples
#' \dontrun{
#' # Basic installation
#' install_r4lineups_python()
#'
#' # Install to a virtual environment
#' install_r4lineups_python(envname = "r4lineups-face-env")
#'
#' # Build the environment with a TensorFlow-supported interpreter
#' install_r4lineups_python(envname = "r4lineups-face-env",
#'                          python_version = "3.11")
#'
#' # Install with GPU support
#' install_r4lineups_python(gpu = TRUE)
#' }
#'
#' @export
install_r4lineups_python <- function(method = "auto",
                                  envname = NULL,
                                  pip_options = NULL,
                                  gpu = FALSE,
                                  python_version = NULL) {

  packages <- c(
    "deepface",
    "opencv-python",
    "numpy",
    "tf-keras",
    "mediapipe",
    "retina-face"
  )

  # Add tensorflow with appropriate variant
  if (gpu) {
    packages <- c(packages, "tensorflow[and-cuda]")
    message("Installing with GPU support...")
  } else {
    packages <- c(packages, "tensorflow")
  }

  if (!is.null(python_version) && is.null(envname)) {
    stop("python_version requires envname, so that a virtual environment can ",
         "be created from that interpreter.", call. = FALSE)
  }

  message("Installing Python dependencies for r4lineups...")
  message("Packages: ", paste(packages, collapse = ", "))

  if (!is.null(envname)) {
    # Create virtual environment if it doesn't exist
    if (!reticulate::virtualenv_exists(envname)) {
      # Check that an interpreter of the requested version actually exists
      # before handing off to reticulate, which otherwise fails with
      # "Suitable Python installation for creating a venv not found" and does
      # not say how to obtain one.
      if (!is.null(python_version)) {
        starter <- tryCatch(
          reticulate::virtualenv_starter(python_version),
          error = function(e) character(0)
        )
        if (length(starter) == 0L || !nzchar(starter[[1L]])) {
          available <- tryCatch(reticulate::virtualenv_starter(all = TRUE),
                                error = function(e) NULL)
          have <- if (is.null(available) || nrow(available) == 0L) {
            "none were found"
          } else {
            paste("found:", paste(unique(available$version), collapse = ", "))
          }
          stop("No Python ", python_version, " interpreter is available to ",
               "build a virtual environment from (", have, ").",
               "\n\nInstall one first, then retry:\n",
               "  reticulate::install_python(version = \"", python_version,
               "\")\n",
               "  install_r4lineups_python(envname = \"", envname,
               "\", python_version = \"", python_version, "\")",
               "\n\nAlternatively install Python ", python_version,
               " from https://www.python.org/downloads/ .",
               call. = FALSE)
        }
      }
      message("Creating virtual environment: ", envname)
      args <- list(envname = envname)
      if (!is.null(python_version)) {
        args$version <- python_version
      }
      do.call(reticulate::virtualenv_create, args)
    }
    reticulate::use_virtualenv(envname, required = TRUE)
  }

  # Fail before downloading anything if the interpreter cannot host TensorFlow.
  .check_tensorflow_python()

  # Install packages
  reticulate::py_install(
    packages = packages,
    pip = TRUE,
    pip_options = pip_options
  )

  message("\nInstallation complete!")
  message("Run check_python_deps() to verify the installation.")

  invisible(TRUE)
}


#' Check Python Dependencies
#'
#' Verifies that all required Python packages are installed and reports
#' their versions.
#'
#' @param verbose Logical. If TRUE, prints detailed information.
#'
#' @return A tibble with package names, installed status, and versions. Its
#'   \code{"face_mesh"} attribute says whether \code{get_face_landmarks()} can
#'   run: \code{"solutions"}, \code{"tasks"} or \code{"unavailable"}.
#'
#' @examples
#' \dontrun{
#' check_python_deps()
#' }
#'
#' @export
check_python_deps <- function(verbose = TRUE) {
  packages <- c(
    "deepface",
    "cv2",
    "numpy",
    "tensorflow",
    "mediapipe",
    "retinaface"
  )

  # Display names (cv2 is imported as cv2 but installed as opencv-python)
  display_names <- c(
    "deepface",
    "opencv-python (cv2)",
    "numpy",
    "tensorflow",
    "mediapipe",
    "retinaface"
  )

  results <- tibble::tibble(
    package = display_names,
    installed = logical(length(packages)),
    version = character(length(packages)),
    required = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  )

  # Do not initialize Python merely to inspect availability. With recent
  # reticulate versions initialization may offer/download a managed Python,
  # which is inappropriate during R CMD check and surprising for a check-only
  # helper.
  if (!reticulate::py_available(initialize = FALSE)) {
    results$version[] <- NA_character_
    if (verbose) {
      cat("Python is not configured. Run install_r4lineups_python() explicitly ",
          "if you want to use face-analysis features.\n", sep = "")
    }
    return(invisible(results))
  }

  for (i in seq_along(packages)) {
    pkg <- packages[i]
    results$installed[i] <- reticulate::py_module_available(pkg)

    if (results$installed[i]) {
      tryCatch({
        mod <- reticulate::import(pkg)
        results$version[i] <- if (!is.null(mod$`__version__`)) {
          as.character(mod$`__version__`)
        } else {
          "unknown"
        }
      }, error = function(e) {
        results$version[i] <- "error"
      })
    } else {
      results$version[i] <- NA_character_
    }
  }

  # Importing mediapipe is not enough: get_face_landmarks() needs either the
  # legacy solutions interface or the Tasks FaceLandmarker
  face_mesh <- "unavailable"
  if (results$installed[packages == "mediapipe"]) {
    face_mesh <- tryCatch({
      mp <- reticulate::import("mediapipe")
      if (reticulate::py_has_attr(mp, "solutions")) {
        "solutions"
      } else if (reticulate::py_has_attr(
        reticulate::import("mediapipe.tasks.python.vision"), "FaceLandmarker")) {
        "tasks"
      } else {
        "unavailable"
      }
    }, error = function(e) "unavailable")
  }
  attr(results, "face_mesh") <- face_mesh

  if (verbose) {
    cat("\n=== r4lineups Python Dependencies ===\n\n")
    cat(sprintf("Python version: %s\n", reticulate::py_config()$version))
    cat(sprintf("Python path: %s\n\n", reticulate::py_config()$python))

    for (i in seq_len(nrow(results))) {
      status <- if (results$installed[i]) {
        paste0("\u2713 ", results$version[i])
      } else if (results$required[i]) {
        "\u2717 MISSING (required)"
      } else {
        "\u2717 not installed (optional)"
      }
      cat(sprintf("%-25s %s\n", results$package[i], status))
    }

    cat(sprintf("%-25s %s\n", "face mesh landmarks",
                switch(face_mesh,
                       solutions = "\u2713 mediapipe.solutions",
                       tasks = "\u2713 MediaPipe Tasks FaceLandmarker",
                       "\u2717 unavailable (get_face_landmarks() will fail)")))

    # Summary
    missing_required <- sum(!results$installed & results$required)
    if (missing_required > 0) {
      cat("\n\u26A0 ", missing_required, " required package(s) missing.\n")
      cat("Run install_r4lineups_python() to install them.\n")
    } else {
      cat("\n\u2713 All required packages installed!\n")
    }
  }

  invisible(results)
}


#' List Available Face Recognition Models
#'
#' Returns a character vector of available face recognition models
#' that can be used for embedding extraction.
#'
#' @return Character vector of model names.
#'
#' @details
#' Available models with their embedding dimensions:
#' \itemize{
#'   \item VGG-Face (4096-d) - Legacy model
#'   \item Facenet (128-d) - Google's FaceNet
#'   \item Facenet512 (512-d) - FaceNet with larger embeddings
#'   \item OpenFace (128-d) - Open source
#'   \item DeepFace (4096-d) - Facebook's model
#'   \item DeepID (160-d) - Compact model
#'   \item ArcFace (512-d) - State-of-the-art, recommended
#'   \item Dlib (128-d) - Lightweight
#'   \item SFace (128-d) - Lightweight
#'   \item GhostFaceNet (512-d) - Efficient
#' }
#'
#' @examples
#' available_models()
#'
#' @export
available_models <- function() {
  c(
    "VGG-Face",
    "Facenet",
    "Facenet512",
    "OpenFace",
    "DeepFace",
    "DeepID",
    "ArcFace",
    "Dlib",
    "SFace",
    "GhostFaceNet"
  )
}


#' List Available Face Detectors
#'
#' Returns a character vector of available face detection backends.
#'
#' @return Character vector of detector names.
#'
#' @details
#' Available detectors (in rough order of accuracy/speed tradeoff):
#' \itemize{
#'   \item retinaface - Most accurate, recommended for research
#'   \item mtcnn - Good accuracy, moderate speed
#'   \item mediapipe - Fast, good for real-time
#'   \item opencv - Fastest, less accurate
#'   \item ssd - Fast CNN-based
#'   \item dlib - Classic HOG-based
#'   \item yolov8 - YOLO-based detection
#'   \item centerface - Anchor-free detection
#'   \item skip - Skip detection (use full image)
#' }
#'
#' @examples
#' available_detectors()
#'
#' @export
available_detectors <- function() {
  c(
    "retinaface",
    "mtcnn",
    "mediapipe",
    "opencv",
    "ssd",
    "dlib",
    "yolov8",
    "centerface",
    "skip"
  )
}


# Minimum and maximum Python versions for which TensorFlow publishes wheels.
# Update these when TensorFlow's supported range changes.
.tf_python_min <- "3.9"
.tf_python_max <- "3.12"


# Resolves the Python interpreter reticulate would use and checks it against the
# range TensorFlow supports. pip otherwise fails partway through the install
# with "No matching distribution found for tensorflow", which does not name the
# actual cause.
.check_tensorflow_python <- function(min_version = .tf_python_min,
                                     max_version = .tf_python_max) {
  cfg <- tryCatch(reticulate::py_discover_config(), error = function(e) NULL)
  if (is.null(cfg) || is.null(cfg$version)) {
    warning("Could not determine the Python version; skipping the TensorFlow ",
            "compatibility check. If installation fails with 'No matching ",
            "distribution found for tensorflow', use Python ", min_version,
            " to ", max_version, ".", call. = FALSE)
    return(invisible(NA))
  }

  version <- package_version(cfg$version)
  if (version < package_version(min_version) ||
      version > package_version(paste0(max_version, ".999"))) {
    stop("Python ", cfg$version, " was found at ", cfg$python, ", but ",
         "TensorFlow only publishes wheels for Python ", min_version, " to ",
         max_version, ". Installing now would download the other dependencies ",
         "and then fail with 'No matching distribution found for tensorflow'.",
         "\n\nObtain a supported Python, then build an environment from it:\n",
         "  reticulate::install_python(version = \"", min_version, "\")\n",
         "  install_r4lineups_python(envname = \"r4lineups-face-env\", ",
         "python_version = \"", min_version, "\")",
         call. = FALSE)
  }

  invisible(TRUE)
}
