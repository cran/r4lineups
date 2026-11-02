#' Extract Face Embedding from an Image
#'
#' Extracts a deep learning embedding vector from a face image using
#' a specified model. The embedding can be used to compute face similarity.
#'
#' @param img_path Path to the image file containing a face.
#' @param model Character. The face recognition model to use.
#'   Default is "ArcFace" (recommended). See \code{\link{available_models}}.
#' @param detector Character. Face detection backend.
#'   Default is "retinaface" (most accurate). See \code{\link{available_detectors}}.
#' @param align Logical. Whether to align the face before embedding.
#'   Default is TRUE (recommended).
#' @param enforce_detection Logical. If TRUE, raises an error if no face
#'   is detected. If FALSE, attempts to use the entire image. Default is TRUE.
#' @param expand_percentage Numeric. Percentage to expand the detected face
#'   region. Default is 0.
#'
#' @return A named list containing:
#' \itemize{
#'   \item embedding - Numeric vector of face embeddings
#'   \item facial_area - List with x, y, w, h of detected face region
#'   \item face_confidence - Detection confidence score
#' }
#'
#' @details
#' The embedding dimension depends on the model:
#' \itemize{
#'   \item ArcFace: 512 dimensions
#'   \item Facenet512: 512 dimensions
#'   \item Facenet: 128 dimensions
#'   \item VGG-Face: 4096 dimensions
#' }
#'
#' One example configuration used in the package is:
#' \itemize{
#'   \item model = "ArcFace"
#'   \item detector = "retinaface"
#'   \item align = TRUE
#' }
#' These defaults do not establish superior accuracy for every population,
#' image source or research question, or validate lineup fairness.
#'
#' @examples
#' \dontrun{
#' # Get embedding for a single face
#' emb <- get_embedding("path/to/face.jpg")
#' 
#' # Access the embedding vector
#' emb$embedding
#' 
#' # Use a different model
#' emb_facenet <- get_embedding("path/to/face.jpg", model = "Facenet512")
#' }
#'
#' @export
get_embedding <- function(img_path,
                          model = "ArcFace",
                          detector = "retinaface",
                          align = TRUE,
                          enforce_detection = TRUE,
                          expand_percentage = 0) {
  
  # Validate inputs
  if (!file.exists(img_path)) {
    stop("Image file not found: ", img_path, call. = FALSE)
  }
  
  model <- match.arg(model, available_models())
  detector <- match.arg(detector, available_detectors())
  
  # Get deepface module
  deepface <- get_deepface()
  
  # Extract embedding
  tryCatch({
    result <- deepface$represent(
      img_path = normalizePath(img_path),
      model_name = model,
      detector_backend = detector,
      enforce_detection = enforce_detection,
      align = align,
      expand_percentage = as.integer(expand_percentage)
    )
    
    # DeepFace returns a list of faces; take the first one
    if (length(result) == 0) {
      stop("No face detected in image: ", img_path, call. = FALSE)
    }
    
    face_data <- result[[1]]
    
    list(
      embedding = unlist(face_data$embedding),
      facial_area = face_data$facial_area,
      face_confidence = face_data$face_confidence,
      model = model,
      img_path = img_path
    )
    
  }, error = function(e) {
    stop("Error extracting embedding from ", img_path, ": ", e$message, call. = FALSE)
  })
}


#' Extract Embeddings for Multiple Images
#'
#' Efficiently extracts face embeddings from multiple images.
#'
#' @param img_paths Character vector of paths to image files.
#' @param model Character. The face recognition model to use. Default is "ArcFace".
#' @param detector Character. Face detection backend. Default is "retinaface".
#' @param align Logical. Whether to align faces. Default is TRUE.
#' @param enforce_detection Logical. Error if face not detected. Default is TRUE.
#' @param progress Logical. Show progress bar. Default is TRUE.
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item img_path - Path to the image
#'   \item embedding - List column containing embedding vectors
#'   \item facial_area - List column with face bounding box
#'   \item face_confidence - Detection confidence
#'   \item success - Logical indicating if extraction succeeded
#'   \item error - Error message if extraction failed
#' }
#'
#' @examples
#' \dontrun{
#' # Get embeddings for all images in a folder
#' img_files <- list.files("faces/", pattern = "\\.jpg$", full.names = TRUE)
#' embeddings <- batch_embeddings(img_files)
#' 
#' # Access individual embeddings
#' embeddings$embedding[[1]]
#' }
#'
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom utils txtProgressBar setTxtProgressBar
batch_embeddings <- function(img_paths,
                             model = "ArcFace",
                             detector = "retinaface",
                             align = TRUE,
                             enforce_detection = TRUE,
                             progress = TRUE) {
  
  n <- length(img_paths)
  
  results <- vector("list", n)
  
  if (progress) {
    pb <- txtProgressBar(min = 0, max = n, style = 3)
  }
  
  for (i in seq_len(n)) {
    tryCatch({
      emb <- get_embedding(
        img_paths[i],
        model = model,
        detector = detector,
        align = align,
        enforce_detection = enforce_detection
      )
      
      results[[i]] <- tibble::tibble(
        img_path = img_paths[i],
        embedding = list(emb$embedding),
        facial_area = list(emb$facial_area),
        face_confidence = emb$face_confidence,
        success = TRUE,
        error = NA_character_
      )
      
    }, error = function(e) {
      results[[i]] <<- tibble::tibble(
        img_path = img_paths[i],
        embedding = list(NULL),
        facial_area = list(NULL),
        face_confidence = NA_real_,
        success = FALSE,
        error = e$message
      )
    })
    
    if (progress) {
      setTxtProgressBar(pb, i)
    }
  }
  
  if (progress) {
    close(pb)
  }
  
  dplyr::bind_rows(results)
}


#' Compute Distance Between Two Embeddings
#'
#' Calculates the distance or similarity between two face embedding vectors.
#'
#' @param emb1 Numeric vector. First embedding.
#' @param emb2 Numeric vector. Second embedding.
#' @param metric Character. Distance metric to use:
#'   \itemize{
#'     \item "cosine" - Cosine distance (1 - cosine similarity). Recommended for ArcFace.
#'     \item "euclidean" - Euclidean (L2) distance.
#'     \item "euclidean_l2" - Euclidean distance on L2-normalized vectors.
#'   }
#'
#' @return Numeric distance value. Lower values indicate more similar faces.
#'
#' @details
#' For ArcFace and other angular-margin trained models, cosine distance
#' is recommended as it aligns with the training objective.
#'
#' Cosine distance ranges from 0 (identical) to 2 (opposite).
#' Typical thresholds for same-person verification:
#' \itemize{
#'   \item ArcFace + cosine: ~0.68
#'   \item Facenet512 + cosine: ~0.30
#' }
#'
#' @examples
#' \dontrun{
#' emb1 <- get_embedding("face1.jpg")$embedding
#' emb2 <- get_embedding("face2.jpg")$embedding
#'
#' # Cosine distance (recommended)
#' embedding_distance(emb1, emb2, metric = "cosine")
#'
#' # Euclidean distance
#' embedding_distance(emb1, emb2, metric = "euclidean")
#' }
#'
#' @export
embedding_distance <- function(emb1, emb2, metric = c("cosine", "euclidean", "euclidean_l2")) {
  
  metric <- match.arg(metric)
  
  if (length(emb1) != length(emb2)) {
    stop("Embeddings must have the same length. Got ", length(emb1), " and ", length(emb2),
         call. = FALSE)
  }
  if (!is.numeric(emb1) || !is.numeric(emb2) || length(emb1) == 0L ||
      anyNA(emb1) || anyNA(emb2) || any(!is.finite(emb1)) || any(!is.finite(emb2))) {
    stop("Embeddings must be non-empty finite numeric vectors.", call. = FALSE)
  }
  norm1 <- sqrt(sum(emb1^2))
  norm2 <- sqrt(sum(emb2^2))
  if (metric %in% c("cosine", "euclidean_l2") && (norm1 == 0 || norm2 == 0)) {
    stop("Cosine and L2-normalized distances are undefined for zero vectors.",
         call. = FALSE)
  }
  
  switch(metric,
    "cosine" = {
      # Cosine distance = 1 - cosine_similarity
      dot_product <- sum(emb1 * emb2)
      cosine_sim <- dot_product / (norm1 * norm2)
      1 - cosine_sim
    },
    "euclidean" = {
      sqrt(sum((emb1 - emb2)^2))
    },
    "euclidean_l2" = {
      # L2 normalize first
      emb1_norm <- emb1 / norm1
      emb2_norm <- emb2 / norm2
      sqrt(sum((emb1_norm - emb2_norm)^2))
    }
  )
}


#' Convert Cosine Distance to Similarity Score
#'
#' Converts cosine distance to cosine similarity in [-1, 1].
#'
#' @param distance Numeric. Cosine distance value(s).
#'
#' @return Numeric cosine similarity score(s), where 1 means the same direction,
#'   0 means orthogonal, and -1 means opposite directions.
#'
#' @examples
#' # Distance of 0 -> similarity of 1
#' cosine_to_similarity(0)  # Returns 1
#'
#' # Distance of 0.5 -> similarity of 0.5
#' cosine_to_similarity(0.5)  # Returns 0.5
#'
#' @export
cosine_to_similarity <- function(distance) {
  1 - distance
}
