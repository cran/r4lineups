#' Compute Face Similarity Between Two Images
#'
#' Computes the similarity between two face images using deep learning embeddings.
#' This is the core function for comparing a target face to a foil face.
#'
#' @param img1_path Path to the first (target) image.
#' @param img2_path Path to the second (foil) image.
#' @param model Character. Face recognition model. Default is "ArcFace".
#' @param metric Character. Distance metric: "cosine" (recommended), 
#'   "euclidean", or "euclidean_l2".
#' @param detector Character. Face detection backend. Default is "retinaface".
#' @param return_embeddings Logical. If TRUE, also returns the embedding vectors.
#'
#' @return A list containing:
#' \itemize{
#'   \item distance - The computed distance (lower = more similar)
#'   \item similarity - Similarity score in [-1,1] for cosine metric
#'   \item verified - Logical, TRUE if distance is below the model's threshold
#'   \item threshold - The verification threshold for this model/metric
#'   \item model - Model used
#'   \item metric - Metric used
#'   \item embeddings - (if return_embeddings=TRUE) List of both embeddings
#' }
#'
#' @details
#' This function wraps DeepFace's verify function but is designed for
#' similarity measurement rather than identity verification. In lineup research,
#' we typically want to measure how similar foils are to the target, not whether
#' they are the same person.
#'
#' The 'verified' field is the model's identity decision: whether the
#' distance falls inside the model's threshold for "same person". In a lineup
#' it is a prompt to inspect the two photographs, not a finding. Face
#' recognition systems make false matches, and a match does not show that a
#' foil is nonfunctioning for witnesses.
#'
#' @examples
#' \dontrun{
#' # Compare target face to a foil
#' result <- face_similarity("target.jpg", "foil1.jpg")
#' 
#' # Check similarity score
#' result$similarity  # 0.85 would mean very similar
#' 
#' # Inspect the model's same-identity flag; this is not a fairness judgment.
#' result$verified  # TRUE would indicate problematic similarity
#' }
#'
#' @export
face_similarity <- function(img1_path,
                            img2_path,
                            model = "ArcFace",
                            metric = "cosine",
                            detector = "retinaface",
                            return_embeddings = FALSE) {
  
  # Validate inputs
  if (!file.exists(img1_path)) {
    stop("Image 1 not found: ", img1_path, call. = FALSE)
  }
  if (!file.exists(img2_path)) {
    stop("Image 2 not found: ", img2_path, call. = FALSE)
  }
  
  model <- match.arg(model, available_models())
  metric <- match.arg(metric, c("cosine", "euclidean", "euclidean_l2"))
  detector <- match.arg(detector, available_detectors())
  
  deepface <- get_deepface()
  
  tryCatch({
    result <- deepface$verify(
      img1_path = normalizePath(img1_path),
      img2_path = normalizePath(img2_path),
      model_name = model,
      distance_metric = metric,
      detector_backend = detector,
      enforce_detection = TRUE,
      align = TRUE
    )
    
    output <- list(
      distance = result$distance,
      similarity = if (metric == "cosine") 1 - result$distance else NA_real_,
      verified = result$verified,
      threshold = result$threshold,
      model = model,
      metric = metric,
      img1_path = img1_path,
      img2_path = img2_path
    )
    
    if (return_embeddings) {
      emb1 <- get_embedding(img1_path, model = model, detector = detector)
      emb2 <- get_embedding(img2_path, model = model, detector = detector)
      output$embeddings <- list(emb1 = emb1$embedding, emb2 = emb2$embedding)
    }
    
    class(output) <- c("face_similarity", "list")
    output
    
  }, error = function(e) {
    stop("Error computing similarity: ", e$message, call. = FALSE)
  })
}


#' Compute Lineup Similarity Matrix
#'
#' Computes similarity between a target face and all foils in a lineup,
#' returning a comprehensive similarity analysis useful for lineup fairness
#' assessment.
#'
#' @param target_path Path to the target (suspect) face image.
#' @param foil_paths Character vector of paths to foil face images.
#' @param model Character. Face recognition model. Default is "ArcFace".
#' @param metric Character. Distance metric. Default is "cosine".
#' @param detector Character. Face detection backend. Default is "retinaface".
#' @param foil_names Optional character vector of names/labels for foils.
#'   If NULL, uses file names.
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item foil_id - Numeric ID (1, 2, 3, ...)
#'   \item foil_name - Name/label for the foil
#'   \item foil_path - Path to foil image
#'   \item distance - Distance from target (lower = more similar)
#'   \item similarity - Cosine similarity score [-1,1] for cosine metric
#'   \item verified - Would be identified as same person
#'   \item rank - Rank by similarity (1 = most similar to target)
#' }
#'
#' Also includes attributes:
#' \itemize{
#'   \item target_path - Path to target image
#'   \item model - Model used
#'   \item metric - Metric used
#'   \item threshold - Verification threshold
#' }
#'
#' @details
#' This function describes how similar each foil is to the target in a face
#' recognition model's embedding space. It does not assess fairness: no
#' validated similarity range defines a fair lineup, and embedding similarity
#' is not the same as resemblance judged by people or as mock-witness choice.
#' Foils very dissimilar to the target may make the target stand out; foils
#' the model judges to be the same identity (verified = TRUE) deserve
#' inspection. Rows are ordered by rank (most similar first), and the model's
#' distance threshold is stored in \code{attr(x, "threshold")}. For cosine,
#' similarity = 1 - distance, which ranges from -1 to 1.
#'
#' @examples
#' \dontrun{
#' # Analyze a 6-person lineup
#' foils <- paste0("foil", 1:5, ".jpg")
#' lineup <- lineup_similarity("target.jpg", foils)
#' 
#' # View results sorted by similarity
#' print(lineup)
#' 
#' # Check which foils might be too similar
#' lineup[lineup$verified, ]
#' 
#' # Get summary statistics
#' summary(lineup)
#' }
#'
#' @export
#' @importFrom dplyr mutate arrange
lineup_similarity <- function(target_path,
                              foil_paths,
                              model = "ArcFace",
                              metric = "cosine",
                              detector = "retinaface",
                              foil_names = NULL) {
  
  # Validate target
  if (!file.exists(target_path)) {
    stop("Target image not found: ", target_path, call. = FALSE)
  }
  
  # Validate foils
  missing <- foil_paths[!file.exists(foil_paths)]
  if (length(missing) > 0) {
    stop("Foil image(s) not found: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  
  # Generate foil names if not provided
  if (is.null(foil_names)) {
    foil_names <- tools::file_path_sans_ext(basename(foil_paths))
  }
  
  if (length(foil_names) != length(foil_paths)) {
    stop("Length of foil_names must match length of foil_paths", call. = FALSE)
  }
  
  # Get target embedding once (more efficient)
  message("Extracting target embedding...")
  target_emb <- get_embedding(
    target_path, 
    model = model, 
    detector = detector
  )
  
  # Get foil embeddings
  message("Extracting foil embeddings...")
  foil_embs <- batch_embeddings(
    foil_paths,
    model = model,
    detector = detector,
    progress = TRUE
  )
  
  # Check for failures
  failed <- foil_embs[!foil_embs$success, ]
  if (nrow(failed) > 0) {
    warning("Failed to extract embeddings for: ", 
            paste(failed$img_path, collapse = ", "))
  }
  
  # Compute distances
  message("Computing similarities...")
  
  # Get threshold for this model/metric combination
  deepface <- get_deepface()
  
  # Compute threshold by doing a dummy verification
  # (This is a workaround to get the default threshold)
  threshold <- tryCatch({
    dummy <- deepface$verify(
      img1_path = normalizePath(target_path),
      img2_path = normalizePath(foil_paths[1]),
      model_name = model,
      distance_metric = metric,
      detector_backend = detector
    )
    dummy$threshold
  }, error = function(e) {
    # Fallback thresholds
    switch(paste(model, metric, sep = "_"),
           "ArcFace_cosine" = 0.68,
           "Facenet512_cosine" = 0.30,
           "Facenet_cosine" = 0.40,
           0.5  # generic fallback
    )
  })
  
  # Compute similarities for each foil
  results <- purrr::map2_dfr(
    seq_along(foil_paths),
    foil_embs$embedding,
    function(i, foil_emb) {
      if (is.null(foil_emb)) {
        return(tibble::tibble(
          foil_id = i,
          foil_name = foil_names[i],
          foil_path = foil_paths[i],
          distance = NA_real_,
          similarity = NA_real_,
          verified = NA
        ))
      }
      
      dist <- embedding_distance(target_emb$embedding, foil_emb, metric = metric)
      sim <- if (metric == "cosine") 1 - dist else NA_real_
      
      tibble::tibble(
        foil_id = i,
        foil_name = foil_names[i],
        foil_path = foil_paths[i],
        distance = dist,
        similarity = sim,
        verified = dist < threshold
      )
    }
  )
  
  # Add rank
  results <- results |>
    dplyr::mutate(rank = rank(distance, ties.method = "min", na.last = "keep")) |>
    dplyr::arrange(rank)
  
  # Add attributes
  attr(results, "target_path") <- target_path
  attr(results, "target_embedding") <- target_emb$embedding
  attr(results, "model") <- model

attr(results, "metric") <- metric
  attr(results, "threshold") <- threshold
  attr(results, "n_foils") <- length(foil_paths)
  
  class(results) <- c("lineup_similarity", class(results))
  
  results
}


#' Print Method for lineup_similarity
#' @param x A lineup_similarity object
#' @param ... Additional arguments (ignored)
#' @export
print.lineup_similarity <- function(x, ...) {
  cat("\n=== Lineup Similarity Analysis ===\n\n")
  cat("Target:", attr(x, "target_path"), "\n")
  cat("Model:", attr(x, "model"), "\n")
  cat("Metric:", attr(x, "metric"), "\n")
  cat("Threshold:", round(attr(x, "threshold"), 3), "\n")
  cat("Number of foils:", attr(x, "n_foils"), "\n\n")
  
  # Print the data
  print(tibble::as_tibble(x), n = Inf)
  
  # Summary stats
  cat("\n--- Summary ---\n")
  cat("Mean distance:", round(mean(x$distance, na.rm = TRUE), 3), "\n")
  cat("SD distance:", round(sd(x$distance, na.rm = TRUE), 3), "\n")
  cat("Range:", round(min(x$distance, na.rm = TRUE), 3), "-", 
      round(max(x$distance, na.rm = TRUE), 3), "\n")
  cat("Foils verified as 'same person':", sum(x$verified, na.rm = TRUE), "\n")
  
  invisible(x)
}


#' Summary Method for lineup_similarity
#' @param object A lineup_similarity object
#' @param ... Additional arguments (ignored)
#' @export
summary.lineup_similarity <- function(object, ...) {
  list(
    n_foils = attr(object, "n_foils"),
    model = attr(object, "model"),
    metric = attr(object, "metric"),
    threshold = attr(object, "threshold"),
    mean_distance = mean(object$distance, na.rm = TRUE),
    sd_distance = sd(object$distance, na.rm = TRUE),
    min_distance = min(object$distance, na.rm = TRUE),
    max_distance = max(object$distance, na.rm = TRUE),
    mean_similarity = mean(object$similarity, na.rm = TRUE),
    n_verified = sum(object$verified, na.rm = TRUE),
    most_similar_foil = object$foil_name[which.min(object$distance)],
    least_similar_foil = object$foil_name[which.max(object$distance)]
  )
}


#' Compute Pairwise Similarity Matrix for All Lineup Members
#'
#' Computes a full pairwise similarity matrix between all faces in a lineup,
#' including the target. Useful for understanding the overall structure
#' of face similarities in the lineup.
#'
#' @param target_path Path to target face.
#' @param foil_paths Paths to foil faces.
#' @param model Face recognition model.
#' @param metric Distance metric.
#' @param detector Face detector.
#'
#' @return A square matrix of pairwise distances, with row/column names.
#'
#' @examples
#' \dontrun{
#' mat <- lineup_pairwise_matrix("target.jpg", c("foil1.jpg", "foil2.jpg"))
#' # Visualize with heatmap
#' heatmap(mat)
#' }
#'
#' @export
lineup_pairwise_matrix <- function(target_path,
                                   foil_paths,
                                   model = "ArcFace",
                                   metric = "cosine",
                                   detector = "retinaface") {
  
  all_paths <- c(target_path, foil_paths)
  n <- length(all_paths)
  if (length(foil_paths) < 1L) {
    stop("foil_paths must contain at least one foil image.", call. = FALSE)
  }
  missing <- all_paths[!file.exists(all_paths)]
  if (length(missing) > 0L) {
    stop("Image file(s) not found: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  
  # Get names
  names <- c("Target", paste0("Foil_", seq_along(foil_paths)))
  
  # Get all embeddings
  message("Extracting embeddings...")
  embs <- batch_embeddings(all_paths, model = model, detector = detector, progress = TRUE)
  
  # Compute pairwise distances
  mat <- matrix(0, nrow = n, ncol = n, dimnames = list(names, names))
  
  message("Computing pairwise distances...")
  for (i in seq_len(n - 1L)) {
    for (j in seq.int(i + 1L, n)) {
      if (!is.null(embs$embedding[[i]]) && !is.null(embs$embedding[[j]])) {
        d <- embedding_distance(embs$embedding[[i]], embs$embedding[[j]], metric = metric)
        mat[i, j] <- d
        mat[j, i] <- d
      } else {
        mat[i, j] <- NA
        mat[j, i] <- NA
      }
    }
  }
  
  attr(mat, "model") <- model
  attr(mat, "metric") <- metric
  
  mat
}
