#' Plot Lineup Similarity Results
#'
#' Creates a bar plot showing the similarity of each foil to the target face,
#' describing scores in a recognition model's embedding space, not measuring
#' or certifying lineup fairness.
#'
#' @param lineup A lineup_similarity object from \code{\link{lineup_similarity}}.
#' @param show_threshold Logical. If TRUE, adds a horizontal line at the
#'   verification threshold. Default is TRUE.
#' @param use_similarity Logical. If TRUE, plots similarity scores (higher = more similar).
#'   If FALSE, plots distance (lower = more similar). Default is TRUE.
#' @param title Optional plot title.
#' @param colors Color palette for bars. Default uses a gradient from
#'   blue (dissimilar) to red (similar).
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' lineup <- lineup_similarity("target.jpg", foil_paths)
#' plot_lineup_similarity(lineup)
#' 
#' # Show distance instead of similarity
#' plot_lineup_similarity(lineup, use_similarity = FALSE)
#' }
#'
#' @export
plot_lineup_similarity <- function(lineup,
                                   show_threshold = TRUE,
                                   use_similarity = TRUE,
                                   title = NULL,
                                   colors = NULL) {
  
  if (!inherits(lineup, "lineup_similarity")) {
    stop("Input must be a lineup_similarity object", call. = FALSE)
  }
  
  # Prepare data
  plot_data <- lineup
  
  if (use_similarity) {
    if (all(is.na(plot_data$similarity))) {
      warning("Similarity scores not available (metric was not cosine). Using distance.")
      use_similarity <- FALSE
    }
  }
  
  if (use_similarity) {
    y_var <- "similarity"
    y_label <- "Similarity to Target"
    threshold_val <- 1 - attr(lineup, "threshold")
    # Order by similarity (descending)
    plot_data <- plot_data[order(-plot_data$similarity), ]
    plot_data$foil_name <- factor(plot_data$foil_name, levels = plot_data$foil_name)
  } else {
    y_var <- "distance"
    y_label <- "Distance from Target"
    threshold_val <- attr(lineup, "threshold")
    # Order by distance (ascending)
    plot_data <- plot_data[order(plot_data$distance), ]
    plot_data$foil_name <- factor(plot_data$foil_name, levels = plot_data$foil_name)
  }
  
  if (is.null(title)) {
    title <- paste0("Lineup Similarity Analysis (", attr(lineup, "model"), ")")
  }
  
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(
    x = foil_name, 
    y = .data[[y_var]],
    fill = .data[[y_var]]
  )) +
    ggplot2::geom_bar(stat = "identity", width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = title,
      subtitle = paste0("Metric: ", attr(lineup, "metric")),
      x = "Foil",
      y = y_label,
      fill = y_label
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      plot.title = ggplot2::element_text(face = "bold")
    )
  
  # Add color gradient
  if (use_similarity) {
    p <- p + ggplot2::scale_fill_gradient(
      low = "#3498db",   # blue = less similar
      high = "#e74c3c",  # red = more similar
      limits = c(-1, 1)
    )
  } else {
    p <- p + ggplot2::scale_fill_gradient(
      low = "#e74c3c",   # red = low distance (similar)
      high = "#3498db",  # blue = high distance (dissimilar)
      limits = c(0, max(plot_data$distance, na.rm = TRUE) * 1.1)
    )
  }
  
  # Add threshold line
  if (show_threshold && !is.na(threshold_val)) {
    p <- p + ggplot2::geom_hline(
      yintercept = threshold_val,
      linetype = "dashed",
      color = "darkred",
      linewidth = 0.8
    ) +
      ggplot2::annotate(
        "text",
        x = 0.5,
        y = threshold_val,
        label = "Verification Threshold",
        hjust = -0.1,
        vjust = -0.5,
        color = "darkred",
        size = 3
      )
  }
  
  p
}


#' Plot Embedding Space
#'
#' Creates a 2D visualization of face embeddings using dimensionality reduction,
#' showing how faces cluster in the embedding space.
#'
#' @param embeddings_df A tibble from \code{\link{batch_embeddings}} with
#'   an 'embedding' list column.
#' @param method Dimensionality reduction method: "umap" (default), "pca", or "tsne".
#' @param labels Optional character vector of labels for each point.
#' @param highlight Optional indices of points to highlight (e.g., target position).
#' @param n_neighbors For UMAP, number of neighbors. Default is 15.
#' @param perplexity For t-SNE, perplexity parameter. Default is 30.
#'
#' @return A ggplot2 object.
#'
#' @details
#' This visualization helps understand:
#' \itemize{
#'   \item How faces cluster by identity
#'   \item Which foils are closest to the target in embedding space
#'   \item Overall structure of similarity in a lineup
#' }
#'
#' @examples
#' \dontrun{
#' # Get embeddings for all lineup members
#' all_faces <- c("target.jpg", paste0("foil", 1:5, ".jpg"))
#' embs <- batch_embeddings(all_faces)
#' 
#' # Plot with target highlighted
#' plot_embedding_space(embs, labels = c("Target", paste0("F", 1:5)), highlight = 1)
#' }
#'
#' @export
#' @importFrom stats prcomp
plot_embedding_space <- function(embeddings_df,
                                 method = c("umap", "pca", "tsne"),
                                 labels = NULL,
                                 highlight = NULL,
                                 n_neighbors = 15,
                                 perplexity = 30) {
  
  method <- match.arg(method)
  
  # Extract embeddings matrix
  valid_embs <- !sapply(embeddings_df$embedding, is.null)
  
  if (sum(valid_embs) < 3) {
    stop("Need at least 3 valid embeddings for visualization", call. = FALSE)
  }
  
  emb_matrix <- do.call(rbind, embeddings_df$embedding[valid_embs])
  
  # Generate labels if not provided
  if (is.null(labels)) {
    labels <- paste0("Face_", seq_len(nrow(emb_matrix)))
  } else {
    labels <- labels[valid_embs]
  }
  
  # Dimensionality reduction
  coords <- switch(method,
    "pca" = {
      pca_result <- prcomp(emb_matrix, center = TRUE, scale. = FALSE)
      pca_result$x[, 1:2]
    },
    "umap" = {
      if (!requireNamespace("umap", quietly = TRUE)) {
        stop("Package 'umap' required for UMAP visualization. Install with: install.packages('umap')")
      }
      umap_result <- umap::umap(emb_matrix, n_neighbors = min(n_neighbors, nrow(emb_matrix) - 1))
      umap_result$layout
    },
    "tsne" = {
      if (!requireNamespace("Rtsne", quietly = TRUE)) {
        stop("Package 'Rtsne' required for t-SNE visualization. Install with: install.packages('Rtsne')")
      }
      tsne_result <- Rtsne::Rtsne(emb_matrix, 
                                   perplexity = min(perplexity, (nrow(emb_matrix) - 1) / 3),
                                   check_duplicates = FALSE)
      tsne_result$Y
    }
  )
  
  colnames(coords) <- c("Dim1", "Dim2")
  
  plot_data <- data.frame(
    Dim1 = coords[, 1],
    Dim2 = coords[, 2],
    label = labels,
    highlighted = if (!is.null(highlight)) {
      seq_len(nrow(coords)) %in% highlight
    } else {
      FALSE
    }
  )
  
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Dim1, y = Dim2)) +
    ggplot2::geom_point(
      ggplot2::aes(color = highlighted, size = highlighted),
      alpha = 0.8
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      hjust = -0.2, vjust = 0.5,
      size = 3
    ) +
    ggplot2::scale_color_manual(
      values = c("FALSE" = "#3498db", "TRUE" = "#e74c3c"),
      guide = "none"
    ) +
    ggplot2::scale_size_manual(
      values = c("FALSE" = 3, "TRUE" = 5),
      guide = "none"
    ) +
    ggplot2::labs(
      title = paste0("Face Embedding Space (", toupper(method), ")"),
      x = paste0(toupper(method), " Dimension 1"),
      y = paste0(toupper(method), " Dimension 2")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::coord_equal()
  
  p
}


#' Display Lineup as Image Grid
#'
#' Creates a visual display of the lineup faces arranged in a grid.
#'
#' @param target_path Path to target image.
#' @param foil_paths Paths to foil images.
#' @param ncol Number of columns in the grid.
#' @param scale Scale factor for images.
#' @param show_labels Logical. Add labels under each face.
#' @param highlight_target Logical. Add border around target.
#'
#' @return A magick image object that can be displayed or saved.
#'
#' @examples
#' \dontrun{
#' # Display lineup
#' img <- display_lineup("target.jpg", paste0("foil", 1:5, ".jpg"))
#' print(img)
#' 
#' # Save to file
#' magick::image_write(img, "lineup_display.png")
#' }
#'
#' @export
display_lineup <- function(target_path,
                           foil_paths,
                           ncol = 3,
                           scale = 150,
                           show_labels = TRUE,
                           highlight_target = TRUE) {
  
  all_paths <- c(target_path, foil_paths)
  labels <- c("Target", paste0("Foil ", seq_along(foil_paths)))
  
  # Read and scale images
  imgs <- lapply(all_paths, function(p) {
    img <- magick::image_read(p)
    magick::image_scale(img, paste0(scale, "x"))
  })
  
  # Add labels if requested
  if (show_labels) {
    imgs <- mapply(function(img, label) {
      magick::image_annotate(
        img, label,
        gravity = "south",
        size = 12,
        color = "white",
        boxcolor = "black"
      )
    }, imgs, labels, SIMPLIFY = FALSE)
  }
  
  # Add border to target if requested
  if (highlight_target) {
    imgs[[1]] <- magick::image_border(imgs[[1]], "red", "3x3")
  }
  
  # Arrange in grid
  n_imgs <- length(imgs)
  nrow_grid <- ceiling(n_imgs / ncol)
  
  # Pad with blank images if needed
  while (length(imgs) < nrow_grid * ncol) {
    blank <- magick::image_blank(scale, scale, "white")
    imgs <- c(imgs, list(blank))
  }
  
  # Create rows
  rows <- lapply(seq_len(nrow_grid), function(r) {
    start_idx <- (r - 1) * ncol + 1
    end_idx <- min(r * ncol, length(imgs))
    row_imgs <- imgs[start_idx:end_idx]
    magick::image_append(do.call(c, row_imgs))
  })
  
  # Stack rows
  magick::image_append(do.call(c, rows), stack = TRUE)
}


#' Plot Similarity Distribution
#'
#' Creates a histogram or density plot showing the distribution of
#' face similarities, useful for understanding the range of similarities
#' in a lineup or database.
#'
#' @param similarities Numeric vector of similarity scores or distances.
#' @param type Plot type: "histogram" or "density".
#' @param threshold Optional threshold value to show as vertical line.
#' @param xlab X-axis label.
#' @param title Plot title.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' set.seed(123)
#' similarities <- rbeta(100, 2, 5)
#' plot_similarity_distribution(similarities, threshold = 0.6)
#'
#' @export
plot_similarity_distribution <- function(similarities,
                                         type = c("histogram", "density"),
                                         threshold = NULL,
                                         xlab = "Similarity",
                                         title = "Distribution of Face Similarities") {
  
  type <- match.arg(type)
  
  plot_data <- data.frame(similarity = similarities)
  
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = similarity))
  
  if (type == "histogram") {
    p <- p + ggplot2::geom_histogram(
      bins = 30,
      fill = "#3498db",
      color = "white",
      alpha = 0.7
    )
  } else {
    p <- p + ggplot2::geom_density(
      fill = "#3498db",
      alpha = 0.5
    )
  }
  
  if (!is.null(threshold)) {
    p <- p + ggplot2::geom_vline(
      xintercept = threshold,
      linetype = "dashed",
      color = "darkred",
      linewidth = 1
    )
  }
  
  p <- p +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = if (type == "histogram") "Count" else "Density"
    ) +
    ggplot2::theme_minimal()
  
  p
}
