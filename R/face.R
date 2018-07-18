#' Helper function
#' @param file_list A selection of image files (raw data)
#' @export
#' @import magick here
# This function reads and shows an array of faces
show_lineup <- function(file_list){

  # Read faces into img object 'img'
  img <- image_read(file_list)

  # Display the faces as an array
  (image_append(image_scale(img, "x150")))

}

# Helper function
# Read lineup faces, and make/return
# a dataframe of RGB values for each face, as a vector in the frame
# Returns a dataframe for further analysis

make_lineup_data <- function(file_list){

  fnamesa <- as.character(1:length(file_list))
  fnamesb <- rep("Face",length(file_list))
  fnamesc <- paste(fnamesb, fnamesa, sep="_")

  face_df <- NULL

  for (i in 1:length(file_list)){
    temp_face <- image_read(file_list[i])
    temp_face <- image_scale(temp_face, "100")
    temp_face <- image_convert(temp_face,"tiff")
    temp_face_array <- as.integer(temp_face[[1]])
    dim(temp_face_array) <- NULL
    face_df <- cbind(face_df, temp_face_array)
  }
  face_df <- as.data.frame(face_df)
  names(face_df) <- fnamesc
  return (face_df)
}





