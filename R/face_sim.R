#'Compute similarity of faces in a lineup; experimental function
#'
#'Function to compute the degree to which each face in a set of faces
#'loads onto a common factor computed from the faces.
#'
#'@details \itemize{
#'         \item The faces need to be standardized for inter-pupil distance, and for
#'                pupil location prior to running the function
#'
#'         \item The user will be asked to choose a set of faces through a dialog
#'               box. These should be jpeg files.
#'
#'        \item There is no argument to the function; it gets what it needs from the
#'              dialog box.  The function prints the lineup array to the viewer
#'              pane in R, and reports the loading of each face on the first common
#'              factor in a factor analysis (using fa in package psych)
#'}
#'
#'@references Tredoux, C. (2002). A direct measure of facial similarity
#'            and its relation to human similarity perceptions.
#'            \emph{Journal of Experimental Psychology: Applied, 8}(3), 180-193.
#'            doi:10.1037/1076-898x.8.3.180
#'
#'
#'@export
#'@import magick here
#'@importFrom psych fa


face_sim <- function(){
  root <- paste(here(),"/Faces/*.jpg",sep = "")
  file_list <- utils::choose.files(default = "root", caption = "Select files",
                            multi = TRUE, filters = utils::Filters,
                            index = nrow(utils::Filters))
  a_face_df <- make_lineup_data(file_list)
  fa_faces <- psych::fa(a_face_df, nfactors = 1)
  z <- fa_faces$loadings[1:length(fa_faces$loadings)]
  cat("\nLineup is shown in the Viewer window,\n")
  cat("Reading from left to right for order,\n")
  cat("values indicate extent to which each face\n")
  cat("matches the other faces in the lineups\n")
  cat("where higher values are a greater match\n\n")
  print(show_lineup(file_list))
  print (z, digits = 3)
}
