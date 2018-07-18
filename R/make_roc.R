#'Compute and plot ROC curve for lineup accuracy ~ confidence
#'
#'Function to compute and plot an ROC curve for data from an eyewitness
#'experiment, where accuracy is recorded for target present and target
#'absent lineups
#'
#'The approach is outlined in several papers by Mickes, Wixted, Gronlund,
#'Clark, and others (see \strong{References})
#'
#'@param df_confacc A dataframe with two columns, named confidence and accuracy (where accuracy = binary accuracy)
#'@return An ROC object of package pROC
#'@examples
#'#Data:
#'data(mickwick)
#'
#'#Call:
#'make_roc(mickwick)
#'
#'
#'
#'@references Gronlund, S. D., Wixted, J. T., & Mickes, L. (2014). Evaluating
#'eyewitness identification procedures using receiver operating characteristic
#'analysis. \emph{Current Directions in Psychological Science, 23}(1), 3-10.
#'
#'@details This function is a user level function.  It chains the two roc functions
#'together. The user must pass a dataframe, with one column indicating
#'confidence, and another accuracy, and these must be named as such.
#'
#'The approach is outlined in several papers by Mickes, Wixted, Gronlund,
#'Clark, and others (see references)
#'
#'@export
#'@import pROC ggplot2 ggrepel

make_roc <- function(df_confacc){
  make_rocdata(df_confacc) %>%
    make_roc_gg() -> rocplot
  return(rocplot)
}
