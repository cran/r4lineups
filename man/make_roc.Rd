% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_roc.R
\name{make_roc}
\alias{make_roc}
\title{Compute and plot ROC curve for lineup accuracy ~ confidence}
\usage{
make_roc(df_confacc)
}
\arguments{
\item{df_confacc}{A dataframe with two columns, named confidence and accuracy (where accuracy = binary accuracy)}
}
\value{
An ROC object of package pROC
}
\description{
Function to compute and plot an ROC curve for data from an eyewitness
experiment, where accuracy is recorded for target present and target
absent lineups
}
\details{
The approach is outlined in several papers by Mickes, Wixted, Gronlund,
Clark, and others (see \strong{References})

This function is a user level function.  It chains the two roc functions
together. The user must pass a dataframe, with one column indicating
confidence, and another accuracy, and these must be named as such.

The approach is outlined in several papers by Mickes, Wixted, Gronlund,
Clark, and others (see references)
}
\examples{
#Data:
data(mickwick)

#Call:
make_roc(mickwick)



}
\references{
Gronlund, S. D., Wixted, J. T., & Mickes, L. (2014). Evaluating
eyewitness identification procedures using receiver operating characteristic
analysis. \emph{Current Directions in Psychological Science, 23}(1), 3-10.
}
