% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datacheck3.R
\name{datacheck3}
\alias{datacheck3}
\title{Helper function}
\usage{
datacheck3(lineup_table, k)
}
\arguments{
\item{lineup_table}{A table of lineup choices}

\item{k}{Nominal size (i.e., total number of members in lineup)}
}
\description{
Checks that number of lineup choices contained in a data table is accurate
}
\details{
This function ensures that a non-selected lineup member is not accidentally
        omitted from the data due to lack of selection by all mock witnesses.
        It functions as a check that the total number of lineup members is accurate.
}
