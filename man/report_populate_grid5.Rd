% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accdb_utils.R
\name{report_populate_grid5}
\alias{report_populate_grid5}
\title{Report on populate_grid5 progress}
\usage{
report_populate_grid5(sam_points, overwrite)
}
\arguments{
\item{sam_points}{\itemize{
\item data frame containing the columns: PRJ_CD,
SAM, DD_LAT0, DD_LON0, GRID5, and grid5_slug.
}}

\item{overwrite}{\itemize{
\item boolean - are existing grid5 values to be
replaced?
}}
}
\description{
Print summary report of populate_grid5 function
}
\details{
a helper function that is used to print a summary of the
populate_grid5 progress includeing the number of samples, unique
points, grids that could be match, and any issues.  This function is
not intended to be used anywere outside of populate_grid5

Nothing is returned from this function, but messages are reported
to the console depending on the contents of sample_points and the
value of overwrite.
}
\author{
R. Adam Cottrill
}
