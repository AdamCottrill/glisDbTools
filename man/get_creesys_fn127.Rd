% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/creesys_utils.R
\name{get_creesys_fn127}
\alias{get_creesys_fn127}
\title{Fetch FN127 data from Creesys Database}
\usage{
get_creesys_fn127(prj_cd, src_db)
}
\arguments{
\item{prj_cd}{\itemize{
\item the project code of creel to export
}}

\item{src_db}{\itemize{
\item path to the accdb with the src data
}}
}
\value{
\itemize{
\item dataframe containing the FN127 data for the specified
creel
}
}
\description{
Fetch FN127 data from Creesys Database
}
\details{
This function will connect to the source database and extract the
FN127 data in a format that matches the FN127 table in the upload
template.
}
\author{
R. Adam Cottrill
}
