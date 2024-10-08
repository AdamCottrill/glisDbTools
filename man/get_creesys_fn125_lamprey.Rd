% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/creesys_utils.R
\name{get_creesys_fn125_lamprey}
\alias{get_creesys_fn125_lamprey}
\title{Fetch FN125 data from Creesys Database}
\usage{
get_creesys_fn125_lamprey(prj_cd, src_db)
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
\item dataframe containing the FN125 data for the specified
creel
}
}
\description{
Fetch FN125_lamprey data from Creesys Database
}
\details{
This function will connect to the source database and extract the
lamprey data from the FN125 table in a format that can be
processed to matches the FN125_lampreys table in the upload
template.
}
\author{
R. Adam Cottrill
}
