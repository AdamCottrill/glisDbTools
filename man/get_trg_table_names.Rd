% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accdb_utils.R
\name{get_trg_table_names}
\alias{get_trg_table_names}
\title{List fields  in table}
\usage{
get_trg_table_names(trg_db, table)
}
\arguments{
\item{trg_db}{\itemize{
\item the absolute or relative path to the target
database (accdb file)
}}

\item{table}{\itemize{
\item the name of the table to query.
}}
}
\value{
dataframe
}
\description{
List field names in target table
}
\details{
This function connects to a target table in the provided database
and returns a dataframe containing all of the field names in the
target table of the provided database.
}
\author{
R. Adam Cottrill
}
