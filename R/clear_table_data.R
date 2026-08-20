##' Delete the all of data from target table
##'
##' This function exectutes a query on the target table that will
##' delete all of the data in the provided table of the target
##' database. No warning or confirmation is currently implemented.
##' @title Delete Table Data
##' @param db path the the target accdb file
##' @param table_name The name of the table in the target database to
##'   clear
##' @param prj_cds a character vector of project codes to remove from
##'   the target table (used to 'unmerge' databases)
##' @return NULL
##' @export
##' @author R. Adam Cottrill
clear_table_data <- function(db, table_name, prj_cds = NULL) {
  if (is.null(prj_cds)) {
    sql <- sprintf("Delete * from [%s];", table_name)
  } else {
    project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")
    sql <- sprintf(
      "Delete * from [%s] where [PRJ_CD] in (%s);",
      table_name,
      project_codes
    )
  }

  payload <- fetch_sql(db, sql, payload = FALSE)
  return(payload)
}
