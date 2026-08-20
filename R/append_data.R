# Append data to the specified table in the target database
##'
##' This is a helper function that compares the names of the provided
##' dataframe with the field names in the target database and stops if
##' the names don't match.  If the names don't match a report is
##' provided explaining where the differences were found.
##'
##' Optional arguments can be used to skip the field name check, force
##' insertion, or print the verbose output from RODBC::sqlSave()
##' function.
##'
##' @title Append data in a dataframe to target table
##' @param dbase - full path to an ms access file.
##' @param trg_table - the name of table in the target data base to
##'   append to.
##' @param data - the data frame to append to the target table. The
##'   names in this dataframe must match the column names in trg_table
##'   if check_names=TRUE.
##' @param append - passed to sqlSave()- should the data be appened to
##'   (TRUE) or overwrite (FALSE) an existing table
##' @param safer - passed to sqlSave() - only appends are allowed if
##'   safer=TRUE
##' @param check_names - boolean - should the names of the target
##'   table be compared to the the names of the provided dataframe
##'   before attempting to insert the rows in the database?
##' @param verbose - passed to sqlSave() - should the sqlSave()
##'   function produce verbose output? Very useful in debugging.
##' @return status of the odbc connection.
##' @export
##' @author R. Adam Cottrill
append_data <- function(
    dbase,
    trg_table,
    data,
    append = T,
    safer = T,
    check_names = T,
    verbose = F) {
  check_accdb(dbase)

  if (check_names) {
    field_check <- check_table_names(dbase, trg_table, data)
    if (length(field_check)) {
      stop("Please fix field differences before proceeding.")
    }
  }

  conn <- RODBC::odbcConnectAccess2007(
    dbase,
    uid = "",
    pwd = "",
    case = "nochange"
  )
  RODBC::sqlSave(
    conn,
    data,
    tablename = trg_table,
    rownames = F,
    safer = safer,
    append = append,
    nastring = "",
    verbose = verbose
  )
  return(RODBC::odbcClose(conn))
}
