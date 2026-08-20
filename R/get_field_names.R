##' List field names in target table
##'
##' This function connects to a target table in the provided database
##' and returns a dataframe containing all of the field names in the
##' target table of the provided database.
##' @title List fields  in table
##' @param trg_db - the absolute or relative path to the target
##'   database (accdb file)
##' @param table - the name of the table to query.
##' @return dataframe
##' @export
##' @author R. Adam Cottrill
get_field_names <- function(trg_db, table) {
  check_accdb(trg_db)
  conn <- RODBC::odbcConnectAccess2007(
    trg_db,
    uid = "",
    pwd = "",
    case = "nochange"
  )
  stmt <- sprintf("select * from [%s] where FALSE;", table)
  dat <- RODBC::sqlQuery(
    conn,
    stmt,
    as.is = TRUE,
    stringsAsFactors = FALSE,
    na.strings = ""
  )
  RODBC::odbcClose(conn)
  return(names(dat))
}
