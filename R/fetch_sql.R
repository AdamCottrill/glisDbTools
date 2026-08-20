##' Execute a select sql statement and return the results
##'
##' This funciton is a wrapper around the RODBC funct sqlQuery with
##' default arguments that will prevent R from converting strings to
##' factors and/or dropping leading zeros.
##' @title Fetch data from src database.
##' @param sql - the string to be exectuted.
##' @param src_db - a string representing the path to the src
##'   database.
##' @param payload - Boolean.  Is this sql statement expected to return data?
##' @return A dataframe containing the data returned by the sql
##'   statement.
##' @export
##' @author R. Adam Cottrill
fetch_sql <- function(src_db, sql, payload = TRUE) {
  check_accdb(src_db)
  if (payload) {
    conn <- RODBC::odbcConnectAccess2007(
      src_db,
      uid = "",
      pwd = "",
      case = "nochange"
    )

    dat <- try(RODBC::sqlQuery(
      conn,
      sql,
      as.is = TRUE,
      stringsAsFactors = FALSE,
      na.strings = ""
    ))
    RODBC::odbcClose(conn)
  } else {
    conn <- RODBC::odbcConnectAccess2007(
      src_db,
      uid = "",
      pwd = "",
      case = "nochange"
    )
    dat <- try(RODBC::sqlQuery(conn, sql))
    RODBC::odbcClose(conn)
  }
  return(dat)
}
