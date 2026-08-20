##' Return a dataframe with table names in target accdb
##'
##' This function will fetch a list of table names in the target
##' database and return them in a data frame.
##' @title List accdb table names
##' @param trg_db - path to the target accdb file
##' @return dataframe containsing table names in the target database.
##' @export
##' @author R. Adam Cottrill
get_tablenames <- function(trg_db) {
  check_accdb(trg_db)
  conn <- RODBC::odbcConnectAccess2007(
    trg_db,
    uid = "",
    pwd = "",
    case = "nochange"
  )
  tables <- RODBC::sqlTables(conn)
  RODBC::odbcClose(conn)
  tables <- tables$TABLE_NAME[tables$TABLE_TYPE == "TABLE"]
  return(tables)
}
