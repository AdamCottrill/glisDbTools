##' Add a meta data record to the target database
##'
##' This function inserts the provided message into the README table
##' in the target database. If a README table does not exists, it will
##' be created.
##' @title Populate README with provided message
##' @param trg_db - the path to the populated template database.
##' @param message - the contents of the record to inserted into the
##'   Readme table of target database.
##' @return NULL
##' @export
##' @author R. Adam Cottrill
update_readme <- function(trg_db, message) {
  README <- data.frame("README" = message)
  conn <- RODBC::odbcConnectAccess2007(
    trg_db,
    uid = "",
    pwd = "",
    case = "nochange"
  )
  RODBC::sqlSave(conn, README, rownames = FALSE, append = TRUE)
  RODBC::odbcClose(conn)
}
