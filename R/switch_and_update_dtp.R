##' Standarize daytype encoding
##'
##' This function will switch the encoding used to indicate week-days
##' and weekends in a creel.  These were left to the discression of
##' the project lead, and as a result are not consisten across all
##' creels.  This is a wrapper function that will switch the day-type
##' values in the FN023, FN024 and F025 tables, and update the strata
##' fields in the FN111 and F121 tables.
##' @title Standarize daytype encoding
##' @param trg_db  - path to the accdb with the template database that is
##'   being populated.
##' @return NULL
##' @export
##' @author R. Adam Cottrill
switch_and_update_dtp <- function(trg_db) {
  check_accdb(trg_db)
  con <- RODBC::odbcConnectAccess2007(
    trg_db,
    uid = "",
    pwd = "",
    case = "nochange"
  )
  switch_dtp(con, "FN023")
  switch_dtp(con, "FN024")
  switch_dtp(con, "FN025", "DTP1")
  update_stratum_dtp(con, "FN111")
  update_stratum_dtp(con, "FN121")
  RODBC::odbcClose(con)
}


##' Switch DTP value in target table
##'
##' this function will execute an update statement that will switch
##' the values of DTP in the specified table in the target table.  The
##' field argument is used to generalize the function to work with
##' FN025 table too.
##' @title Switch DTP value in target table
##' @param con - an open ODBC connection to the target database.
##' @param table the table to run the update query against.
##' @param field - the field to change (DTP for the FN023 and FN024
##'   tables. DTP1 for the FN025 table)
##' @return NULL
##' @author R. Adam Cottrill
switch_dtp <- function(con, table, field = "DTP") {
  stmt <- "update [%s] set [%s]= iif([%s]='1', '2', '1')"
  sql <- sprintf(stmt, table, field, field)
  print(sprintf("updating %s", table))
  RODBC::sqlQuery(con, sql)
}


##' Update the DTP value in the Stratum of the target table
##'
##' This function will execute an update statement that will switch
##' the values of DTP in stratum field of the specified table in the targe database.
##' @title Update the DTP value in the Stratum field
##' @param con - an open ODBC connection to the target database.
##' @param table the table to run the update query against.
##' @return NULL
##' @author R. Adam Cottrill
update_stratum_dtp <- function(con, table) {
  stmt <- "update [%s] set stratum =
IIf(Mid([stratum],4,1)=1,
Left([stratum],3) & '2' & Right([stratum],7),
Left([stratum],3) & '1' & Right([stratum],7))
"
  sql <- sprintf(stmt, table)
  print(sprintf("updating %s", table))
  RODBC::sqlQuery(con, sql)
}
