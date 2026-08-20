##' Set waterhaul to True if there are no FN123 records.
##'
##' This function set FN122.WATERHAUL values to TRUE or 0 based on
##' values in the FN123 table.  If there are not fn123 records
##' associated with an Fn122 record, waterhaul is set to TRUE.
##' @title Set waterhaul
##' @param dbase - path the populated tempalte database.
##' @return odbc connection status ('0' (success) or '1')
##' @author R. Adam Cottrill
update_FN122_waterhaul <- function(dbase) {
  sql <- "UPDATE FN122 LEFT JOIN FN123
ON (FN122.EFF = FN123.EFF)
AND(FN122.SAM = FN123.SAM)
AND(FN122.PRJ_CD = FN123.PRJ_CD)
SET FN122.WATERHAUL = 'TRUE'
WHERE (((FN123.PRJ_CD) Is Null));"
  check_accdb(dbase)
  conn <- RODBC::odbcConnectAccess2007(
    dbase,
    uid = "",
    pwd = "",
    case = "nochange"
  )
  RODBC::sqlQuery(conn, sql)
  return(RODBC::odbcClose(conn))
}
