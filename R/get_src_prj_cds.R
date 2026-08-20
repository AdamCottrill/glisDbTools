##' Get distinct project codes in the source database
##'
##' Returns the unique project codes in the source database.  This
##' list can be used to compare against project codes in Project
##' Tracker, creel Portal, or the assessment portal to find projects
##' that are missing in one or the other.
##' @title Get unique Project Codes from Source DB.
##' @param src_db - complete path to an accdb file.
##' @param src_table - option table name to query PRJ_CD
##'   from. Defaults to 'FN011', but any table name from the source
##'   data can be used.
##' @return a dataframe containing all of the PRJ_CD values in the
##'   provided table.
##' @export
##' @author R. Adam Cottrill
get_src_prj_cds <- function(src_db, src_table = "FN011") {
  check_accdb(src_db)

  stmt <- sprintf(
    "select distinct [PRJ_CD] from [%s] order by [PRJ_CD];",
    src_table
  )

  conn <- RODBC::odbcConnectAccess2007(
    src_db,
    uid = "",
    pwd = "",
    case = "nochange"
  )
  dat <- RODBC::sqlQuery(conn, stmt)
  RODBC::odbcClose(conn)
  return(dat)
}
