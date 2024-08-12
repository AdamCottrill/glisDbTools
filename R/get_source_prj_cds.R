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
##' @return - a dataframe containing all of the PRJ_CD values in the
##'   provided table.
##' @author R. Adam Cottrill
get_src_prj_cds <- function(src_db, src_table="FN011"){

  if (!file.exists(src_db)) {
    message <-
      sprintf(
        paste0(
          "Could not find the database '%s'. ",
          "Make sure it exists and try again"
        ),
        src_db
      )
    stop(message)
  }

  if (!grepl("\\.accdb$",src_db)) {
    message <-
      sprintf(
        paste0(
          "The provided database '%s' ",
          "does not appear to be an MS access (*.accdb) file."
        ),
        src_db
      )
    stop(message)
  }

  stmt <- sprintf("select distinct [PRJ_CD] from [%s] order by [PRJ_CD];", src_table)

  DBConnection <- RODBC::odbcConnectAccess2007(src_db, uid = "", pwd = "")
  dat <- RODBC::sqlQuery(DBConnection, stmt)
  RODBC::odbcClose(DBConnection)
  return(dat)
}
