##' Fetch FN011 data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' FN011 data in a format that matches the FN011 table in the upload
##' template.
##' @title Fetch FN011 data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN011 data for the specified
##'   assessment project
##' @export
##' @author R. Adam Cottrill
get_nearshore_fn011 <- function(prj_cds, src_db) {
  # a function replace the Get_FN011 query from the mapper database.

  sql <- "SELECT YEAR, PRJ_CD, PRJ_NM, PRJ_LDR, PRJ_DATE0, PRJ_DATE1,
          COMMENT0, PROTOCOL
          FROM IA011
          WHERE PRJ_CD in (%s)
          ORDER BY Year, PRJ_CD;"

  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )
  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
