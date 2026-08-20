##' Fetch FN026 data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' FN026 data in a format that matches the FN026 table in the upload
##' template.
##' @title Fetch FN026 data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN026 data for the specified
##'   assessment project
##' @export
##' @author R. Adam Cottrill
get_nearshore_fn026 <- function(prj_cds, src_db) {
  # a function replace the Get_FN026 query from the mapper database.

  sql <- "SELECT PRJ_CD,
          '00' AS [SPACE],
          'Space is ...' AS SPACE_DES,
          Avg(IA121.DD_LON) AS DD_LON,
          Avg(IA121.DD_LAT) AS DD_LAT,
          Int(Min([SIDEP])) AS SIDEP_GE,
          Int(Max([SIDEP])) + 1 AS SIDEP_LT,
          Int(Min([GRDEP])) AS GRDEP_GE,
          Int(Max([SIDEP])) + 1 AS GRDEP_LT,
          '' as SPACE_WT
          FROM IA121
          GROUP BY PRJ_CD, '00', 'Space is ...'
          HAVING PRJ_CD in (%s);
          "
  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
