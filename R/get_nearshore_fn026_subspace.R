##' Fetch FN026_subspace data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' FN026_subspace data in a format that matches the FN026_subspace
##' table in the upload template.
##' @title Fetch FN026_subspace data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN026_subspace data for the
##'   specified assessment project
##' @export
##' @author R. Adam Cottrill
get_nearshore_fn026_subspace <- function(prj_cds, src_db) {
  # a function replace the Get_FN026_subspace query from the mapper database.

  sql <- "SELECT PRJ_CD,
          '00' AS [SPACE],
          '11' as [SUBSPACE],
          'Subspace is ...' AS SUBSPACE_DES,
          Avg(IA121.DD_LON) AS DD_LON,
          Avg(IA121.DD_LAT) AS DD_LAT,
          Int(Min([SIDEP])) AS SIDEP_GE,
          Int(Max([SIDEP])) + 1 AS SIDEP_LT,
          Int(Min([GRDEP])) AS GRDEP_GE,
          Int(Max([SIDEP])) + 1 AS GRDEP_LT,
          1 as SUBSPACE_WT
          FROM IA121
          GROUP BY PRJ_CD
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
