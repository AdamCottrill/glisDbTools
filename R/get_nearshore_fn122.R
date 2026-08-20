##' Fetch FN122 data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' FN122 data in a format that matches the FN122 table in the upload
##' template.
##' @title Fetch FN122 data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN122 data for the specified
##'   assessment project
##' @export
##' @author R. Adam Cottrill
get_nearshore_fn122 <- function(prj_cds, src_db) {
  # a function replace the Get_FN122 query from the mapper database.

  sql <- " SELECT PRJ_CD, Trim(Str([ia122].[SAM])) AS SAM, EFF, EFFDST,
           GRDEP as GRDEP0,
           '' as GRDEP1,
           GRTEM0, GRTEM1, 'FALSE' AS WATERHAUL, '' AS COMMENT2
           FROM IA122
           WHERE PRJ_CD in (%s)
           ORDER BY PRJ_CD, Trim(Str([ia122].[SAM])), EFF;
          "
  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
