##' Fetch FN123 data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' FN123 data in a format that matches the FN123 table in the upload
##' template.
##' @title Fetch FN123 data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN123 data for the specified
##'   assessment project
##' @export
##' @author R. Adam Cottrill
get_nearshore_fn123 <- function(prj_cds, src_db) {
  # a function replace the Get_FN123 query from the mapper database.

  sql <- "SELECT PRJ_CD, Trim(Str([ia123].[SAM])) AS SAM, EFF, SPC, GRP, CATCNT,
           BIOCNT, SUBCNT, SUBWT, COMMENT3, '' AS CATWT
           FROM IA123
           WHERE PRJ_CD in (%s)
           ORDER BY PRJ_CD, Trim(Str([ia123].[SAM])), EFF, SPC, GRP;
           "
  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
