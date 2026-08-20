##' Fetch FN125_lamprey data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' lamprey data from the FN125 table in a format that can be
##' processed to matches the FN125_lampreys table in the upload
##' template.
##' @title Fetch FN125 data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN125 data for the specified
##'   assessment project
##' @export
##' @author R. Adam Cottrill
get_nearshore_fn125_lamprey <- function(prj_cds, src_db) {
  # a function replace the Get_FN125_tags query from the mapper database.

  sql <- "SELECT PRJ_CD, Trim(Str([IA125].[SAM])) AS SAM, EFF, SPC, GRP,
          FISH, 1 AS LAMID, LAMIJC, XLAM, COMMENT5 AS COMMENT_LAM
          FROM IA125
          WHERE PRJ_CD in (%1$s) AND LAMIJC Is Not Null OR
          PRJ_CD in (%1$s) AND XLAM Is Not Null;"

  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
