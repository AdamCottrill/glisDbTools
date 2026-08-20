##' Fetch FN125_lamprey data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' lamprey data from the FN125 table in a format that can be
##' processed to matches the FN125_lampreys table in the upload
##' template.
##' @title Fetch FN125 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN125 data for the specified
##'   creel
##' @export
##' @author R. Adam Cottrill
get_creesys_fn125_lamprey <- function(prj_cd, src_db) {
  # a function replace the Get_FN125_tags query from the mapper database.

  sql <- "SELECT PRJ_CD, TRIM(STR([FN125].[SAM])) AS SAM, EFF, SPC, GRP,
          FISH, 1 AS LAMID, LAMIJC, XLAM, COMMENT5 AS COMMENT_LAM
          FROM FN125
          WHERE (PRJ_CD='%1$s' AND LAMIJC IS NOT NULL) OR
          (PRJ_CD='%1$s' AND XLAM Is Not Null);"

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
