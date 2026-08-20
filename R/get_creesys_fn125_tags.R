##' Fetch FN125_tag data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' tag data from the FN125 table in a format that matches the
##' FN125_tags table in the upload template.
##' @title Fetch FN125 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN125 data for the specified
##'   creel
##' @export
##' @author R. Adam Cottrill
get_creesys_fn125_tags <- function(prj_cd, src_db) {
  # a function replace the Get_FN125_tags query from the mapper database.

  sql <- "SELECT PRJ_CD, SAM, EFF, SPC, GRP, FISH, 1 AS FISH_TAG_ID,
          TAGID, TAGDOC, TAGSTAT, XCWTSEQ AS CWTSEQ, '' AS COMMENT_TAG
          FROM FN125
          WHERE PRJ_CD='%s' AND TAGID Is Not Null And TAGID<>'0';"

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
