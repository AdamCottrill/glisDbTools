##' Fetch FN123 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN123 data in a format that matches the FN123 table in the upload
##' template.
##' @title Fetch FN123 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN123 data for the specified
##'   creel
##' @export
##' @author R. Adam Cottrill
get_creesys_fn123 <- function(prj_cd, src_db) {
  # a function replace the Get_FN123 query from the mapper database.

  sql <- "SELECT PRJ_CD, SAM, EFF, SPC, GRP, SEK, HVSCNT, RLSCNT,
        BIOCNT AS  MESCNT, MESWT, COMMENT3
        FROM FN123
        WHERE PRJ_CD='%s'
        ORDER BY PRJ_CD, SAM, EFF, Spc, GRP, SEK;
        "

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
