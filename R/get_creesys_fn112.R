##' Fetch FN112 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN112 data in a format that matches the FN112 table in the upload
##' template.
##' @title Fetch FN112 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN112 data for the specified
##'   creel
##' @export
##' @author R. Adam Cottrill
get_creesys_fn112 <- function(prj_cd, src_db) {
  # a function replace the Get_FN112 query from the mapper database.

  sql <- "SELECT PRJ_CD, SAMA, ATYTM0, ATYTM1, ATYCNT, ITVCNT, CHKCNT, COMMENT2
        FROM FN112
        WHERE PRJ_CD='%s'
        ORDER BY PRJ_CD, SAMA, ATYTM0;"

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
