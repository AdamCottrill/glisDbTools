##' Fetch FN028 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN028 data in a format that matches the FN028 table in the upload
##' template.
##' @title Fetch FN028 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN028 data for the specified
##'   creel
##' @export
##' @author R. Adam Cottrill
get_creesys_fn028 <- function(prj_cd, src_db) {
  # a function replace the Get_FN028 query from the mapper database.

  sql <- "SELECT PRJ_CD, MODE, MODE_DES, ATYUNIT, ITVUNIT, CHKFLAG, COMMENT8
        FROM FN028
        WHERE PRJ_CD='%s';"

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
