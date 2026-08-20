##' Fetch FN023 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN023 data in a format that matches the FN023 table in the upload
##' template.
##' @title Fetch FN023 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN023 data for the specified
##'   creel
##' @export
##' @author R. Adam Cottrill
get_creesys_fn023 <- function(prj_cd, src_db) {
  # a function replace the Get_FN023 query from the mapper database.

  sql <- "SELECT PRJ_CD, SSN, DTP, DTP_NM, DOW_LST
        FROM FN023
        WHERE PRJ_CD='%s'
        ORDER BY PRJ_CD, SSN, DTP;"

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
