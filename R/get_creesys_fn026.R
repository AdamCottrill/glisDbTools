##' Fetch FN026 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN026 data in a format that matches the FN026 table in the upload
##' template.
##' @title Fetch FN026 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN026 data for the specified
##'   creel
##' @export
##' @author R. Adam Cottrill
get_creesys_fn026 <- function(prj_cd, src_db) {
  # a function replace the Get_FN026 query from the mapper database.

  sql <- "SELECT PRJ_CD, SPACE, SPACE_DES, AREA_WT AS SPACE_WT, SPACE_SIZ, DD_LAT, DD_LON
        FROM FN026
        WHERE PRJ_CD='%s'
        ORDER BY PRJ_CD, Space;"

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
