##' Fetch FN111 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN111 data in a format that matches the FN111 table in the upload
##' template.
##' @title Fetch FN111 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN111 data for the specified
##'   creel
##' @export
##' @author R. Adam Cottrill
get_creesys_fn111 <- function(prj_cd, src_db) {
  # a function replace the Get_FN111 query from the mapper database.

  sql <- "SELECT PRJ_CD, SAMA, STRATUM, MODE, DATE, SAMTM0, COMMENT1, DOW,
        SPACE AS SUBSPACE, WEATHER AS WEATHER_EFFECT, ATYDATA, CREW,
        AIRTEM0 AS AIRTEM, SITEM0 AS SITEM, WIND, CLOUD_PC, PRECIP
        FROM FN111
        WHERE PRJ_CD='%s'
        ORDER BY PRJ_CD, SAMA, STRATUM;"

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
