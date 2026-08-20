##' Fetch FN022 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN022 data in a format that matches the FN022 table in the upload
##' template.
##' @title Fetch FN022 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN022 data for the specified
##'   creel
##' @export
##' @author R. Adam Cottrill
get_creesys_fn022 <- function(prj_cd, src_db) {
  # a function replace the Get_FN022 query from the mapper database.

  sql <- "SELECT PRJ_CD, SSN, SSN_DATE0, SSN_DATE1, SSN_DES
    FROM FN022
    GROUP BY PRJ_CD, SSN, SSN_DATE0, SSN_DATE1, SSN_DES
    HAVING PRJ_CD='%s'
    ORDER BY PRJ_CD, SSN;
    "
  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
