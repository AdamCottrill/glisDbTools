##' Fetch FN025 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN025 data in a format that matches the FN025 table in the upload
##' template.
##' @title Fetch FN025 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN025 data for the specified
##'   creel
##' @export
##' @author R. Adam Cottrill
get_creesys_fn025 <- function(prj_cd, src_db) {
  # a function replace the Get_FN025 query from the mapper database.

  sql <- "SELECT FN025.PRJ_CD, SSN, FN025.DATE, DTP1, 'Holiday' AS DESCRIPTION
        FROM FN022 INNER JOIN FN025 ON FN022.PRJ_CD = FN025.PRJ_CD
        WHERE FN025.PRJ_CD='%s' AND
        FN025.Date Between [SSN_DATE0] And [SSN_DATE1]
        ORDER BY FN025.PRJ_CD, FN025.Date;
        "

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
