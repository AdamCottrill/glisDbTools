##' Fetch FN022 data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' FN022 data in a format that matches the FN022 table in the upload
##' template.
##' @title Fetch FN022 data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN022 data for the specified
##'   assessment project
##' @export
##' @author R. Adam Cottrill
get_nearshore_fn022 <- function(prj_cds, src_db) {
  # a function replace the Get_FN022 query from the mapper database.

  sql <- "SELECT PRJ_CD, '00' AS SSN, 'COMING SOON' AS SSN_DES,
           MIN(EFFDT0) AS SSN_DATE0, MAX(EFFDT1) AS SSN_DATE1
           FROM IA121 GROUP BY PRJ_CD, '00', 'COMING SOON'
          HAVING PRJ_CD in (%s);"

  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )
  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
