##' Fetch FN011 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN011 data in a format that matches the FN011 table in the upload
##' template.
##' @title Fetch FN011 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN011 data for the specified
##'   creel
##' @export
##' @author R. Adam Cottrill
get_creesys_fn011 <- function(prj_cd, src_db) {
  # a function replace the Get_FN011 query from the mapper database.

  sql <- "SELECT YEAR, PRJ_CD, CONTMETH, PRJ_DATE0, PRJ_DATE1,
        PRJ_LDR, PRJ_NM, COMMENT0
        FROM fn011
        WHERE PRJ_CD='%s';"

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
