##' Fetch FN126 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN126 data in a format that matches the FN126 table in the upload
##' template.
##' @title Fetch FN126 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN126 data for the specified
##'   creel
##' @export
##' @author R. Adam Cottrill
get_creesys_fn126 <- function(prj_cd, src_db) {
  # a function replace the Get_FN126 query from the mapper database.

  sql <- "SELECT PRJ_CD, TRIM(STR([FN126].[SAM])) AS SAM, EFF, SPC,
        '00' AS GRP, FISH, FOOD, TAXON, FDCNT, '' AS FDMES,
        '' AS FDVAL, '' AS LF, '' AS COMMENT6
        FROM FN126
        WHERE PRJ_CD='%s'
        ORDER BY PRJ_CD, Trim(Str([FN126].[SAM])), EFF, Spc, '00', FISH, FOOD;"

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
