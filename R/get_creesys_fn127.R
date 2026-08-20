##' Fetch FN127 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN127 data in a format that matches the FN127 table in the upload
##' template.
##' @title Fetch FN127 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN127 data for the specified
##'   creel
##' @export
##' @author R. Adam Cottrill
get_creesys_fn127 <- function(prj_cd, src_db) {
  # a function replace the Get_FN126 query from the mapper database.

  sql <- "SELECT
     PRJ_CD,
     TRIM(STR([FN127].[SAM])) AS SAM,
     EFF,
     SPC,
     GRP,
     FISH,
     IIF(ISNULL([FN127].[AGEID]),1,[FN127].[AGEID]) AS AGEID,
     AGEA,
     'FALSE' AS PREFERRED,
     AGEMT,
     CONF,
     '' AS NCA,
     EDGE,
     F7 AS COMMENT7,
     '' AS AGESTRM,
     '' AS AGELAKE,
     '' AS SPAWNCHKCNT,
     IIF(ISNULL([AGEA]),1,NULL) AS AGE_FAIL
       FROM FN127
     WHERE PRJ_CD='%s'
     ORDER BY PRJ_CD, TRIM(STR([FN127].[SAM])), EFF, SPC, GRP, FISH,
      IIF(ISNULL([FN127].[AGEID]),1,[FN127].[AGEID]);
     "

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
