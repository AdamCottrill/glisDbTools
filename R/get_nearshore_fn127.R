##' Fetch FN127 data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' FN127 data in a format that matches the FN127 table in the upload
##' template.
##' @title Fetch FN127 data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN127 data for the specified
##'   assessment project
##' @export
##' @author R. Adam Cottrill
get_nearshore_fn127 <- function(prj_cds, src_db) {
  # a function replace the Get_FN126 query from the mapper database.

  sql <- "SELECT
     PRJ_CD,
     TRIM(STR([IA127].[SAM])) AS SAM,
     EFF,
     SPC,
     GRP,
     FISH,
     IIF(ISNULL([IA127].[AGEID]),1,[IA127].[AGEID]) AS AGEID,
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
       FROM IA127
     WHERE PRJ_CD in (%s)
     ORDER BY PRJ_CD, Trim(Str([IA127].[SAM])), EFF, SPC, GRP, FISH,
      IIf(IsNull([IA127].[ageid]),1,[IA127].[ageid]);
     "

  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(src_db, stmt)

  return(dat)
}
