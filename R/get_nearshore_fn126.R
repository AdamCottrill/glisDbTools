##' Fetch FN126 data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' FN126 data in a format that matches the FN126 table in the upload
##' template.  The query used in this function includes a union
##' statement to extract some diet data that is incorrectly stored in
##' the FN125 data for a small number of projects.
##'
##'
##' @title Fetch FN126 data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN126 data for the specified
##'   assessment project
##' @export
##' @author R. Adam Cottrill
get_nearshore_fn126 <- function(prj_cds, src_db) {
  # a function replace the Get_FN126 query from the mapper database.

  sql <- "SELECT IA125.PRJ_CD,
          Trim(Str([IA125].[SAM])) AS SAM,
          IA125.EFF,
           IA125.SPC,
           IA125.GRP,
           IA125.FISH,
           1 AS FOOD,
           IA125.TAXON,
           IA125.FDCNT,
           '' AS FDMES,
           '' AS FDVAL,
           '' AS LIFESTAGE,
           '' AS COMMENT6
          FROM IA125
          WHERE IA125.PRJ_CD in (%1$s)
          AND IA125.TAXON Is Not Null
          UNION ALL
          SELECT IA126.PRJ_CD,
           Trim(Str([IA126].[SAM])) AS SAM,
           IA126.EFF,
           IA126.SPC,
           '00' AS GRP,
           IA126.FISH,
           IA126.FOOD,
           IA126.TAXON,
           IA126.FDCNT,
           '' AS FDMES,
           '' AS FDVAL,
           '' AS LIFESTAGE,
           '' AS COMMENT6
          FROM IA126
          WHERE IA126.PRJ_CD in (%1$s)
          "

  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(src_db, stmt)

  dat <- dat[with(dat, order(PRJ_CD, SAM, EFF, SPC, GRP, FISH, FOOD)), ]
  return(dat)
}
