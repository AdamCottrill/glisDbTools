##' Fetch FN125 Age data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' age data from the FN125 table in a format that matches the FN127
##' table in the upload template.  It automatically assigns
##' preferred=TRUE and sets ageid=125.
##' @title Fetch FN126 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN126 data for the specified
##'   creel
##' @export
##' @author R. Adam Cottrill
get_creesys_fn125_ages <- function(prj_cd, src_db) {
  sql <- "SELECT
         PRJ_CD,
         TRIM(STR([FN125].[SAM])) AS SAM,
         EFF,
         SPC,
         GRP,
         FISH,
         125 AS AGEID,
         AGE AS AGEA,
         'TRUE' AS PREFERRED,
         AGEMT,
         XAGEM,
         CONF,
         '' AS NCA,
         EDGE,
         '' AS COMMENT7,
          '' AS AGESTRM,
          '' AS AGELAKE,
          '' AS SPAWNCHKCNT,
          IIF(
             ISNULL(
                 [AGEA]
             ),
             1,
             NULL
         ) AS AGE_FAIL
     FROM
         FN125
     where PRJ_CD='%s' and  XAGEM is not null
     order by
         PRJ_CD,
         Trim(Str([FN125].[SAM])),
         EFF,
         Spc,
         GRP,
         FISH;
     "

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
