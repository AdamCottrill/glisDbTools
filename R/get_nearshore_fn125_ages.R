##' Fetch FN125 Age data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' age data from the FN125 table in a format that matches the FN127
##' table in the upload template.  It automatically assigns
##' preferred=TRUE and sets ageid=125.
##' @title Fetch FN126 data from Nearshore Database
##' @param  prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN126 data for the specified
##'   assessment project
##' @export
##' @author R. Adam Cottrill
get_nearshore_fn125_ages <- function(prj_cds, src_db) {
  sql <- "select
         PRJ_CD,
         Trim(Str([IA125].[SAM])) as SAM,
         EFF,
         SPC,
         GRP,
         FISH,
         125 AS AGEID,
         AGE as AGEA,
         'TRUE' AS PREFERRED,
         AGEMT,
         XAGEM,
         CONF,
         NCA,
         EDGE,
         '' as COMMENT7,
          '' as AGESTRM,
          '' as AGELAKE,
          '' as SPAWNCHKCNT,
          IIf(
             isnull(
                 [AGEA]
             ),
             1,
             NULL
         ) as AGE_FAIL
     from
         IA125
     where PRJ_CD in (%s) and  XAGEM is not null
     order by
         PRJ_CD,
         Trim(Str([IA125].[SAM])),
         EFF,
         SPC,
         GRP,
         FISH;
     "

  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(src_db, stmt)

  return(dat)
}
