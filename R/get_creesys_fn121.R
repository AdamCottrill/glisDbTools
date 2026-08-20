##' Fetch FN121 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN121 data in a format that matches the FN121 table in the upload
##' template.
##' @title Fetch FN121 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN121 data for the specified
##'   creel
##' @export
##' @author R. Adam Cottrill
get_creesys_fn121 <- function(prj_cd, src_db) {
  # a function replace the Get_FN121 query from the mapper database.

  sql <- "
        select distinct
            FN121.PRJ_CD,
            FN121.SAM,
            FN121.SAMA,
            ITVSEQ,
            IIF(
                ISNULL(
                    [FN121].[STRATUM]
                ),
                [FN111].[STRATUM],
                [FN121].[STRATUM]
            ) AS STRATUM,
            DATE,
            DOW,
            ITVTM0,
            SPACE AS SUBSPACE,
            MODE,
            SAMTM0,
            EFFDT0,
            EFFTM0,
            EFFDT1,
            EFFTM1,
            EFFCMP,
            EFFDURC,
            EFFDUR,
            PERSONS,
            ANGLERS,
            RODS,
            COMMENT1,
            GRID AS GRID5,
            DD_LAT,
            DD_LON,
            ANGMETH,
            ANGGUID,
            ANGORIG,
            ANGVIS,
            ANGOP1,
            ANGOP2,
            ANGOP3,
            ANGOP4,
            ANGOP5
        from
            FN121
        inner join (
        SELECT PRJ_CD, SAMA, STRATUM
                FROM FN111
        ) as get_fn111 on
                FN121.SAMA = get_fn111.SAMA and
                FN121.PRJ_CD = get_fn111.PRJ_CD
        where FN121.PRJ_CD='%s'
        order by
            FN121.SAM,
            FN121.SAMA;"

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
