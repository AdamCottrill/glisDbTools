##' Fetch FN121 data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' FN121 data in a format that matches the FN121 table in the upload
##' template.
##' @title Fetch FN121 data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN121 data for the specified
##'   assessment project
##' @export
##' @author R. Adam Cottrill
get_nearshore_fn121 <- function(prj_cds, src_db) {
  # a function replace the Get_FN121 query from the mapper database.

  sql <- "SELECT
            PRJ_CD,
            Trim(Str([IA121].[SAM])) AS SAM,
            EFFDT0,
            EFFTM0,
            EFFDT1,
            EFFTM1,
            EFFDUR,
            EFFST,
            GR,
            IIf(IsNull([IA121].[GRUSE]),'9',[IA121].[GRUSE]) AS GRUSE,
            IIf(IsNull([IA121].[ORIENT]),'9',[IA121].[ORIENT]) AS ORIENT,
            SIDEP as SIDEP0,
            GRID AS GRID5,
            DD_LAT AS DD_LAT0,
            DD_LON AS DD_LON0,
            IIf([IA121].[DD_LAT1]=0,Null,[IA121].[DD_LAT1]) AS DD_LAT1,
            IIf([IA121].[DD_LON1]=0,Null,[IA121].[DD_LON1]) AS DD_LON1,
            Secchi as SECCHI0,
            COMMENT1,
            GRDEPMIN,
            XGRDEPMID as GRDEPMID,
            GRDEPMAX,
            XANGLE AS LEAD_ANGLE,
            XLEADUSE AS LEADUSE,
            XDISTOFF AS DISTOFF,
            SITEM as SITEM0,
            AIRTEM0,
            AIRTEM1,
            WIND AS WIND0,
            PRECIP AS  PRECIP0,
            CLOUD_PC AS CLOUD_PC0,
            XWAVEHT AS  WAVEHT0,
            XWEATHER,
            SITP,
            CREW,
            xslime as SLIME,
            '' as SECCHI1,
            '' as WIND1,
            '' as SITEM1,
            '' as SIDEP1,
            '' as PRECIP1,
            '' as CLOUD_PC1,
            '' as WAVEHT1,
            '' as VESSEL,
            '' as VESSEL_DIRECTION,
            '' as VESSEL_SPEED,
            '' as WARP,
            '' as BOTTOM,
            '' as COVER,
            '' as VEGETATION,
            '' as O2BOT0,
            '' as O2BOT1,
            '' as O2SURF0,
            '' as O2SURF1,
            '' as O2GR0,
            '' as O2GR1,
            IIf([GR] In ('GL51','GL38','GL64','NA12','ON22'),'3',
                IIf([GRTP]='GL' Or Left([GR],2)='GL','2','1')) AS PROCESS_TYPE
            FROM IA121
          WHERE PRJ_CD in (%s)
          ORDER BY
            PRJ_CD,
            Trim(Str([IA121].[SAM]));"

  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
