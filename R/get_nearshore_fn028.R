##' Fetch FN028 data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' FN028 data in a format that matches the FN028 table in the upload
##' template.
##' @title Fetch FN028 data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN028 data for the specified
##'   assessment project
##' @export
##' @author R. Adam Cottrill
get_nearshore_fn028 <- function(prj_cds, src_db) {
  # a function replace the Get_FN028 query from the mapper database.

  sql <- "SELECT PRJ_CD,
          '01' AS MODE,
          GR,
          IIf(IsNull([IA121].[GRUSE]),'9',[IA121].[GRUSE]) AS GRUSE,
          IIf(IsNull([IA121].[ORIENT]),'9',[IA121].[ORIENT]) AS ORIENT,
          'Gear: ' & [GR] & ', Orient: ' & [ORIENT] & ', Gear use:' & IIf(IsNull([IA121].[GRUSE]),'9',[IA121].[GRUSE]) AS MODE_DES,
          Min(Round([EFFDUR],1)) AS EFFDUR_GE,
          Max(Int([EFFDUR])+1) AS EFFDUR_LT,
          Min(TimeSerial(Hour([EFFTM0]),0,0)) AS EFFTM0_GE,
          Max(TimeSerial(Hour([EFFTM0])+1,0,0)) AS EFFTM0_LT
          FROM IA121
          GROUP BY
          PRJ_CD,
          GR,
          IIf(IsNull([IA121].[GRUSE]),'9',[IA121].[GRUSE]),
          IIf(IsNull([IA121].[ORIENT]),'9',[IA121].[ORIENT]),
          'Gear: ' & [GR] & ', Orient: ' & [ORIENT] & ', Gear use:' & IIf(IsNull([IA121].[GRUSE]),'9',[IA121].[GRUSE])
          HAVING PRJ_CD in (%s);"
  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
