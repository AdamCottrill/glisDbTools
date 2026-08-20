##' Fetch FN125 data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' FN125 data in a format that matches the FN125 table in the upload
##' template.
##' @title Fetch FN125 data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN125 data for the specified
##'   assessment project
##' @export
##' @author R. Adam Cottrill
get_nearshore_fn125 <- function(prj_cds, src_db) {
  # a function replace the Get_FN125 query from the mapper database.

  sql <- "
      SELECT PRJ_CD,
      Trim(Str([ia125].[SAM])) AS SAM,
      EFF,
      SPC,
      GRP,
      FISH,
      FLEN,
      TLEN,
      RWT,
      SEX,
      MAT,
      GON,
      GONWT,
      CLIPC,
      GIRTH,
      AGEST,
      NODA,
      NODC,
      IIf(IsNull([ia125].[fate]),'K',[ia125].[fate]) AS FATE,
      COMMENT5,
      CLIPA,
      TISSUE,
      '' AS EVISWT,
      '' AS FDSAM,
      '' AS STOM_CONTENTS_WT
      FROM IA125
      WHERE PRJ_CD in (%s)
      ORDER BY PRJ_CD, Trim(Str([ia125].[SAM])), EFF, SPC, GRP, FISH;"
  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
