##' Fetch FN125 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN125 data in a format that matches the FN125 table in the upload
##' template.
##' @title Fetch FN125 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN125 data for the specified
##'   creel
##' @export
##' @author R. Adam Cottrill
get_creesys_fn125 <- function(prj_cd, src_db) {
  # a function replace the Get_FN125 query from the mapper database.

  sql <- "SELECT PRJ_CD, TRIM(STR([FN125].[SAM])) AS SAM, EFF, SPC, GRP, FISH, FLEN,
        TLEN, RWT, SEX, MAT, GON, CLIPC, GIRTH, AGEST, NODC, COMMENT5, TISSUE,
        0 AS FDSAM,
        '' AS EVISWT,
        '' AS GONWT,
        '' AS STOM_CONTENTS_WT
        FROM FN125
        WHERE PRJ_CD='%s'
        ORDER BY PRJ_CD, Trim(Str([FN125].[SAM])), EFF, Spc, GRP, FISH;
        "

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
