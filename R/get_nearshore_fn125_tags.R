##' Fetch FN125_tags data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' FN125_tags data in a format that matches the FN125_tags table in the upload
##' template.
##' @title Fetch FN125_tags data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN125_tags data for the specified
##'   assessment project
##' @export
##' @author R. Adam Cottrill
get_nearshore_fn125_tags <- function(prj_cds, src_db) {
  # a function replace the Get_FN125 query from the mapper database.

  sql <- "SELECT PRJ_CD, SAM, EFF, SPC, GRP, FISH,
          1 AS FISH_TAG_ID,
          TAGID,
          TAGDOC,
          TAGSTAT,
          [xcwtseq] AS CWTSEQ,
          '' AS COMMENT_TAG
          FROM IA125
          WHERE PRJ_CD in (%s)
          AND TAGID Is Not Null
          And TAGID<>'0';"

  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(src_db, stmt)
  return(dat)
}


get_nearshore_fn125_xtags <- function(prj_cds, src_db) {
  # to fetch any tag data contained in XTAGID fields.  A warning will
  # be printed if this query returns any resutls because TAGDOC and
  # TAGSTAT will have to be verified in the populated template:
  sql <- "SELECT PRJ_CD, SAM, EFF, SPC, GRP, FISH,
          1 AS FISH_TAG_ID,
          XTAGID AS TAGID,
          TAGDOC,
          TAGSTAT,
          [XCWTSEQ] AS CWTSEQ,
          '' AS COMMENT_TAG
          FROM IA125
          WHERE PRJ_CD in (%s)
          AND XTAGID Is Not Null
          And XTAGID<>'0';"

  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
