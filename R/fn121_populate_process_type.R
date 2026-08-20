##' Populate FN121 Process Type based on gear and FN122 records
##'
##' This function is used to populate the process type assocaited with
##' each FN121 record by using the gear from the FN028 table, the
##' number of child fn122 records and the known gear effort process
##' types.
##' @title Populate FN121 Process Type
##' @param fn028 - fn028 table for the selected project(s)
##' @param fn121 - fn121 table for the selected project(s)
##' @param fn122 - fn122 table for the selected project(s)
##' @param gear_effort_process_types datafame with gear, effort and
##'   process types.
##' @return fn121 dataframe with populated PROCESS_TYPE column
##' @export
##' @author R. Adam Cottrill
fn121_populate_process_type <- function(
    fn028,
    fn121,
    fn122,
    gear_effort_process_types) {
  eff_counts <- stats::aggregate(EFF ~ PRJ_CD + SAM, data = fn122, FUN = length)
  gept_counts <- stats::aggregate(
    EFF ~ GR + PROCESS_TYPE,
    data = gear_effort_process_types,
    FUN = length
  )

  prj_sam_gr <- merge(
    fn121[, c("PRJ_CD", "SAM", "MODE")],
    fn028[, c("PRJ_CD", "MODE", "GR")],
    by = c("PRJ_CD", "MODE"),
    all.x = TRUE
  )

  # add the effort conts to our project sam_gr:

  prj_sam_gr <- merge(
    prj_sam_gr[, c("PRJ_CD", "SAM", "GR")],
    eff_counts[, c("PRJ_CD", "SAM", "EFF")],
    by = c("PRJ_CD", "SAM"),
    all.x = TRUE
  )

  prj_sam_gr <- merge(
    prj_sam_gr,
    gept_counts,
    by = c("GR", "EFF"),
    all.x = TRUE
  )

  prj_sam_gr$PROCESS_TYPE <- ifelse(
    is.na(prj_sam_gr$PROCESS_TYPE) &
      prj_sam_gr$EFF == 1,
    1,
    prj_sam_gr$PROCESS_TYPE
  )
  prj_sam_gr <- prj_sam_gr[, c("PRJ_CD", "SAM", "PROCESS_TYPE")]

  fn121$PROCESS_TYPE <- NULL
  fn121 <- merge(fn121, prj_sam_gr, by = c("PRJ_CD", "SAM"), all.x = TRUE)

  return(fn121)
}
