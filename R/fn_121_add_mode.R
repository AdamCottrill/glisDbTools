##' Add mode to FN121 based on GR, GRUSE and ORIENT
##'
##' This function is used by the nearshore and offshore mapping
##' funcitons to populate the correct MODE value based on the records
##' in the FN028 table and the values of GEAR, GEAR_USE and ORIENT
##' specified in each FN121 record.
##' @title Add Mode to FN121
##' @param fn121 - dataframe representing sampling events (net sets)
##' @param fn028 - dataframe representing available modes (set methods)
##' @return fn121 dataframe with populated mode field added
##' @export
##' @author R. Adam Cottrill
fn121_add_mode <- function(fn121, fn028) {
  # populate the correct mode for each sam:
  x121 <- fn121[, c("PRJ_CD", "SAM", "GR", "GRUSE", "ORIENT")]
  x028 <- fn028[, c("PRJ_CD", "GR", "GRUSE", "ORIENT", "MODE")]
  tmp <- merge(x121, x028, by = c("PRJ_CD", "GR", "GRUSE", "ORIENT"))
  fn121 <- merge(
    fn121,
    tmp,
    by = c("PRJ_CD", "SAM", "GR", "GRUSE", "ORIENT"),
    all.x = TRUE
  )
  drop <- c("GR", "GRUSE", "ORIENT")
  return(fn121[, !(names(fn121) %in% drop)])
}
