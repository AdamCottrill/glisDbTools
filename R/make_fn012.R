##' Populate the FN012 table for projects created from master databases
##'
##' Some data sources do not have a FN012 table. This funciton will
##' created a FN012 table based on values in the FN011 table (LAKE and
##' PROTOCOL), and will use the provided default if a matching
##' protocol cannot be found.  This table is further refined based on
##' the catch in each project.
##' @title Make FN011 Table
##' @param fn011 - data frame containing FN011 data. Must contain
##'   PRJ_CD, LAKE and PROTOCOL
##' @param default_protocol - the protocol to use if one matching the
##'   specifed protol and lake cannot be found
##' @return dataframe containing fn012 records for each project
##'   reported in the FN011 table.
##' @export
##' @author R. Adam Cottrill
make_fn012 <- function(fn011, default_protocol = "BSM") {
  lake <- fn011$LAKE[1]
  default_fn012 <- glfishr::get_FN012_Protocol(list(
    lake = lake,
    protocol = default_protocol
  ))
  drop <- c("LAKE", "PROTOCOL")
  default_fn012$PRJ_CD <- NA
  default_fn012 <- default_fn012[, !(names(default_fn012) %in% drop)]

  fn012 <- default_fn012[FALSE, ]

  for (i in 1:nrow(fn011)) {
    project <- fn011[i, ]
    tmp <- glfishr::get_FN012_Protocol(list(
      lake = project$LAKE,
      protocol = project$PROTOCOL
    ))

    if (length(tmp) == 0) {
      msg <- sprintf(
        "\t%s - Unable to find protocol for '%s' in Lake %s.
                       \tUsing %s for FN012 values instead.\n",
        project$PRJ_CD,
        project$PROTOCOL,
        project$LAKE,
        default_protocol
      )
      cat(msg)
      tmp <- default_fn012
    } else {
      tmp <- tmp[, !(names(tmp) %in% drop)]
    }
    tmp$PRJ_CD <- project$PRJ_CD
    fn012 <- rbind(fn012, tmp)
  }

  return(fn012)
}
