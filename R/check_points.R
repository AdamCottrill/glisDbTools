##' Verify point data is complete and within bounds of the Great Lakes.
##'
##' This function is used to verify that the coordinates used for
##' plotting are complete and have values that fall within the
##' (buffered) extent of the Great Lakes. Any values that are
##' incomplete or are outside of the bounds will be removed from the
##' returned dataset.  A warning will be issued that presents the
##' offending record(s).
##' @title Verify point data for plotting.
##' @param pts - dataframe containing a slug/label, latitude and
##'   longitude to be checked.  Data is extracted by index, so column
##'   names don't matter and extra columns will be ignored.
##' @return dataframe
##' @export
##' @author R. Adam Cottrill
check_points <- function(pts) {
  # pull out any points that are: missing lat, missing lon, or have a
  # lat or lon outside of some gross bounds if there are any points
  # satisfy that those criteria print out a warning and remove them
  # from the returned dataset.

  # strip out any records where both lat and lon are empty:
  empty <- pts$SLUG[(is.na(pts[1]) & is.na(pts[2]))]

  # intentionally broader than process validate to ensure values are
  # plotted on the map:
  MIN_LAT <- 40.0
  MAX_LAT <- 50.0
  MIN_LONG <- -90.0
  MAX_LONG <- -74.0

  problems <- pts$SLUG[
    is.na(pts[, 2]) |
      is.na(pts[, 3]) |
      # bad lat:
      pts[, 2] < MIN_LAT |
      pts[, 2] > MAX_LAT |
      # bad lon:
      pts[, 3] < MIN_LONG |
      pts[, 3] > MAX_LONG
  ]

  if (length(problems) > 0) {
    msg <- "There was a problem with the following points:\n"
    warning(msg, print_and_capture(pts[(pts$SLUG %in% problems), ]))

    pts <- pts[!(pts$SLUG %in% problems), ]
  }

  return(pts)
}
