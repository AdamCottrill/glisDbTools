##' Plot GPS Tracks Data in a populated GLIS Template
##'
##' This function will create an interactive leaflet map show all of
##' the GPS tracks data in the provided template database. The SLUG
##' value corresponding to any given point can be viewed by clicking
##' on the marker on the map.  If the src_db is not an GLIS assessment
##' template, an error will be thrown.
##'
##' @title Plot GPS Tracks Data
##'
##' @param src_db path to populated glis assessment template database
##' @param fill0 - An optional string representing the colour to be
##'   used to fill the first set of points defaults to 'red'.##'
##' @param radius - An optional integer representing the size of the
##'   plotting symbol.  Defaults to 3.
##'
##' @return leaflet map
##' @export
##' @author R. Adam Cottrill
fn121_gps_tracks_map <- function(src_db, fill0 = "red", radius = 3) {
  sql <- "select PRJ_CD, SAM, TRACKID, DD_LAT, DD_LON from FN121_GPS_Tracks;"

  pts <- fetch_sql(src_db, sql)

  if (inherits(pts, "character")) {
    msg <- paste0(
      "Something went wrong. Did you try to plot ",
      "FN121_GPS_Track points for a creel project?\n"
    )
    stop(msg, pts)
  }

  pts$SLUG <- tolower(with(pts, paste(PRJ_CD, SAM, TRACKID, sep = "-")))
  pts <- pts[, c("SLUG", "DD_LAT", "DD_LON")]
  pts <- check_points(pts)
  map <- point_map(pts, fill0 = fill0, radius = radius)
  return(map)
}
