##' Plot FN026 Spatial Data contained in a populated GLIS Template
##'
##' This function will create an interactive leaflet map show all of
##' the FN026 data in the provided template database. The
##' SLUG value corresponding to any given point can be viewed by
##' clicking on the marker on the map. This function will work with
##' both creel and assessment templates.
##'
##' @title Plot FN026 Spatial Data
##'
##' @param src_db - path to populated glis template database
##' @param fill0 - An optional string representing the colour to be
##'   used to fill the first set of points. Defaults to 'red'.
##' @param radius - An optional integer representing the size of the
##'   plotting symbol.  Defaults to 3.
##'
##' @return leaflet map
##' @export
##' @author R. Adam Cottrill
fn026_map <- function(src_db, fill0 = "red", radius = 3) {
  sql <- "select PRJ_CD, SPACE, DD_LAT, DD_LON from fn026;"
  pts <- fetch_sql(src_db, sql)
  pts$SLUG <- tolower(with(pts, paste(PRJ_CD, SPACE, sep = "-")))
  pts <- pts[, c("SLUG", "DD_LAT", "DD_LON")]
  pts <- check_points(pts)
  map <- point_map(pts, fill0 = fill0, radius = radius)
  return(map)
}
