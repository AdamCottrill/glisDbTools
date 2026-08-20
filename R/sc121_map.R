##' Plot SC121 Spatial Data contained in a GLIS Creel Template
##'
##' This function will create an interactive leaflet map show all of
##' the FN121 lat-lon data in the provided template database.  The
##' SLUG value corresponding to any given point can be viewed by
##' clicking on the marker on the map.  If the src_db is not an GLIS
##' creel template, an error will be thrown.
##'
##' @title Plot SC121 Intervew Points
##'
##' @param src_db path to populated glis creel template database
##' @param fill0 - An optional string representing the colour to be
##'   used to fill the first set of points. Defaults to 'red'.
##' @param radius - An optional integer representing the size of the
##'   plotting symbol.  Defaults to 3.
##'
##' @return leaflet map
##' @author R. Adam Cottrill
sc121_map <- function(src_db, fill0 = "red", radius = 3) {
  sql <- "select PRJ_CD, SAM, DD_LAT, DD_LON from fn121;"

  pts <- fetch_sql(src_db, sql)

  if (inherits(pts, "character")) {
    msg <- paste0(
      "Something went wrong. ",
      "If this is an assessment template please use 'FN121' as the table name?\n"
    )
    stop(msg, pts)
  }

  pts$SLUG <- tolower(with(pts, paste(PRJ_CD, SAM, sep = "-")))

  pt0s <- pts[, c("SLUG", "DD_LAT", "DD_LON")]

  pt0s <- check_points(pt0s)

  map <- point_map(pt0s, fill0 = fill0, radius = radius)
  return(map)
}
