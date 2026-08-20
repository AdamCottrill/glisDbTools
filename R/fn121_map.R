##' Plot FN121 Spatial Data contained in a populated GLIS Template
##'
##' This function will create an interactive leaflet map show all of
##' the FN121 lat-lon data in the provided template database. If both
##' DD_LAT0/DD_LON0 and DD_LAT1/DD_LON1 data is available, the map
##' will includes pairs of points connected by a line segment.  The
##' SLUG value corresponding to any given point can be viewed by
##' clicking on the marker on the map.  If the src_db is not an GLIS
##' assessment template, an error will be thrown.
##'
##' @title Plot FN121 Spatial Data
##'
##' @param src_db path to populated glis assessment template database
##' @param fill0 - An optional string representing the colour to be
##'   used to fill the first set of points. Defaults to 'red'.
##' @param fill1 - An optional string representing the colour to be
##'   used to fill the second set of points. Defaults to 'blue'.
##' @param radius - An optional integer representing the size of the
##'   plotting symbol.  Defaults to 3.
##'
##' @return leaflet map
##' @export
##' @author R. Adam Cottrill
fn121_map <- function(src_db, fill0 = "red", fill1 = "blue", radius = 3) {
  sql <- "select PRJ_CD, SAM, DD_LAT0, DD_LON0, DD_LAT1, DD_LON1 from fn121;"

  pts <- fetch_sql(src_db, sql)

  if (inherits(pts, "character")) {
    msg <- paste0(
      "Something went wrong. ",
      "If this is a template please use 'SC121' as the table_name.\n"
    )
    stop(msg, pts)
  }

  pts$SLUG <- tolower(with(pts, paste(PRJ_CD, SAM, sep = "-")))

  pt0s <- pts[, c("SLUG", "DD_LAT0", "DD_LON0")]
  pt1s <- pts[, c("SLUG", "DD_LAT1", "DD_LON1")]

  pt0s <- check_points(pt0s)
  pt1s <- check_points(pt1s)

  map <- point_map(pt0s, pt1s, fill0 = fill0, fill1 = fill1, radius = radius)
  return(map)
}
