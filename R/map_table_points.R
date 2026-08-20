##' Map spatial data in from table in a GLIS template
##'
##' This function will produce interactive leaflet maps for the
##' spatial data contained in the FN026, FN026_subspace, FN121, or
##' FN121_GPS_Tracks tables.  If the table has two coordinates for
##' each record, the map will present both points connected by a line
##' segment to represent the association.  Warning are produced if any
##' of the cordinates are invalid or fall outside the bounds of the t
##' Great Lakes.  Clicking on the markers will render a pop-up that
##' will present the SLUG of the object so that it can be found in the
##' source database.
##'
##' @title Map Spatial Data in GLIS Template
##' @param src_db - path to populated glis template database
##' @param table_name - one of "FN026", "FN026_Subspace", "FN121", or
##'   "FN121_GPS_Tracks".  If either of the FN121 tables are selected
##'   and the src_db is a creel_template an error will be thrown.  The
##'   SC121 has been provided to plot spatial data collected during
##'   creel interviews.
##' @param fill0 - An optional string representing the colour to be
##'   used to fill the first set of points defaults to 'red'.
##' @param fill1 - An optional string representing the colour to be
##'   used to fill the first set of points defaults to 'blue'.
##' @param radius - An optional integer representing the size of the
##'   plotting symbol.  Defaults to 3.
##' @export
##' @return leaflet map
##' @author R. Adam Cottrill
map_table_points <- function(
    src_db,
    table_name = c(
      "FN026",
      "FN026_Subspace",
      "FN121",
      "FN121_GPS_Tracks",
      "SC121"
    ),
    fill0 = "red",
    fill1 = "blue",
    radius = 3) {
  table_name <- match.arg(table_name)

  mymap <- switch(table_name,
    FN026 = fn026_map(src_db, fill0 = fill0, radius = radius),
    FN026_Subspace = fn026_subspace_map(src_db, fill0 = fill0, radius = radius),
    FN121 = fn121_map(src_db, fill0 = fill0, fill1 = fill1, radius = radius),
    FN121_GPS_Tracks = fn121_gps_tracks_map(
      src_db,
      fill0 = fill0,
      radius = radius
    ),
    SC121 = sc121_map(src_db, fill0 = fill0, radius = radius),
  )
  return(mymap)
}
