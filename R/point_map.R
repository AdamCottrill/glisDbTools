##' Plot GLIS Template Spatial Data
##'
##' This function will create a leaflet map showing the spatial
##' information in a GLIS template table.  At a minimum, the point
##' will be presented for spaces, subspace, and gps_tracks. If the
##' second array of points is optional and represents the second set
##' of coordinates for a sampling event (DD_LAT1 and DD_LON1).  If
##' this argument is provide, a second set of points will be plotted
##' in secondary colour with line segments joining each associated
##' coordinate.  Each point will include a pop-up that present the
##' user with the slug for the corresponding entity so that outliers
##' can be found in the original data set and updated.
##'
##' @title Plot GLIS Template Spatial Data
##' @param pt0s a dataframe contains slug, latitude and longitude (in
##'   that order).  The values are extracted using column index.
##' @param pt1s an optional dataframe contains slug, latitude and
##'   longitude (in that order).  The values are extracted using.
##'   column index.
##' @param fill0 - An optional string representing the colour to be
##'   used to fill the first set of points defaults to 'red'.
##' @param fill1 - An optional string representing the colour to be
##'   used to fill the first set of points defaults to 'blue'.
##' @param radius - An optional integer representing the size of the
##'   plotting symbol.  Defaults to 3.
##' @return leaflet plot
##' @export
##' @author R. Adam Cottrill
##'
point_map <- function(
    pt0s,
    pt1s = NULL,
    fill0 = "red",
    fill1 = "blue",
    radius = 3) {
  # map <- leaflet::leaflet()
  # map <- leaflet::addTiles(map)

  tile_url <- "https://ws.lioservices.lrc.gov.on.ca/arcgis1/rest/services/LIO_Cartographic/LIO_Topographic/MapServer/tile/{z}/{y}/{x}"

  imagery_url <- "https://intra.ws.lioservices.lrc.gov.on.ca/arcgis2/rest/services/LIO_Imagery/Ontario_Imagery_Web_Map_Service/MapServer/tile/{z}/{y}/{x}"

  map <- leaflet::leaflet(
    options = leaflet::leafletOptions(
      minZoom = 4,
      maxZoom = 18
    )
  )

  map <- leaflet::addTiles(
    map,
    urlTemplate = tile_url,
    options = leaflet::tileOptions(
      minZoom = 4,
      maxZoom = 15,
    )
  )

  map <- leaflet::addTiles(
    map,
    urlTemplate = imagery_url,
    options = leaflet::tileOptions(
      minZoom = 16,
    )
  )

  # add our lines if first if we can so they are under the points:
  if (!is.null(pt1s)) {
    pts <- merge(pt0s, pt1s, by = "SLUG")

    for (i in 1:nrow(pts)) {
      map <- leaflet::addPolylines(
        map,
        lng = c(pts$DD_LON0[i], pts$DD_LON1[i]),
        lat = c(pts$DD_LAT0[i], pts$DD_LAT1[i]),
        stroke = TRUE,
        color = "#707070",
        weight = 1
      )
    }
  }

  map <- leaflet::addCircleMarkers(
    map,
    popup = pt0s[, 1],
    lat = pt0s[, 2],
    lng = pt0s[, 3],
    color = "black",
    radius = radius,
    fillColor = fill0,
    fillOpacity = 0.5,
    weight = 1
  )

  if (!is.null(pt1s)) {
    map <- leaflet::addCircleMarkers(
      map,
      popup = pt1s[, 1],
      lat = pt1s[, 2],
      lng = pt1s[, 3],
      color = "black",
      radius = radius,
      fillColor = fill1,
      fillOpacity = 0.5,
      weight = 1
    )
  }

  return(map)
}
