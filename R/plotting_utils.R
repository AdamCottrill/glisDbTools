##' Plot Process Validate FN125 data against FN012 constraints
##'
##' This function will create plots associated with some of the
##' process validate queries that check biological attributes against
##' constrains in the FN012 table.  Currently, plots for fork length,
##' total length, round weight, and condition factor (based on fork or
##' total length) are supported.  In most cases, the generated plot
##' includes a density plot showing the distribution of the observed
##' variable for specified species, and a rug plot illustrating
##' individual observations.  The plot will also include vertical
##' lines that illustrate the associated limits from the FN012 table.
##' Values outside of these limits are highlighted.  The function
##' takes an optional argument (glis_fish) that can be used to provide
##' additional context to evaluate data in the current project against
##' - this is particularly helpful for species that are often
##' encountered in small number in a single project.
##'
##' @title Plot FN125 data against FN012 constraints
##' @param src Path to the populated template database.  realative
##'   paths are supported. An error will be thrown if the database
##'   cannot be found or is not an accdb file.
##' @param spc Three character species code for the species to plot
##' @param grp The group of the spc+grp to select. Defaults to '00'
##' @param what The bioligical attribute to plot - must be on of
##'   "FLEN", "TLEN", "RWT", "Kflen", or "Ktlen".
##' @param glis_fish - an optional dataframe that can be used to
##'   prodive additional biological data for more context.
##' @return a plot
##' @export
##' @author R. Adam Cottrill
procval_plot <- function(src, spc, grp = "00",
                         what = c("FLEN", "TLEN", "RWT", "Kflen", "Ktlen"),
                         glis_fish = NULL) {
  what <- match.arg(what)

  sql012 <- sprintf("SELECT * from FN012 where [spc] = '%s' AND GRP='%s'", spc, grp)
  species_attributes <- fetch_sql(src, sql012)

  sql125 <- sprintf("SELECT [PRJ_CD], [SAM], [EFF], [SPC], [GRP], [FISH], [FLEN], [TLEN], [RWT] from [FN125] where [spc] ='%s' AND GRP='%s'", spc, grp)
  fish <- fetch_sql(src, sql125)
  fish$attr <- NA

  if (!is.null(glis_fish)) {
    glis_fish <- glis_fish[, c("PRJ_CD", "SAM", "EFF", "SPC", "GRP", "FISH", "FLEN", "TLEN", "RWT")]
    glis_fish$attr <- "reference"
    fish <- rbind(fish, glis_fish)
  }

  if (what == "Ktlen") {
    fish$K <- 100000 * fish$RWT / (fish$TLEN^3)
  } else {
    fish$K <- 100000 * fish$RWT / (fish$FLEN^3)
  }

  if (what %in% c("Kflen", "Ktlen")) {
    condition_plots(fish, species_attributes, what)
  } else {
    bioattr_plot(fish, species_attributes, what)
  }
}



##' Two pannel plot of Condition Factor
##'
##' This funciton will produce a two-panel plot showing the
##' distrubtion of condition factor values reported in a template
##' database.  The panel on the left will contain a density plot of
##' the observed condition factors as well as the upper and lower
##' warning and error values.  Observations outside of those bounds
##' will be highlighted.  The left plot will show weight vs length,
##' including curves that represent the bounds of the the upper and
##' lower warning and error values.
##' @title Two pannel plot of Condition Factor
##' @param fish - dataframe containing basic FN125 data (FLEN,TLEN,
##'   RWT, K)
##' @param species_attributes - single row dataframe created by
##'   subsettting FN012 for the given species and grp.
##' @param what The type of condiition factor to plot - must be either
##'   "Kflen" for Fulton's K calculated using Fork Length or , or
##'   "Ktlen" Fulton's K calculated using Total Length.
##' @return - plot
##' @author R. Adam Cottrill
condition_plots <- function(fish, species_attributes, what = c("Kflen", "Ktlen")) {
  what <- match.arg(what)
  what <- toupper(gsub("K", "", what))

  var_idx <- which(names(fish) == what)

  fish$attr[is.na(fish$attr) &
    !is.na(fish$K) &
    fish$K < species_attributes$K_MAX_WARN &
    fish$K > species_attributes$K_MIN_WARN] <- "ok"

  fish$attr[is.na(fish$attr) &
    !is.na(fish$K) &
    fish$K >= species_attributes$K_MAX_WARN &
    fish$K < species_attributes$K_MAX_ERROR] <- "too_big"

  fish$attr[is.na(fish$attr) &
    !is.na(fish$K) &
    fish$K >= species_attributes$K_MAX_ERROR] <- "way_too_big"

  fish$attr[is.na(fish$attr) &
    !is.na(fish$K) &
    fish$K <= species_attributes$K_MIN_WARN &
    fish$K > species_attributes$K_MIN_ERROR] <- "too_small"

  fish$attr[is.na(fish$attr) &
    !is.na(fish$K) &
    fish$K <= species_attributes$K_MIN_ERROR] <- "way_too_small"

  graphics::split.screen(c(1, 2))
  graphics::screen(1) #

  with(fish, plot(stats::density(K, na.rm = T), main = "Condition Factor"))

  # density of observed K

  graphics::rug(fish$K[fish$attr == "reference"], col = "grey")

  graphics::rug(fish$K[fish$attr == "ok"], col = "black")
  graphics::rug(fish$K[fish$attr == "too_big"], col = "orange")
  graphics::rug(fish$K[fish$attr == "way_too_big"], col = "red")
  graphics::rug(fish$K[fish$attr == "too_small"], col = "steelblue")
  graphics::rug(fish$K[fish$attr == "way_too_small"], col = "navy")

  graphics::abline(v = species_attributes$K_MAX_ERROR, col = "red")
  graphics::abline(v = species_attributes$K_MIN_ERROR, col = "navy")

  graphics::abline(v = species_attributes$K_MAX_WARN, col = "orange", lty = 3)
  graphics::abline(v = species_attributes$K_MIN_WARN, col = "steelblue", lty = 3)

  graphics::screen(2)

  fish <- fish[!is.na(fish$RWT) & !is.na(fish[, var_idx]), ]

  # Length vs weight
  main_label <- sprintf("RWT vs %s", what)
  xlabel <- sprintf("%s (mm)", what)
  plot(
    y = fish$RWT, x = fish[, var_idx],
    xlab = xlabel, ylab = "RWT (g)",
    type = "n", main = main_label
  )

  # a factory function to return a curve with the given condition parameter:
  kcurve <- function(condition) {
    fct <- function(x) (condition * x^3) / 100000
    return(fct)
  }


  # kmin_err <- function(x) (species_attributes$K_MIN_ERROR * x^3) / 100000
  kmin_err <- kcurve(species_attributes$K_MIN_ERROR)
  graphics::curve(kmin_err, add = TRUE, col = "navy")

  # kmin_warn <- function(x) (species_attributes$K_MIN_WARN * x^3) / 100000
  kmin_warn <- kcurve(species_attributes$K_MIN_WARN)
  graphics::curve(kmin_warn, add = TRUE, col = "steelblue", lty = 3)

  # kmax_err <- function(x) (species_attributes$K_MAX_ERROR * x^3) / 100000
  kmax_err <- kcurve(species_attributes$K_MAX_ERROR)
  graphics::curve(kmax_err, add = TRUE, col = "red")

  # kmax_warn <- function(x) (species_attributes$K_MAX_WARN * x^3) / 100000
  kmax_warn <- kcurve(species_attributes$K_MAX_WARN)
  graphics::curve(kmax_warn, add = TRUE, col = "orange", lty = 3)


  graphics::points(fish[fish$attr == "reference", var_idx], fish$RWT[fish$attr == "reference"], col = "grey")
  graphics::points(fish[fish$attr == "ok", var_idx], fish$RWT[fish$attr == "ok"], col = "black")
  graphics::points(fish[fish$attr == "too_big", var_idx], fish$RWT[fish$attr == "too_big"], col = "orange", pch = 19)
  graphics::points(fish[fish$attr == "way_too_big", var_idx], fish$RWT[fish$attr == "way_too_big"], col = "red", pch = 8)
  graphics::points(fish[fish$attr == "too_small", var_idx],
    fish$RWT[fish$attr == "too_small"],
    col = "steelblue", pch = 19
  )
  graphics::points(fish[fish$attr == "way_too_small", var_idx], fish$RWT[fish$attr == "way_too_small"], col = "navy", pch = 8)

  graphics::close.screen(all = TRUE)
}


##' Plot Density of FN125 data against FN012 constraints
##'
##' This function will create plot a density plot for the specified
##' attribute reported in a template database and superimpose those
##' values against the constrains in the FN012 table. Individual
##' observations are presented as a rug plot while the FN012 limits
##' are presented as vertical lines.  Values outside of these limits
##' are highlighted.  The function takes an optional argument
##' (glis_fish) that can be used to provide additional context to
##' evaluate the data in the in the current project against - this is
##' particularly helpful for species that are often encountered in
##' small number in a single project, but may exists in sufficient
##' numbers in other sources.
##'
##' @title Plot FN125 data against FN012 constraints##'
##' @param fish - dataframe containing basic FN125 data (FLEN,TLEN,
##'   RWT)
##' @param species_attributes - single row dataframe created by
##'   subsettting FN012 for the given species and grp.
##' @param what The type of condiition factor to plot - must be one of
##'   FLEN, TLEN or RWT
##'
##' @return plot
##' @author R. Adam Cottrill
bioattr_plot <- function(fish, species_attributes, what = c("FLEN", "TLEN", "RWT")) {
  what <- match.arg(what)

  # get the column numbers for data we want to plot depending on what has been selected
  var_idx <- which(names(fish) == what)
  min_idx <- which(names(species_attributes) == sprintf("%s_MIN", what))
  max_idx <- which(names(species_attributes) == sprintf("%s_MAX", what))


  fish$attr[is.na(fish$attr) &
    !is.na(fish[, var_idx]) &
    fish[, var_idx] <= species_attributes[, max_idx] &
    fish[, var_idx] >= species_attributes[, min_idx]] <- "ok"

  fish$attr[is.na(fish$attr) &
    !is.na(fish[, var_idx]) &
    fish[, var_idx] > species_attributes[, max_idx]] <- "too_big"

  fish$attr[is.na(fish$attr) &
    !is.na(fish[, var_idx]) &
    fish[, var_idx] < species_attributes[, min_idx]] <- "too_small"

  plot(stats::density(fish[, var_idx], na.rm = T), main = what)

  graphics::rug(fish[fish$attr == "reference", var_idx], col = "grey")
  graphics::rug(fish[fish$attr == "ok", var_idx], col = "black")
  graphics::rug(fish[fish$attr == "too_big", var_idx], col = "red")
  graphics::rug(fish[fish$attr == "too_small", var_idx], col = "red")

  graphics::abline(v = species_attributes[1, min_idx], col = "red", lty = 3)
  graphics::abline(v = species_attributes[1, max_idx], col = "red", lty = 3)
}





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
##'   and the src_db is a creel_template an error will be thrown.
##' @param fill0 - An optional string representing the colour to be
##'   used to fill the first set of points defaults to 'red'.
##' @param fill1 - An optional string representing the colour to be
##'   used to fill the first set of points defaults to 'blue'.
##' @param radius - An optional integer representing the size of the
##'   plotting symbol.  Defaults to 3.
##'  @export
##'
##' @return leaflet map
##' @author R. Adam Cottrill
map_table_points <- function(
    src_db, table_name = c("FN026", "FN026_Subspace", "FN121", "FN121_GPS_Tracks"),
    fill0 = "red", fill1 = "blue", radius = 3) {
  table_name <- match.arg(table_name)

  mymap <- switch(table_name,
    FN026 = fn026_map(src_db, fill0 = fill0, radius = radius),
    FN026_Subspace = fn026_subspace_map(src_db, fill0 = fill0, radius = radius),
    FN121 = fn121_map(src_db, fill0 = fill0, fill1 = fill1, radius = radius),
    FN121_GPS_Tracks = fn121_gps_tracks_map(src_db, fill0 = fill0, radius = radius)
  )

  return(mymap)
}



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
}


##' Plot FN026 Subspace Spatial Data contained in a populated GLIS Template
##'
##' This function will create an interactive leaflet map show all of
##' the FN026 Subspace data in the provided template database. The
##' SLUG value corresponding to any given point can be viewed by
##' clicking on the marker on the map. This function will work with
##' both creel and assessment templates.
##'
##' @title Plot FN026_Subspace Spatial Data
##'
##' @param src_db path to populated glis template database
##' @param fill0 - An optional string representing the colour to be
##'   used to fill the first set of points. Defaults to 'red'.
##' @param radius - An optional integer representing the size of the
##'   plotting symbol.  Defaults to 3.
##'
##' @return leaflet map
##' @export
##' @author R. Adam Cottrill
fn026_subspace_map <- function(src_db, fill0 = "red", radius = 3) {
  sql <- "select PRJ_CD, SPACE, SUBSPACE, DD_LAT, DD_LON from fn026_subspace;"
  pts <- fetch_sql(src_db, sql)
  pts$SLUG <- tolower(with(pts, paste(PRJ_CD, SPACE, SUBSPACE, sep = "-")))
  pts <- pts[, c("SLUG", "DD_LAT", "DD_LON")]
  pts <- check_points(pts)
  map <- point_map(pts, fill0 = fill0, radius = radius)
}




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
    msg <- "Something went wrong. Did you try to plot FN121 points for a creel project?\n"
    stop(msg, pts)
  }

  pts$SLUG <- tolower(with(pts, paste(PRJ_CD, SAM, sep = "-")))

  pt0s <- pts[, c("SLUG", "DD_LAT0", "DD_LON0")]
  pt1s <- pts[, c("SLUG", "DD_LAT1", "DD_LON1")]

  pt0s <- check_points(pt0s)
  pt1s <- check_points(pt1s)
  # check_point_pairs(pt0s, pt1s)
  map <- point_map(pt0s, pt1s, fill0 = fill0, fill1 = fill1, radius = radius)
}


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
    msg <- "Something went wrong. Did you try to plot FN121_GPS_Track points for a creel project?\n"
    stop(msg, pts)
  }

  pts$SLUG <- tolower(with(pts, paste(PRJ_CD, SAM, TRACKID, sep = "-")))
  pts <- pts[, c("SLUG", "DD_LAT", "DD_LON")]
  pts <- check_points(pts)
  map <- point_map(pts, fill0 = fill0, radius = radius)
}


##' Capture inforamtion printed to the console
##'
##' A simple little helper to capture information printed by the
##' console and return it as a string.
##'
##' @title Print and Capture
##' @param x - the R object to print.
##'
##' @return string
##' @author R. Adam Cottrill
print_and_capture <- function(x) {
  paste(utils::capture.output(print(x)), collapse = "\n")
}


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
  MIN_LONG <- -85.0
  MAX_LONG <- -74.0

  problems <- pts$SLUG[is.na(pts[, 2]) | is.na(pts[, 3]) |
    # bad lat:
    pts[, 2] < MIN_LAT | pts[, 2] > MAX_LAT |
    # bad lon:
    pts[, 3] < MIN_LONG | pts[, 3] > MAX_LONG]

  if (length(problems) > 0) {
    msg <- "There was a problem with the following points:\n"
    warning(msg, print_and_capture(pts[(pts$SLUG %in% problems), ]))

    pts <- pts[!(pts$SLUG %in% problems), ]
  }

  return(pts)
}


##' Check Point Pairs for missing or incomplete coordinates
##'
##' A helper function to remove any missing/invalid net-set pairs
##' before they are passed to the mapping function.  If there is any
##' issue with a pair, a warning is issued and offending pairs removed
##' from the returned data-fame.
##'
##' @title Check Point Pairs
##' @param ptsA dataframe containing the fields SLUG, DD_LAT0 and
##'   DD_LON0
##' @param ptsB dataframe containing the fields SLUG, DD_LAT1 and
##'   DD_LON1
##' @return dataframe
##' @author R. Adam Cottrill
check_point_pairs <- function(ptsA, ptsB) {
  # compare the slugs in ptsA and Pts B (which have both already been
  # scrubbed) and verify that each point in A has a matching point in
  # B. Issue a warning if there are any unmatched pairs.
  pts <- merge(ptsA, ptsB, by = "SLUG")

  problems <- pts$SLUG[is.na(pts$DD_LAT0) | is.na(pts$DD_LON0) |
    is.na(pts$DD_LAT1) | is.na(pts$DD_LON1)]

  if (length(problems) > 0) {
    msg <- "There was a problem with the following point pairs:\n"
    warning(msg, print_and_capture(pts[(pts$SLUG %in% problems), ]))
    pts <- pts[!(pts$SLUG %in% problems), ]
  }

  return(pts)
}


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
##' @author R. Adam Cottrill
point_map <- function(pt0s, pt1s = NULL, fill0 = "red", fill1 = "blue", radius = 3) {
  map <- leaflet::leaflet()
  map <- leaflet::addTiles(map)

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

  map <- leaflet::addCircleMarkers(map,
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
    map <- leaflet::addCircleMarkers(map,
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
