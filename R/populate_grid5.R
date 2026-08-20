##' Populate GRID5 in target database
##'
##' This function will populate the Grid5 column in the FN121 table of
##' the target database using the corrdinates provided by DD_LAT0 and
##' DD_LON0 and the grid-5 api exposed by GLIS.  If verbose is TRUE, a
##' progress/summary report will be printed out to the console. If
##' overwrite it FALSE, any existing values in GRID5 will be
##' maintained.  The grid values can be either left padded numbers
##' "0123" or slugs which include a the lake prefix ('hu-0123').
##'
##' @title Populate GRID5
##' @param db - path the the target accdb file
##' @param verbose - should a report be printed to the console?
##' @param overwrite  - if the target databae has grid5 values, should they be replaced?
##' @param grid_format - either slug (with lake prefix) or left padded number?
##' @return NULL
##' @author R. Adam Cottrill
##'
##' @export
##'
##' @examples
##' \dontrun{
##' # populate the grid-5 values, print a report overwrite any
##' # existing grid5 values with the grid5-slug
##' populate_grid5(db, verbose=TRUE, overwrite=T)
##'
##' # populate the grid-5 values, print a report, but do overwrite any
##' # existing grid5 values with the grid5-slug
##' populate_grid5(db, verbose=TRUE, overwrite=F)
##'
##' # do the same thing without the report
##' populate_grid5(db, verbose=FALSE, overwrite=F)
##'
##' # populate the grid-5 values, print a report, including any
##' # exisiting values that don't match.  but do not overwrite any
##' # existing grid5 values.  Use the grid5 number (without the lake
##' # prefix)
##' populate_grid5(db, verbose=TRUE, overwrite=FALSE)
##'
##' }
populate_grid5 <- function(
    db,
    verbose = TRUE,
    overwrite = FALSE,
    grid_format = c("slug", "number")) {
  grid_format <- match.arg(grid_format)
  ## check_accdb(db)
  ## sql <- "select [PRJ_CD], [SAM], [DD_LAT0], [DD_LON0], [GRID5] from FN121;"

  ## sam_points <- fetch_sql(db, sql)

  check_accdb(db)
  sql <- "select [PRJ_CD], [SAM], [DD_LAT0], [DD_LON0], [GRID5] from [FN121]
          WHERE  [DD_LAT0] is not null AND [DD_LON0] IS NOT NULL;"

  sam_points <- fetch_sql(db, sql)
  sam_points$slug <- with(sam_points, paste(DD_LAT0, DD_LON0, sep = ";"))

  unique_points <- unique(sam_points[, c("slug", "DD_LAT0", "DD_LON0")])
  names(unique_points) <- c("slug", "dd_lat", "dd_lon")
  grid5s <- glfishr::get_grid5s_from_points(unique_points)

  sam_points <- merge(
    sam_points,
    grid5s[, c("slug", "grid5_slug")],
    all.x = TRUE
  )

  # strip off the lake prefix if we want just the grid number:
  if (grid_format == "number") {
    sam_points$grid5_slug <- gsub(
      "^[a-zA-Z]{2}(-|_)",
      "",
      sam_points$grid5_slug
    )
  }

  if (verbose) {
    report_populate_grid5(sam_points, overwrite)
  }

  # keep just the columns we need
  sam_points <- sam_points[, c("PRJ_CD", "SAM", "grid5_slug")]

  # now save the predicted grid in a temporary table in the target
  append_data(
    db,
    trg_table = "tmp_predicted_grid5",
    data = sam_points,
    append = F,
    check_names = F,
    safer = F
  )

  # update the FN121 table with our predicted grid, overwrite any existing values if requested:
  sql <- "UPDATE [FN121] INNER JOIN [tmp_predicted_grid5]
        ON FN121.SAM = tmp_predicted_grid5.SAM
        AND FN121.PRJ_CD = tmp_predicted_grid5.PRJ_CD
        SET FN121.GRID5 = [tmp_predicted_grid5].[grid5_slug]"

  if (!overwrite) {
    sql <- paste(sql, " WHERE FN121.GRID5 is NULL;")
  }

  conn <- RODBC::odbcConnectAccess2007(
    db,
    uid = "",
    pwd = "",
    case = "nochange"
  )
  RODBC::sqlQuery(conn, sql)
  RODBC::sqlQuery(conn, "DROP TABLE [tmp_predicted_grid5];")
  RODBC::odbcClose(conn)
}


##' Print summary report of populate_grid5 function
##'
##' a helper function that is used to print a summary of the
##' populate_grid5 progress including the number of samples, unique
##' points, grids that could be match, and any issues.  This function is
##' not intended to be used anywere outside of populate_grid5
##'
##' Nothing is returned from this function, but messages are reported
##' to the console depending on the contents of sample_points and the
##' value of overwrite.
##'
##' @title Report on populate_grid5 progress
##' @param sam_points - data frame containing the columns: PRJ_CD,
##'   SAM, DD_LAT0, DD_LON0, GRID5, and grid5_slug.
##' @param overwrite - boolean - are existing grid5 values to be
##'   replaced?
##' @return NULL
##' @author R. Adam Cottrill
report_populate_grid5 <- function(sam_points, overwrite) {
  # found x unique points
  n <- nrow(sam_points)
  what <- if (n == 1) "record" else "records"
  msg <- sprintf("Found %s FN121 %s with both DD_LAT0 and DD_LON0.\n", n, what)
  cat(msg)

  unique_points <- unique(sam_points[, c("DD_LAT0", "DD_LON0")])
  n <- nrow(unique_points)
  what <- if (n == 1) "point" else "points"
  msg <- sprintf("Found %s unique %s.\n", n, what)
  cat(msg)

  # matched y unique grid5 values
  n <- nrow(sam_points[!is.na(sam_points$grid5_slug), ])
  what <- if (n == 1) "record" else "records"
  msg <- sprintf("Matched %s FN121 %s to a grid5\n", n, what)
  cat(msg)

  # FN121 records could not be matched to a grid5 based on their lat-lon
  unmatched <- sam_points[is.na(sam_points$grid5_slug), ]
  n <- nrow(unmatched)
  if (n) {
    what <- if (n == 1) "record" else "records"
    msg <- sprintf(
      "%s fn121 %s could not be matched up to a 5-minute grid\n",
      n,
      what
    )
    cat(msg)
    print("For example:")
    print(utils::head(unmatched[, c(
      "PRJ_CD",
      "SAM",
      "DD_LAT0",
      "DD_LON0",
      "GRID5",
      "grid5_slug"
    )]))
  }

  mismatched <- sam_points[
    !is.na(sam_points$grid5_slug) & sam_points$grid5_slug != sam_points$GRID5,
  ]
  n <- nrow(mismatched)
  if (nrow(mismatched)) {
    if (n == 1) {
      msg <- paste0(
        "There is 1 FN121 record that has an existing grid5 that is ",
        "not consistent with its lat-lon."
      )
    } else {
      msg <- paste0(
        sprintf(
          "There are %s FN121 records that have an existing grid5 that is ",
          n
        ),
        "not consistent with their lat-lon."
      )
    }

    cat(msg)
    print("For example:")
    print(utils::head(mismatched[, c(
      "PRJ_CD",
      "SAM",
      "DD_LAT0",
      "DD_LON0",
      "GRID5",
      "grid5_slug"
    )]))
    if (overwrite) {
      if (n == 1) {
        cat("It was updated to the predicted value.\n")
      } else {
        cat("They were updated to their predicted values.\n")
      }
    } else {
      if (n == 1) {
        cat(
          "It was NOT updated. Set overwrite=TRUE to update it to the predicted value.\n"
        )
      } else {
        cat(
          "They were NOT updated. Set overwrite=TRUE to update them to their predicted values.\n"
        )
      }
    }
  }
}
