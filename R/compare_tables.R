##' Compare data in same table in different databases.
##'
##' This function compares the data contained in the same table from
##' two different databases.  It uses the R-package 'waldo' to print a
##' report of where the differences occur.  If no differences are
##' found it reports "No Differences"
##' @title Compare Database Tables
##'
##' @param dbX - path to the first accdb file.
##' @param dbY - path to the second accdb file.
##' @param tablename - the name of the table to extract the data from
##'   in each table.
##' @param x_label - option label for dbX
##' @param y_label  - option label for dbY
##' @return NULL
##' @export
##' @author R. Adam Cottrill
compare_tables <- function(
  dbX,
  dbY,
  tablename,
  x_label = "glis",
  y_label = "old_master"
) {
  check_accdb(dbX)
  check_accdb(dbY)

  # check table names - if the table isn't in our data base we need to
  # stop and let the user know:
  tablesx <- get_tablenames(dbX)
  if (!(tablename %in% tablesx)) {
    msg <- sprintf(
      "Table '%s' does not appear in the first database '%s'./n",
      tablename,
      dbX
    )
    stop(msg)
  }
  tablesy <- get_tablenames(dbY)
  if (!(tablename %in% tablesy)) {
    msg <- sprintf(
      "Table '%s' does not appear in the second database '%s'./n",
      tablename,
      dbY
    )
    stop(msg)
  }

  dataX <- fetch_table_data(dbX, tablename)
  dataY <- fetch_table_data(dbY, tablename)

  # make sure the data frames are ordered the same, all columns, left to right:
  dataX <- dataX[do.call(order, as.list(dataX)), ]
  dataY <- dataY[do.call(order, as.list(dataY)), ]

  dataX <- prep_date_time_fields(dataX)
  dataY <- prep_date_time_fields(dataY)

  # remove rownames so irrelevant diffrences are not flagged
  row.names(dataX) <- NULL
  row.names(dataY) <- NULL

  waldo::compare(dataX, dataY, x_arg = x_label, y_arg = y_label)
}
