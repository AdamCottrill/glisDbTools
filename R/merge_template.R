##' Merge Template Databases
##'
##' This function will merge one template databaes into another.  This
##' function simply attempts to append all of the data in the source
##' database with data in the target. No attemp is made by this
##' function to satisify contstraints imposed at the database level.
##' If erros are encountered, the function unmerge_templates() can be
##' used to removed data associated with the source db.
##'
##' @title Merge data from one template database based into another
##' @param dbX - path to the target database that will be updated
##' @param dbY - path the source database with data that will be inserted into the target.##'
##' @return NULL
##'
##' @export
##'
##' @author R. Adam Cottrill
merge_templates <- function(dbX, dbY) {
  # y will be inserted into X
  check_accdb(dbX)
  check_accdb(dbY)

  # check table names - if the table isn't in our data base we need to
  # stop and let the user know:
  tablesx <- get_tablenames(dbX)
  tablesy <- get_tablenames(dbY)

  append <- intersect(tablesx, tablesy)
  insert <- setdiff(tablesx, tablesy)

  # for each table in append, fetch the data from Y and append it to X
  skip <- c("_version")
  append <- append[!(append %in% skip)]
  insert <- insert[!(insert %in% skip)]

  if (length(append)) {
    cat("Appending data from:\n")
    for (table in append) {
      payload <- fetch_table_data(dbY, table)
      cat(sprintf("\t%s: %s\n", table, nrow(payload)))

      if (nrow(payload)) {
        payload <- prep_date_time_fields(payload)
        append_data(dbX, table, payload)
      }
    }
  }

  if (length(insert)) {
    cat("Inserting data from:\n")
    for (table in insert) {
      payload <- fetch_table_data(dbY, table)
      cat(sprintf("\t%s: %s\n", table, nrow(payload)))
      if (nrow(payload)) {
        payload <- prep_date_time_fields(payload)
        append_data(dbX, table, payload, append = FALSE)
      }
    }
  }

  cat(sprintf(
    "Done. All data from '%s' has been inserted into '%s'\n",
    dbY,
    dbX
  ))
}
