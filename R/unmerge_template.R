##' Unmerge Template Databases
##'
##' This function is the compliment to merge_template and will remove
##' all of the data from one template based on the project codes found
##' in another. The most common use case for this function is backing
##' out changes that were created by merge_templates when an error
##' occurs.
##'
##' @title Delete data from one database based on another
##' @param dbX - path to the target database that will be updated
##' @param dbY - path the source database with the project codes that
##'   will be removed from dbX.  It must have an FN011 table with
##'   field PRJ_CD.
##' @param prompt - Should the use be ask to confirm before deleting
##'   the data? Defaults to TRUE.
##' @return NULL
##'
##' @export
##'
##' @author R. Adam Cottrill
unmerge_templates <- function(dbX, dbY, prompt = TRUE) {
  # y will be revoved from X based on project code
  check_accdb(dbX)
  check_accdb(dbY)

  # check table names - if the table isn't in our data base we need to
  # stop and let the user know:
  tablenames <- get_tablenames(dbX)

  # get the list of project codes from our source db:
  sql <- "select distinct [PRJ_CD] from [FN011]"
  prj_cds <- fetch_sql(dbY, sql)

  if (prompt) {
    project_codes <- paste0(
      sapply(prj_cds, function(x) sprintf("\t+ %s\n", x)),
      collapse = ""
    )
    msg <- sprintf(
      "Are you sure you want to delete all of the data for these projects?\n%s",
      project_codes
    )
    confirm <- utils::askYesNo(msg)
  } else {
    confirm <- TRUE
  }

  if (!confirm) {
    return()
  }

  msg <- sprintf(
    "Data from the following projects will be removed from '%s':\n",
    dbX
  )
  cat(msg)
  for (prj_cd in prj_cds) {
    cat(sprintf("\t%s\n", prj_cd))
  }

  # clear out the old data
  cat("Clearing data from:\n")
  for (i in length(tablenames):1) {
    table <- tablenames[i]
    payload <- clear_table_data(dbX, table, prj_cds)
    cat(sprintf("\t%s\n", table))
  }

  cat(sprintf(
    "Done. All data from '%s' has been revoved from '%s'\n",
    dbY,
    dbX
  ))
}
