##' Compare dataframe names to field names in the target table
##'
##' A function to compare the names of the provided dataframe against
##' the names of a specifed table in a target database.  If there are
##' differences in the names, a warning message is issues explaining
##' where differences (extra fields) are found.  No message is issued
##' if no difference are found.  Returns a two element vector
##' containing the list of extra fields in the dataframe, and a list
##' of fields missing from the datafrane,
##' @title Compare dataframe to target table
##' @param trg_db - the path to the accdb file.
##' @param table - the name of the table in the src database to check
##'   against
##' @param src_data - a dataframe containing the data that will be
##'   compared against the field names of 'table'
##' @return vector
##' @export
##' @author R. Adam Cottrill
check_table_names <- function(trg_db, table, src_data) {
  fld_names <- get_field_names(trg_db, table)
  missing <- setdiff(fld_names, names(src_data))
  extra <- setdiff(names(src_data), fld_names)
  if (length(extra)) {
    msg <- sprintf(
      "The source data frame has extra fields: %s",
      paste(extra, collapse = ", ")
    )
    warning(msg)
  }
  if (length(missing)) {
    msg <- sprintf(
      "The source data frame is missing fields: %s",
      paste(missing, collapse = ", ")
    )
    warning(msg)
  }
  return(c(extra, missing))
}
