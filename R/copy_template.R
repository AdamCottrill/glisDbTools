##' Copy and Rename a Template Database
##'
##' A convience function that can be used to copy and rename a
##' populated template database.  An error will be thrown if the
##' filenames do not end with accdb, or the original db does not
##' exists.  If overwrite is TRUE, the target data base will the
##' overwritten if it exists.
##' @title Copy and Rename a Template Database
##' @param old_name - character string representing the path to the
##'   original database. It must exist, and must end in '*.accdb'.
##' @param new_name - character string representing the path to the
##'   new database. The path must end in '*.accdb'.
##' @param overwrite - boolean - if the new_name already exists,
##'   should it be overwritten? Defaults to FALSE.
##' @return NULL
##' @author R. Adam Cottrill
copy_template <- function(old_name, new_name, overwrite = FALSE) {
  check_accdb(old_name)
  check_accdb(new_name, FALSE)

  if (file.exists(new_name) && !overwrite) {
    message_a <- sprintf("The trg_db database: '%s' already exists.", new_name)
    message_b <- "Please provide a different file name or set overwrite=TRUE."
    stop(paste(message_a, message_b, sep = "\n"))
  } else {
    file.copy(old_name, new_name, overwrite = overwrite)
  }
}
