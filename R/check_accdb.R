##' Verify that the src_db is an accdb file that exists
##'
##' A function that accepts a path to an accdb file and verifies that
##' it has an accdb extension and that it actually exists.  An error
##' message is reported if either of these condistion is false.
##' @title Verify *.accdb file
##' @param src_db path to an accdb file - it must end with accdb and
##'   actually exist on the filesystem.
##' @param exists - check if the file actually exists. If this value
##'   is true and the file does not exist, an error will be thrown.
##' @return TRUE if the file exists and has an accdb extension.
##' @export
##' @author R. Adam Cottrill
check_accdb <- function(src_db, exists = TRUE) {
  if (!grepl("\\.accdb$", src_db)) {
    message <-
      sprintf(
        paste0(
          "The provided filename '%s' ",
          "does not appear to be an MS access (*.accdb) file."
        ),
        src_db
      )
    stop(message)
  }
  # if exists is TRUE, verify that the files accually exists too:
  if (exists && !file.exists(src_db)) {
    message <-
      sprintf(
        paste0(
          "Could not find the database '%s'. ",
          "Make sure it exists and try again."
        ),
        src_db
      )
    stop(message)
  }

  return(TRUE)
}
