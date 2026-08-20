##' Check for target and template databases.
##'
##' A helper funciton used by the data mappers to verify that the
##' target and template datebase exists in the expected locations.
##' @title Check for target and template databases.
##' @param trg_db - the path to the target database.
##' @param template_db - the path to the template database.
##' @param overwrite - if the target database already exists, should
##'   it be replaced?
##' @return NULL
##' @author R. Adam Cottrill
check_db_setup <- function(trg_db, template_db, overwrite) {
  if (file.exists(trg_db) && !overwrite) {
    message_a <- sprintf("The trg_db database: '%s' already exists.", trg_db)
    message_b <- "Please provide a different project code or set overwrite=TRUE."
    stop(paste(message_a, message_b, sep = "\n"))
  }

  if (!file.exists(template_db)) {
    message <-
      sprintf(
        paste0(
          "Could not find the template database '%s'. ",
          "Make sure it exists and try again"
        ),
        template_db
      )
    stop(message)
  } else {
    file.copy(template_db, trg_db, overwrite = overwrite)
  }
}
