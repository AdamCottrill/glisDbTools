##' Extract time from datetime object
##'
##' MS Access and/or odbc driver appends a date of 1899-12-30 to any
##' time that does have a date component.  Plane time fields are
##' relatively common in the FN-2 datamodel.  This function takes a
##' datetime in R, removed the date component, and returns just the
##' time.
##' @title Get time from datetime object.
##' @param datetime - string representing a data time object. It must
##'   contain time formatted as "HH:MM:SS"
##' @return datatime object with date component stripped off.
##' @author R. Adam Cottrill
##' @export
##' @examples
##'
##' my_date <- "2024-08-12 09:49:15"
##' my_time <- get_time(my_date)
##'
##' get_time(Sys.time())
##'
get_time <- function(datetime){
  # extract time from date/time string

  time_regex <- ".*([0-2][0-9]:[0-5][0-9]:[0-5][0-9])$"
  datetime <- ifelse(is.na(datetime), datetime, gsub(time_regex, "\\1", datetime, perl = TRUE))

  msg <- paste0(
    "One or more values is not a valid time or datetime. ",
    "The time component should be in the format HH:MM:SS."
  )

  ifelse(!grepl(time_regex, datetime[!is.na(datetime)]),
         stop(msg),
         datetime[!is.na(datetime)])
  return(datetime)
}
