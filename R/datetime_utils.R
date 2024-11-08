##' Known Date Fields in Upload template
##'
##' This function returns a character vector containing the list of
##' fields that are know to contain just date values.
##'
##' @title Known Time Fields
##' @return character vector.
##' @author R. Adam Cottrill
get_date_fields <- function() {
  date_fields <- c(
    "DATE",
    "PRJ_DATE0",
    "PRJ_DATE1",
    "SSN_DATE0",
    "SSN_DATE1",
    "EFFDT0",
    "EFFDT1"
  )
  return(date_fields)
}



##' Known Time Fields in Upload template
##'
##' This function returns a character vector containing the list of
##' fields that are know to contain just time values.
##'
##' @title Known Time Fields
##' @return character vector.
##' @author R. Adam Cottrill
get_time_fields <- function() {
  time_fields <- c(
    "PRDTM0",
    "PRDTM1",
    "SAMTM0",
    "ATYTM0",
    "ATYTM1",
    "ITVTM0",
    "EFFTM0_GE",
    "EFFTM0_LT",
    "EFFTM0",
    "EFFTM1"
  )
  return(time_fields)
}


##' Extract time from datetime object
##'
##' MS Access and/or odbc driver appends a date of 1899-12-30 to any
##' time that does not have a date component.  Plain time fields are
##' relatively common in the FN-2 data model.  This function takes a
##' datetime in R, removed the date component, and returns just the
##' time.
##'
##' @title Get time from datetime object.
##' @param timestamp - string representing a data time object. It must
##'   contain time formatted as "HH:MM:SS"
##' @return datatime object with date component stripped off.
##' @author R. Adam Cottrill
##' @export
##' @examples
##'
##' my_date <- "2024-08-12 09:49:15"
##' my_time <- get_time(my_date)
##'
##' #get_time(Sys.time())
##'
get_time <- function(timestamp) {
  # extract time from date/time string

  time_regex <- ".*([0-2][0-9]:[0-5][0-9]:[0-5][0-9])$"
  timestamp <- ifelse(is.na(timestamp), timestamp, gsub(time_regex, "\\1", timestamp, perl = TRUE))

  msg <- paste0(
    "One or more values is not a valid time or timestamp. ",
    "The time component should be in the format HH:MM:SS."
  )

  ifelse(!grepl(time_regex, timestamp[!is.na(timestamp)]),
    stop(msg),
    timestamp[!is.na(timestamp)]
  )
  return(timestamp)
}


##' Extract date from datetime object
##'
##' MS Access and/or odbc driver appends a defaual time of 00:00:00 to any
##' date that does not have a time component.  Plain date fields are
##' relatively common in the FN-2 data model.  This function takes a
##' datetime in R, removed the time component, and returns just the
##' date.
##' @title Get date from datetime object.
##' @param timestamp - string representing a datetime object. It must
##'   contain a default time formatted as "00:00:00"
##' @return datatime object with time component stripped off.
##' @author R. Adam Cottrill
##' @export
##' @examples
##'
##' my_date <- "2024-08-12 00:00:00"
##' my_date <- get_date(my_date)
##'
##' get_date(Sys.time())
##'
get_date <- function(timestamp) {
  date_string <- gsub(" 00:00:00$", "", timestamp)
  return(date_string)
}



##' Prepare Date and Time Fields for MS Access
##'
##' This funciton is used to prepare data for insertion into MS Access
##' - There are several fields in the template database that are
##' eihter dates or times (rather than datetime).  These fields often
##' have default values in their 'un-used' part ("00:00:00" for the
##' time or the current date for dates).  When these components are
##' sent to MS access, they will often raise a parse or conversion
##' error.  This function strips off the extra time or date if the
##' field name matches of the known time or date fields before
##' returning the same dataframe.
##'
##' @title Prepare Date and Time Fields
##' @param payload - a dataframe
##' @return dataframe
##' @seealso get_date_fields, get_time_fields
##' @author R. Adam Cottrill
prep_date_time_fields <- function(payload) {
  time_fields <- get_time_fields()
  date_fields <- get_date_fields()

  shared_time_fields <- names(payload) %in% time_fields
  shared_date_fields <- names(payload) %in% date_fields

  payload[shared_time_fields] <- lapply(payload[shared_time_fields], get_time)
  payload[shared_date_fields] <- lapply(payload[shared_date_fields], get_date)

  return(payload)
}
