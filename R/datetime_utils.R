
##' Extract time from datetime object
##'
##' MS Access and/or odbc driver appends a date of 1899-12-30 to any
##' time that does not have a date component.  Plain time fields are
##' relatively common in the FN-2 data model.  This function takes a
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
get_time <- function(datetime) {
  # extract time from date/time string

  time_regex <- ".*([0-2][0-9]:[0-5][0-9]:[0-5][0-9])$"
  datetime <- ifelse(is.na(datetime), datetime, gsub(time_regex, "\\1", datetime, perl = TRUE))

  msg <- paste0(
    "One or more values is not a valid time or datetime. ",
    "The time component should be in the format HH:MM:SS."
  )

  ifelse(!grepl(time_regex, datetime[!is.na(datetime)]),
    stop(msg),
    datetime[!is.na(datetime)]
  )
  return(datetime)
}


get_date <- function(timestamp) {
    date_string <- gsub(" 00:00:00$", "", timestamp)
    return(date_string)
  }


get_date_fields <- function(){
  date_fields <- c(
    "DATE",
    "PRJ_DATE0", "PRJ_DATE1",
    "SSN_DATE0", "SSN_DATE1",
    "EFFDT0", "EFFDT1"
  )
  return(date_fields)
}


get_time_fields <- function(){
  time_fields <- c(
    "PRDTM0", "PRDTM1",
    "SAMTM0",
    "ATYTM0",
    "ATYTM1",
    "ITVTM0",
    "EFFTM0_GE", "EFFTM0_LT",
    "EFFTM0", "EFFTM1")
  return(time_fields)
}


prep_date_time_fields <- function(payload){

  time_fields <- get_time_fields()
  date_fields <- get_date_fields()

  shared_time_fields <- names(payload) %in% time_fields
  shared_date_fields <- names(payload) %in% date_fields

  payload[shared_time_fields] <- lapply(payload[shared_time_fields], get_time)
  payload[shared_date_fields] <- lapply(payload[shared_date_fields], get_date)

  return(payload)
}
