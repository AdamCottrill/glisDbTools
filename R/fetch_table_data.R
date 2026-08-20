sort_by_fn_keys <- function(df) {
  keyfields <- c(
    "PRJ_CD",
    "SAM",
    "SAMA",
    "SSN",
    "PRD",
    "DTP",
    "SPACE",
    "SUBSPACE",
    "MODE",
    "EFF",
    "SPC",
    "GRP",
    "SIZ",
    "FISH",
    "AGEID",
    "LAMID",
    "FISH_TAG_ID",
    "ATYTMO",
    "FOOD"
  )

  shared_fields <- intersect(keyfields, names(df))

  if (length(shared_fields)) {
    df[do.call("order", df[shared_fields]), ]
  }

  return(df)
}

##' Fetch all of the data from the target table.
##'
##' This function fetches all of the data from the specified table in
##' the target database. It simply executes a select * statement and
##' returns the result as a data frame.
##' @title Fetch all data from an accdb table.
##' @param src_db - the path the accdb database
##' @param tablename - the name of the table to extract the data from.
##' @param as.is - passed to RODBC, should returned values be returned
##'   "as-is", or converted to their R-equivalents?
##' @param stringsAsFactors - passed to RODBC, should string values be
##'   returned as character vectors or converted factors?
##' @param na.strings - passed to RODBC - default placeholder or empty
##'   or missing strings.
##' @return dataframe containing all of the data in the specified
##'   table.
##' @export
##' @author R. Adam Cottrill
fetch_table_data <- function(
  src_db,
  tablename,
  as.is = TRUE,
  stringsAsFactors = FALSE,
  na.strings = ""
) {
  check_accdb(src_db)
  sql <- sprintf("select * from [%s];", tablename)
  conn <- RODBC::odbcConnectAccess2007(
    src_db,
    uid = "",
    pwd = "",
    case = "nochange"
  )
  dat <- RODBC::sqlQuery(
    conn,
    sql,
    as.is = as.is,
    stringsAsFactors = stringsAsFactors,
    na.strings = na.strings
  )
  RODBC::odbcClose(conn)

  dat <- sort_by_fn_keys(dat)
  dat <- prep_date_time_fields(dat)
  return(dat)
}
