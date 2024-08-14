##' Add a meta data record to the target database
##'
##' This function connects to the target database and creates a README
##' table with a single record that has the source of the data and the
##' date that it was populated.
##' @title Populate README with source and timestamp
##' @param trg_db - the path to the populated template database.
##' @param src - the name of the source databae (e.g., "creesys", "nearshore master")
##' @return NULL
##' @author R. Adam Cottrill
populate_readme <- function(trg_db, src) {

  readme_msg <- sprintf("Template populated from %s on %s", src, Sys.time())
  README <- data.frame("README" = readme_msg)
  conn <- RODBC::odbcConnectAccess2007(trg_db, uid = "", pwd = "")
  RODBC::sqlSave(conn, README, rownames = F, append = FALSE)
  RODBC::odbcClose(conn)


}

##' Check for target and template databases.
##'
##' A helper funciton used by the data mappers to verify that the
##' target and template dateabase exists in the expected locations.
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


##' Verify that the src_db is an accdb file that exists
##'
##' A function that accepts a path to an accdb file and verifies that
##' it has an accdb extension and that it actually exists.  An error
##' message is reported if either of these condistion is false.
##' @title Verify *.accdb file
##' @param src_db path to an accdb file - it must end with accdb and
##'   actually exist on the filesystem.
##' @return TRUE if the file exists and has an accdb extension.
##' @author R. Adam Cottrill
check_accdb <- function(src_db) {
  if (!grepl("\\.accdb$",src_db)) {
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

  if (!file.exists(src_db)) {
    message <-
      sprintf(
        paste0(
          "Could not find the database '%s'. ",
          "Make sure it exists and try again"
        ),
        src_db
      )
    stop(message)
  }

  return(TRUE)

}




##' Execute a select sql statement and return the results
##'
##' This funciton is a wrapper around the RODBC funct sqlQuery with
##' default arguments that will prevent R from converting strings to
##' factors and/or dropping leading zeros. The option toupper ensures
##' the names of the returned dataframe are in upper case which
##' matches FN-2 naming convension.
##' @title Fetch data from src database.
##' @param sql - the string to be exectuted.
##' @param src_db - a string representing the path to the src
##'   database.
##' @param toupper - Boolean. default=TRUE, should names of returned
##'   dataframe be converted to uppercase before returning?
##' @return A dataframe containing the data returned by the sql
##'   statement.
##' @author R. Adam Cottrill
fetch_sql <- function(sql, src_db, toupper=T){
  check_accdb(src_db)
  DBConnection <- RODBC::odbcConnectAccess2007(src_db,uid = "", pwd = "")
  dat <- RODBC::sqlQuery(DBConnection, sql, as.is=TRUE, stringsAsFactors=FALSE, na.strings = "")
  RODBC::odbcClose(DBConnection)
  if (toupper)names(dat) <- toupper(names(dat))
  return(dat)
}

##' Validate a project code
##'
##' A little helper functin that verifies that the provided project
##' code conforms to fn-2 standards.  Essentially, three numbers or
##' letters, followed by an underscore, two letters, followed by two
##' numbers, followed by another underscore, followed by three
##' alph-numeric characters.  Strings that conform to this pattern
##' will be considered valid project codes and the function will
##' return TRUE, otherwise it will return FALSE.
##' @title Validate project codes
##' @param prj_cd - a string representing a project code.
##' @return - boolean indicating whether or not the string is a valid
##'   project code.
##' @author R. Adam Cottrill
valid_prj_cd <- function(prj_cd) {

  if (grepl(", ", prj_cd)) {
    prj_cds <- gsub("'", "", strsplit(prj_cd, ", ")[[1]])
    for (item in prj_cds) {
      return(valid_prj_cd(item))
    }
  }
# update the regex if you are using this after 2030!
  prj_cd_regex <- "^'?[A-Z0-9]{3}_[A-Z]{2}\\d{2}_[A-Z0-9]{3}'?$"
  return(grepl(prj_cd_regex, prj_cd))

}



##' validate prj_cd and format sql statment
##'
##' This function verifies that the provided prj_cd is a string that
##' conforms to standr project code conventions and returns the
##' provided sql string with the project code inserted in the
##' specified location.
##' @title validate prj_cd and format sql statment
##' @param sql - a sql statement that will be formatted with prj_cd.
##'   sprintf() is used to make the substition, so %s should be used
##'   as the project code placeholder.
##' @param prj_cd - a valid FN-2 project code.
##' @return a string representing the provided sql statement
##'   containing prj_cd in place of the %s placeholder(s).
##' @author R. Adam Cottrill
format_prj_cd_sql <- function(sql, prj_cd) {

  if (valid_prj_cd(prj_cd)==FALSE) {

    msg <- sprintf("the provided prj_cd (%s) does not appear to be a valid prj_cd!",
      prj_cd)
    stop(msg)
  }
  stmt <- sprintf(sql, prj_cd)
  return(stmt)
}


# Append data to the specified table in the target database
##'
##' This is a helper function that compares the names of the provided
##' dataframe with the field names in the target database and stops if
##' the names don't match.  If the names don't match a report is
##' provided explaining where the differences were found.
##'
##' Optional arguments can be used to skip the field name check, force
##' insertion, or print the verbose output from RODBC::sqlSave()
##' function.
##'
##' @title Append data in a dataframe to target table
##' @param dbase - full path to an ms access file.
##' @param trg_table - the name of table in the target data base to
##'   append to.
##' @param data - the data frame to append to the target table. The
##'   names in this dataframe must match the column names in trg_table
##'   if check_names=TRUE.
##' @param append - passed to sqlSave()- should the data be appened to
##'   (TRUE) or overwrite (FALSE) an existing table
##' @param safer - passed to sqlSave() - only appends are allowed if
##'   safer=TRUE
##' @param toupper - should the names of tables be converted to
##'   uppercase before being checked?  Ensures that check_names is
##'   case-insentive.
##' @param check_names - boolean - should the names of the target
##'   table be compared to the the names of the provided dataframe
##'   before attempting to insert the rows in the database?
##' @param verbose - passed to sqlSave() - should the sqlSave()
##'   function produce verbose output? Very useful in debugging.
##' @return status of the odbc connection.
##' @export
##' @author R. Adam Cottrill
append_data <- function(dbase, trg_table, data, append=T, safer=T,
                        toupper=T, check_names=T,
                        verbose=F){
  if (toupper)names(data) <- toupper(names(data))

  check_accdb(dbase)

  field_check <- check_table_names(dbase, trg_table, data)
  if(length(field_check)) stop("Please fix field differences before proceeding.")

  conn <- RODBC::odbcConnectAccess2007(dbase, uid = "", pwd = "")
  RODBC::sqlSave(conn, data, tablename = trg_table, rownames = F,
          safer = safer, append = append, nastring = NULL, verbose = verbose)
  return(RODBC::odbcClose(conn))

}


##' List field names in target table
##'
##' This function connects to a target table in the provided database
##' and returns a dataframe containing all of the field names in the
##' target table of the provided database.
##' @title List fields  in table
##' @param trg_db - the absolute or relative path to the target
##'   database (accdb file)
##' @param table - the name of the table to query.
##' @return dataframe
##' @author R. Adam Cottrill
get_trg_table_names <- function(trg_db, table){
  check_accdb(trg_db)
  DBConnection <- RODBC::odbcConnectAccess2007(trg_db, uid = "", pwd = "")
  stmt <- sprintf("select * from [%s] where FALSE;", table)
  dat <- RODBC::sqlQuery(DBConnection, stmt, as.is=TRUE, stringsAsFactors=FALSE, na.strings = "")
  RODBC::odbcClose(DBConnection)
  return(toupper(names(dat)))
}


##' Compare dataframe names to target table
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
##' @author R. Adam Cottrill
check_table_names <- function(trg_db, table, src_data){
  trg_names <- get_trg_table_names(trg_db, table)
  missing <- setdiff(trg_names, names(src_data))
  extra <- setdiff(names(src_data), trg_names)
  if(length(extra)) {
    msg <- sprintf("The source data frame has extra fields: %s",
      paste(extra,collapse = ', '))
      warning(msg)
    }
  if(length(missing)) {
    msg <- sprintf("The source data frame is missing fields: %s",
      paste(missing, collapse = ', '))
      warning(msg)
  }
  return(c(extra, missing))
}



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



##' Get distinct project codes in the source database
##'
##' Returns the unique project codes in the source database.  This
##' list can be used to compare against project codes in Project
##' Tracker, creel Portal, or the assessment portal to find projects
##' that are missing in one or the other.
##' @title Get unique Project Codes from Source DB.
##' @param src_db - complete path to an accdb file.
##' @param src_table - option table name to query PRJ_CD
##'   from. Defaults to 'FN011', but any table name from the source
##'   data can be used.
##' @return - a dataframe containing all of the PRJ_CD values in the
##'   provided table.
##' @export
##' @author R. Adam Cottrill
get_src_prj_cds <- function(src_db, src_table="FN011"){

  check_accdb(src_db)

  stmt <- sprintf("select distinct [PRJ_CD] from [%s] order by [PRJ_CD];", src_table)

  DBConnection <- RODBC::odbcConnectAccess2007(src_db, uid = "", pwd = "")
  dat <- RODBC::sqlQuery(DBConnection, stmt)
  RODBC::odbcClose(DBConnection)
  return(dat)
}


##' Add mode to FN121 based on GR, GRUSE and ORIENT
##'
##' This function is used by the nearshore and offshore mapping
##' funcitons to populate the correct MODE value based on the records
##' in the FN028 table and the values of GEAR, GEAR_USE and ORIENT
##' specified in each FN121 record.
##' @title Add Mode to FN121
##' @param fn121 - dataframe representing sampling events (net sets)
##' @param fn028 - dataframe representing available modes (set methods)
##' @return fn121 dataframe with populated mode field added
##' @author R. Adam Cottrill
add_mode <- function(fn121, fn028) {
  # populate the correct mode for each sam:
  x121 <- subset(fn121, select = c("PRJ_CD", "SAM", "GR", "GRUSE", "ORIENT"))
  x028 <- subset(fn028, select = c("PRJ_CD", "GR", "GRUSE", "ORIENT", "MODE"))
  tmp <- merge(x121, x028, by = c("PRJ_CD", "GR", "GRUSE", "ORIENT"))
  foo <- merge(fn121, tmp, by = c(
    "PRJ_CD", "SAM", "GR", "GRUSE",
    "ORIENT"
  ), all.x=TRUE)
  return (foo[with(foo, order(PRJ_CD, SAM)),])
}





##' Set waterhaul to True if there are no FN123 records.
##'
##' This function set FN122.WATERHAUL values to TRUE or 0 based on
##' values in the FN123 table.  If there are not fn123 records
##' associated with an Fn122 record, waterhaul is set to TRUE.
##' @title Set waterhaul
##' @param dbase - path the populated tempalte database.
##' @return odbc connection status ('0' (success) or '1')
##' @author R. Adam Cottrill
update_FN122_waterhaul <- function(dbase) {
  sql <- "UPDATE FN122 LEFT JOIN FN123
ON (FN122.EFF = FN123.EFF)
AND(FN122.SAM = FN123.SAM)
AND(FN122.PRJ_CD = FN123.PRJ_CD)
SET FN122.WATERHAUL = 'True'
WHERE (((FN123.PRJ_CD) Is Null));"
  check_accdb(dbase)
  conn <- RODBC::odbcConnectAccess2007(dbase, uid = "", pwd = "")
  RODBC::sqlQuery(conn, sql)
  return(RODBC::odbcClose(conn))
}
