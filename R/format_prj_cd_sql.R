##' validate prj_cd and format sql statment
##'
##' This function verifies that the provided prj_cd is a string that
##' conforms to standard project code conventions and returns the
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
  if (valid_prj_cd(prj_cd) == FALSE) {
    msg <- sprintf(
      "the provided prj_cd (%s) does not appear to be a valid prj_cd!",
      prj_cd
    )
    stop(msg)
  }
  stmt <- sprintf(sql, prj_cd)
  return(stmt)
}
