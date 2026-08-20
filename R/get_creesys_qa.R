##' Fetch questions data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' question data in a format that matches the question table in the upload
##' template.
##' @title Fetch Creel question data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the question data for the specified
##'   creel
##' @export
##' @author R. Adam Cottrill
get_creesys_questions <- function(prj_cd, src_db) {
  sql <- "SELECT PRJ_CD, ANG_OP AS QUESTION_NUMBER, ANG_QUES AS QUESTION_TEXT
     FROM OPTIONQ
     WHERE PRJ_CD='%s';"

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(src_db, stmt)
  return(dat)
}


##' Fetch answers data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' answer data in a format that matches the answer table in the upload
##' template.
##' @title Fetch Creel answer data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the answer data for the specified
##'   creel
##' @export
##' @author R. Adam Cottrill
get_creesys_answers <- function(prj_cd, src_db) {
  # a function replace the Get_FN126 query from the mapper database.

  sql <- "SELECT PRJ_CD, ANG_OP AS QUESTION_NUMBER, ANG_AN AS ANSWER_NUMBER, ANG_ANS AS ANSWER_TEXT
     FROM OPTIONA
     WHERE PRJ_CD='%s';
     "
  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(src_db, stmt)
  return(dat)
}
