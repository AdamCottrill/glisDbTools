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
##' @export
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
