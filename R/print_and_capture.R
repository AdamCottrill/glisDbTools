##' Capture information printed to the console
##'
##' A simple little helper to capture information printed by the
##' console and return it as a string.
##'
##' @title Print and Capture
##' @param x - the R object to print.
##'
##' @return string
##' @author R. Adam Cottrill
print_and_capture <- function(x) {
  paste(utils::capture.output(print(x)), collapse = "\n")
}
