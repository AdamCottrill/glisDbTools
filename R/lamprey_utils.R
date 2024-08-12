##' Split wide lamijc into long lamprey wound records
##'
##' A function that takes a LAMIJC wound string that encodes an
##' arbirary number of lamprey wounds into individual lamprey wound
##' records.  The string "A125B230" is parsed into c( "A125B", "230"
##' ).  An error is raised if the provided string does not match the
##' IJC lamprey wounding conventions
##' @title Split LAMIJC into wounds
##' @param wound - a string representing an IJC Lamprey would
##'   observation. See data dictionary for more details.
##' @return a dataframe containing one row for each wound encoded in
##'   the LAMIJC string.
##' @author R. Adam Cottrill
split_lamijc <- function(wound) {
  wnd_regex <- "^0$|^([A|B][1-4])+$|^([A|B][1-4][1-5][0-9])+$"
  if (!grepl(wnd_regex, wound)) {
    msg <- sprintf("'%s' is not a valid lamijc.", wound)
    stop(msg)
  }

  if (wound == "0") {
    # retrun a matrix to match other output types:
    return(matrix(c("0", "")))
  }

  a123_regex <- "^([A|B][1-4][1-5][0-9])+$"
  if (grepl(a123_regex, wound)) {
    # wound with diameter
    wounds <- strsplit(wound, "(?<=.{4})", perl = TRUE)[[1]]
  } else {
    wounds <- strsplit(wound, "(?<=.{2})", perl = TRUE)[[1]]
  }

  # split the wounds up in to wound and diameter components:
  wounds <- t(sapply(wounds, function(x) c(substr(x, 1, 2), substr(x, 3, 4))))
  return(wounds)
}


##' Convert Lamprey data to FN125_Lamprey
##'
##' this function takes lamprey wounding from an fn125 table and
##' returns an FN125_lamprey compatible dataframe with each individual
##' lamprey wound represented as a single record.  LamID are
##' automatically incremented by fish in the returned dataframe.
##' @title Convert Lamprey data to FN125_Lamprey
##' @param df - FN125 like dataframe - should contain the key fields
##'   plus XLAM and LAMIJC.
##' @return dataframe with keyfields, plus columns for XLAM,
##'   LAMICJ_TYPE,LAMICJ_TYPE. There will be one row for each lamprey
##'   wound observation (rather than one row per fish)
##' @author R. Adam Cottrill
process_fn125_lamprey <- function(df) {
  tmp <- df[FALSE, ]
  tmp$LAMICJ_TYPE <- character()
  tmp$LAMICJ_SIZE <- character()

  for (i in 1:nrow(df)) {
    fish <- df[i, ]
    if (is.na(df$LAMIJC[i])) {
        fish$LAMIJC_TYPE <- NA
        fish$LAMIJC_SIZE <- NA
        tmp <- rbind(tmp, fish)
    } else {
      wounds <- split_lamijc(df$LAMIJC[i])
      if (wounds[1, 1] == "0") {
        fish$LAMIJC_TYPE <- "0"
        fish$LAMIJC_SIZE <- NA
        tmp <- rbind(tmp, fish)
      } else {
        for (w in 1:nrow(wounds)) {
          fish$LAMID <- w
          fish$LAMIJC_TYPE <- wounds[w, 1]
          fish$LAMIJC_SIZE <- wounds[w, 2]
          tmp <- rbind(tmp, fish)
        }
      }
    }
  }
  tmp$LAMIJC <- NULL
  return(tmp)
}
