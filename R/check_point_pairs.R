##' Check Point Pairs for missing or incomplete coordinates
##'
##' A helper function to remove any missing/invalid net-set pairs
##' before they are passed to the mapping function.  If there is any
##' issue with a pair, a warning is issued and offending pairs removed
##' from the returned data-fame.
##'
##' @title Check Point Pairs
##' @param ptsA dataframe containing the fields SLUG, DD_LAT0 and
##'   DD_LON0
##' @param ptsB dataframe containing the fields SLUG, DD_LAT1 and
##'   DD_LON1
##' @return dataframe
##' @author R. Adam Cottrill
check_point_pairs <- function(ptsA, ptsB) {
  # compare the slugs in ptsA and Pts B (which have both already been
  # scrubbed) and verify that each point in A has a matching point in
  # B. Issue a warning if there are any unmatched pairs.
  pts <- merge(ptsA, ptsB, by = "SLUG")

  problems <- pts$SLUG[
    is.na(pts$DD_LAT0) |
      is.na(pts$DD_LON0) |
      is.na(pts$DD_LAT1) |
      is.na(pts$DD_LON1)
  ]

  if (length(problems) > 0) {
    msg <- "There was a problem with the following point pairs:\n"
    warning(msg, print_and_capture(pts[(pts$SLUG %in% problems), ]))
    pts <- pts[!(pts$SLUG %in% problems), ]
  }

  return(pts)
}
