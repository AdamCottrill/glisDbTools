##' Two pannel plot of Condition Factor
##'
##' This funciton will produce a two-panel plot showing the
##' distrubtion of condition factor values reported in a template
##' database.  The panel on the left will contain a density plot of
##' the observed condition factors as well as the upper and lower
##' warning and error values.  Observations outside of those bounds
##' will be highlighted.  The left plot will show weight vs length,
##' including curves that represent the bounds of the the upper and
##' lower warning and error values.
##' @title Two pannel plot of Condition Factor
##' @param fish - dataframe containing basic FN125 data (FLEN,TLEN,
##'   RWT, K)
##' @param species_attributes - single row dataframe created by
##'   subsettting FN012 for the given species and grp.
##' @param what The type of condiition factor to plot - must be either
##'   "Kflen" for Fulton's K calculated using Fork Length or , or
##'   "Ktlen" Fulton's K calculated using Total Length.
##' @return - plot
##' @export
##' @author R. Adam Cottrill
condition_plots <- function(
  fish,
  species_attributes,
  what = c("Kflen", "Ktlen")
) {
  what <- match.arg(what)
  what <- toupper(gsub("K", "", what))

  var_idx <- which(names(fish) == what)

  fish$attr[
    is.na(fish$attr) &
      !is.na(fish$K) &
      fish$K < species_attributes$K_MAX_WARN &
      fish$K > species_attributes$K_MIN_WARN
  ] <- "ok"

  fish$attr[
    is.na(fish$attr) &
      !is.na(fish$K) &
      fish$K >= species_attributes$K_MAX_WARN &
      fish$K < species_attributes$K_MAX_ERROR
  ] <- "too_big"

  fish$attr[
    is.na(fish$attr) &
      !is.na(fish$K) &
      fish$K >= species_attributes$K_MAX_ERROR
  ] <- "way_too_big"

  fish$attr[
    is.na(fish$attr) &
      !is.na(fish$K) &
      fish$K <= species_attributes$K_MIN_WARN &
      fish$K > species_attributes$K_MIN_ERROR
  ] <- "too_small"

  fish$attr[
    is.na(fish$attr) &
      !is.na(fish$K) &
      fish$K <= species_attributes$K_MIN_ERROR
  ] <- "way_too_small"

  graphics::split.screen(c(1, 2))
  graphics::screen(1) #

  with(fish, plot(stats::density(K, na.rm = TRUE), main = "Condition Factor"))

  # density of observed K

  graphics::rug(fish$K[fish$attr == "reference"], col = "grey")

  graphics::rug(fish$K[fish$attr == "ok"], col = "black")
  graphics::rug(fish$K[fish$attr == "too_big"], col = "orange")
  graphics::rug(fish$K[fish$attr == "way_too_big"], col = "red")
  graphics::rug(fish$K[fish$attr == "too_small"], col = "steelblue")
  graphics::rug(fish$K[fish$attr == "way_too_small"], col = "navy")

  graphics::abline(v = species_attributes$K_MAX_ERROR, col = "red")
  graphics::abline(v = species_attributes$K_MIN_ERROR, col = "navy")

  graphics::abline(v = species_attributes$K_MAX_WARN, col = "orange", lty = 3)
  graphics::abline(
    v = species_attributes$K_MIN_WARN,
    col = "steelblue",
    lty = 3
  )

  graphics::screen(2)

  fish <- fish[!is.na(fish$RWT) & !is.na(fish[, var_idx]), ]

  # Length vs weight
  main_label <- sprintf("RWT vs %s", what)
  xlabel <- sprintf("%s (mm)", what)
  plot(
    y = fish$RWT,
    x = fish[, var_idx],
    xlab = xlabel,
    ylab = "RWT (g)",
    type = "n",
    main = main_label
  )

  # a factory function to return a curve with the given condition parameter:
  kcurve <- function(condition) {
    fct <- function(x) (condition * x^3) / 100000
    return(fct)
  }

  kmin_err <- kcurve(species_attributes$K_MIN_ERROR)
  graphics::curve(kmin_err, add = TRUE, col = "navy")

  kmin_warn <- kcurve(species_attributes$K_MIN_WARN)
  graphics::curve(kmin_warn, add = TRUE, col = "steelblue", lty = 3)

  kmax_err <- kcurve(species_attributes$K_MAX_ERROR)
  graphics::curve(kmax_err, add = TRUE, col = "red")

  kmax_warn <- kcurve(species_attributes$K_MAX_WARN)
  graphics::curve(kmax_warn, add = TRUE, col = "orange", lty = 3)

  graphics::points(
    fish[fish$attr == "reference", var_idx],
    fish$RWT[fish$attr == "reference"],
    col = "grey"
  )
  graphics::points(
    fish[fish$attr == "ok", var_idx],
    fish$RWT[fish$attr == "ok"],
    col = "black"
  )
  graphics::points(
    fish[fish$attr == "too_big", var_idx],
    fish$RWT[fish$attr == "too_big"],
    col = "orange",
    pch = 19
  )
  graphics::points(
    fish[fish$attr == "way_too_big", var_idx],
    fish$RWT[fish$attr == "way_too_big"],
    col = "red",
    pch = 8
  )
  graphics::points(
    fish[fish$attr == "too_small", var_idx],
    fish$RWT[fish$attr == "too_small"],
    col = "steelblue",
    pch = 19
  )
  graphics::points(
    fish[fish$attr == "way_too_small", var_idx],
    fish$RWT[fish$attr == "way_too_small"],
    col = "navy",
    pch = 8
  )

  graphics::close.screen(all = TRUE)
}
