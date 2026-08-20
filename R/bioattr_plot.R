##' Plot Density of FN125 data against FN012 constraints
##'
##' This function will create plot a density plot for the specified
##' attribute reported in a template database and superimpose those
##' values against the constrains in the FN012 table. Individual
##' observations are presented as a rug plot while the FN012 limits
##' are presented as vertical lines.  Values outside of these limits
##' are highlighted.  The function takes an optional argument
##' (glis_fish) that can be used to provide additional context to
##' evaluate the data in the in the current project against - this is
##' particularly helpful for species that are often encountered in
##' small number in a single project, but may exists in sufficient
##' numbers in other sources.
##'
##' @title Plot FN125 data against FN012 constraints##'
##' @param fish - dataframe containing basic FN125 data (FLEN,TLEN,
##'   RWT)
##' @param species_attributes - single row dataframe created by
##'   subsettting FN012 for the given species and grp.
##' @param what The type of condiition factor to plot - must be one of
##'   FLEN, TLEN or RWT
##' @export
##' @return plot
##' @author R. Adam Cottrill
bioattr_plot <- function(
    fish,
    species_attributes,
    what = c("FLEN", "TLEN", "RWT")) {
  what <- match.arg(what)

  # get the column numbers for data we want to plot depending on
  # what has been selected
  var_idx <- which(names(fish) == what)
  min_idx <- which(names(species_attributes) == sprintf("%s_MIN", what))
  max_idx <- which(names(species_attributes) == sprintf("%s_MAX", what))

  fish$attr[
    is.na(fish$attr) &
      !is.na(fish[, var_idx]) &
      fish[, var_idx] <= species_attributes[, max_idx] &
      fish[, var_idx] >= species_attributes[, min_idx]
  ] <- "ok"

  fish$attr[
    is.na(fish$attr) &
      !is.na(fish[, var_idx]) &
      fish[, var_idx] > species_attributes[, max_idx]
  ] <- "too_big"

  fish$attr[
    is.na(fish$attr) &
      !is.na(fish[, var_idx]) &
      fish[, var_idx] < species_attributes[, min_idx]
  ] <- "too_small"

  plot(stats::density(fish[, var_idx], na.rm = TRUE), main = what)

  graphics::rug(fish[fish$attr == "reference", var_idx], col = "grey")
  graphics::rug(fish[fish$attr == "ok", var_idx], col = "black")
  graphics::rug(fish[fish$attr == "too_big", var_idx], col = "red")
  graphics::rug(fish[fish$attr == "too_small", var_idx], col = "red")

  graphics::abline(v = species_attributes[1, min_idx], col = "red", lty = 3)
  graphics::abline(v = species_attributes[1, max_idx], col = "red", lty = 3)
}
