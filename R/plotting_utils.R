##' Plot Process Validate FN125 data against FN012 constraints
##'
##' This function will create plots associated with some of the
##' process validate queries that check biological attributes against
##' constrains in the FN012 table.  Currently, plots for fork length,
##' total length, round weight, and condition factor (based on fork or
##' total length) are supported.  In most cases, the generated plot
##' includes a density plot showing the distribution of the observed
##' variable for specified species, and a rug plot illustrating
##' individual observations.  The plot will also include vertical
##' lines that illustrate the associated limits from the FN012 table.
##' Values outside of these limits are highlighted.  The function
##' takes an optional argument (glis_fish) that can be used to provide
##' additional context to evaluate data in the current project against
##' - this is particularly helpful for species that are often
##' encountered in small number in a single project.
##'
##' @title Plot FN125 data against FN012 constraints
##' @param src Path to the populated template database.  realative
##'   paths are supported. An error will be thrown if the database
##'   cannot be found or is not an accdb file.
##' @param spc Three character species code for the species to plot
##' @param grp The group of the spc+grp to select. Defaults to '00'
##' @param what The bioligical attribute to plot - must be on of
##'   "FLEN", "TLEN", "RWT", "Kflen", or "Ktlen".
##' @param glis_fish - an optional dataframe that can be used to
##'   prodive additional biological data for more context.
##' @return a plot
##' @export
##' @author R. Adam Cottrill
procval_plot <- function(src, spc, grp = "00",
                         what = c("FLEN", "TLEN", "RWT", "Kflen", "Ktlen"),
                         glis_fish = NULL) {
  what <- match.arg(what)

  sql012 <- sprintf("SELECT * from FN012 where [spc] = '%s' AND GRP='%s'", spc, grp)
  species_attributes <- fetch_sql(sql012, src)

  sql125 <- sprintf("SELECT [PRJ_CD], [SAM], [EFF], [SPC], [GRP], [FISH], [FLEN], [TLEN], [RWT] from [FN125] where [spc] ='%s' AND GRP='%s'", spc, grp)
  fish <- fetch_sql(sql125, src)
  fish$attr <- NA

  if (!is.null(glis_fish)) {
    glis_fish <- glis_fish[, c("PRJ_CD", "SAM", "EFF", "SPC", "GRP", "FISH", "FLEN", "TLEN", "RWT")]
    glis_fish$attr <- "reference"
    fish <- rbind(fish, glis_fish)
  }

  if (what == "Ktlen") {
    fish$K <- 100000 * fish$RWT / (fish$TLEN^3)
  } else {
    fish$K <- 100000 * fish$RWT / (fish$FLEN^3)
  }

  if (what %in% c("Kflen", "Ktlen")) {
    condition_plots(fish, species_attributes, what)
  } else {
    bioattr_plot(fish, species_attributes, what)
  }
}



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
##' @author R. Adam Cottrill
condition_plots <- function(fish, species_attributes, what = c("Kflen", "Ktlen")) {
  what <- match.arg(what)
  what <- toupper(gsub("K", "", what))

  var_idx <- which(names(fish) == what)

  fish$attr[is.na(fish$attr) &
    !is.na(fish$K) &
    fish$K < species_attributes$K_MAX_WARN &
    fish$K > species_attributes$K_MIN_WARN] <- "ok"

  fish$attr[is.na(fish$attr) &
    !is.na(fish$K) &
    fish$K >= species_attributes$K_MAX_WARN &
    fish$K < species_attributes$K_MAX_ERROR] <- "too_big"

  fish$attr[is.na(fish$attr) &
    !is.na(fish$K) &
    fish$K >= species_attributes$K_MAX_ERROR] <- "way_too_big"

  fish$attr[is.na(fish$attr) &
    !is.na(fish$K) &
    fish$K <= species_attributes$K_MIN_WARN &
    fish$K > species_attributes$K_MIN_ERROR] <- "too_small"

  fish$attr[is.na(fish$attr) &
    !is.na(fish$K) &
    fish$K <= species_attributes$K_MIN_ERROR] <- "way_too_small"

  graphics::split.screen(c(1, 2))
  graphics::screen(1) #

  with(fish, plot(stats::density(K, na.rm = T), main = "Condition Factor"))

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
  graphics::abline(v = species_attributes$K_MIN_WARN, col = "steelblue", lty = 3)

  graphics::screen(2)

  fish <- fish[!is.na(fish$RWT) & !is.na(fish[, var_idx]), ]

  # Length vs weight
  main_label <- sprintf("RWT vs %s", what)
  xlabel <- sprintf("%s (mm)", what)
  plot(
    y = fish$RWT, x = fish[, var_idx],
    xlab = xlabel, ylab = "RWT (g)",
    type = "n", main = main_label
  )

  # a factory function to return a curve with the given condition parameter:
  kcurve <- function(condition) {
    fct <- function(x) (condition * x^3) / 100000
    return(fct)
  }


  # kmin_err <- function(x) (species_attributes$K_MIN_ERROR * x^3) / 100000
  kmin_err <- kcurve(species_attributes$K_MIN_ERROR)
  graphics::curve(kmin_err, add = TRUE, col = "navy")

  # kmin_warn <- function(x) (species_attributes$K_MIN_WARN * x^3) / 100000
  kmin_warn <- kcurve(species_attributes$K_MIN_WARN)
  graphics::curve(kmin_warn, add = TRUE, col = "steelblue", lty = 3)

  # kmax_err <- function(x) (species_attributes$K_MAX_ERROR * x^3) / 100000
  kmax_err <- kcurve(species_attributes$K_MAX_ERROR)
  graphics::curve(kmax_err, add = TRUE, col = "red")

  # kmax_warn <- function(x) (species_attributes$K_MAX_WARN * x^3) / 100000
  kmax_warn <- kcurve(species_attributes$K_MAX_WARN)
  graphics::curve(kmax_warn, add = TRUE, col = "orange", lty = 3)


  graphics::points(fish[fish$attr == "reference", var_idx], fish$RWT[fish$attr == "reference"], col = "grey")
  graphics::points(fish[fish$attr == "ok", var_idx], fish$RWT[fish$attr == "ok"], col = "black")
  graphics::points(fish[fish$attr == "too_big", var_idx], fish$RWT[fish$attr == "too_big"], col = "orange", pch = 19)
  graphics::points(fish[fish$attr == "way_too_big", var_idx], fish$RWT[fish$attr == "way_too_big"], col = "red", pch = 8)
  graphics::points(fish[fish$attr == "too_small", var_idx],
    fish$RWT[fish$attr == "too_small"],
    col = "steelblue", pch = 19
  )
  graphics::points(fish[fish$attr == "way_too_small", var_idx], fish$RWT[fish$attr == "way_too_small"], col = "navy", pch = 8)

  graphics::close.screen(all = TRUE)
}


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
##'
##' @return plot
##' @author R. Adam Cottrill
bioattr_plot <- function(fish, species_attributes, what = c("FLEN", "TLEN", "RWT")) {
  what <- match.arg(what)

  # get the column numbers for data we want to plot depending on what has been selected
  var_idx <- which(names(fish) == what)
  min_idx <- which(names(species_attributes) == sprintf("%s_MIN", what))
  max_idx <- which(names(species_attributes) == sprintf("%s_MAX", what))


  fish$attr[is.na(fish$attr) &
    !is.na(fish[, var_idx]) &
    fish[, var_idx] <= species_attributes[, max_idx] &
    fish[, var_idx] >= species_attributes[, min_idx]] <- "ok"

  fish$attr[is.na(fish$attr) &
    !is.na(fish[, var_idx]) &
    fish[, var_idx] > species_attributes[, max_idx]] <- "too_big"

  fish$attr[is.na(fish$attr) &
    !is.na(fish[, var_idx]) &
    fish[, var_idx] < species_attributes[, min_idx]] <- "too_small"

  plot(stats::density(fish[, var_idx], na.rm = T), main = what)

  graphics::rug(fish[fish$attr == "reference", var_idx], col = "grey")
  graphics::rug(fish[fish$attr == "ok", var_idx], col = "black")
  graphics::rug(fish[fish$attr == "too_big", var_idx], col = "red")
  graphics::rug(fish[fish$attr == "too_small", var_idx], col = "red")

  graphics::abline(v = species_attributes[1, min_idx], col = "red", lty = 3)
  graphics::abline(v = species_attributes[1, max_idx], col = "red", lty = 3)
}
