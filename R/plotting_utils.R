##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param src
##' @param SPC
##' @param GRP
##' @param what
##' @param glis_fish
##' @return
##' @author R. Adam Cottrill
procval_plot <- function(src, SPC, GRP = "00",
                         what = c("FLEN", "TLEN", "RWT", "Kflen", "Ktlen"),
                         glis_fish = NULL) {
  what <- match.arg(what)

  sql012 <- sprintf("SELECT * from FN012 where [spc] = '%s' AND GRP='%s'", SPC, GRP)
  species_attributes <- fetch_sql(sql012, src)

  sql125 <- sprintf("SELECT [PRJ_CD], [SAM], [EFF], [SPC], [GRP], [FISH], [FLEN], [TLEN], [RWT] from [FN125] where [spc] ='%s' AND GRP='%s'", SPC, GRP)
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



# two pannel plot of Condition:
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param fish
##' @param species_attributes
##' @param what
##' @return
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

  split.screen(c(1, 2))
  screen(1) #

  with(fish, plot(density(K, na.rm = T), main = "Condition Factor"))

  # density of observed K

  rug(fish$K[fish$attr == "reference"], col = "grey")

  rug(fish$K[fish$attr == "ok"], col = "black")
  rug(fish$K[fish$attr == "too_big"], col = "orange")
  rug(fish$K[fish$attr == "way_too_big"], col = "red")
  rug(fish$K[fish$attr == "too_small"], col = "steelblue")
  rug(fish$K[fish$attr == "way_too_small"], col = "navy")

  abline(v = species_attributes$K_MAX_ERROR, col = "red")
  abline(v = species_attributes$K_MIN_ERROR, col = "navy")

  abline(v = species_attributes$K_MAX_WARN, col = "orange", lty = 3)
  abline(v = species_attributes$K_MIN_WARN, col = "steelblue", lty = 3)

  screen(2)

  fish <- fish[!is.na(fish$RWT) & !is.na(fish[, var_idx]), ]

  # Length vs weight
  main_label <- sprintf("RWT vs %s", what)
  xlabel <- sprintf("%s (mm)", what)
  plot(
    y = fish$RWT, x = fish[, var_idx],
    xlab = xlabel, ylab = "RWT (g)",
    type = "n", main = main_label
  )

  points(fish[fish$attr == "reference", var_idx], fish$RWT[fish$attr == "reference"], col = "grey")
  points(fish[fish$attr == "ok", var_idx], fish$RWT[fish$attr == "ok"], col = "black")
  points(fish[fish$attr == "too_big", var_idx], fish$RWT[fish$attr == "too_big"], col = "orange", pch = 19)
  points(fish[fish$attr == "way_too_big", var_idx], fish$RWT[fish$attr == "way_too_big"], col = "red", pch = 8)
  points(fish[fish$attr == "too_small", var_idx],
    fish$RWT[fish$attr == "too_small"],
    col = "steelblue", pch = 19
  )
  points(fish[fish$attr == "way_too_small", var_idx], fish$RWT[fish$attr == "way_too_small"], col = "navy", pch = 8)

  close.screen(all = TRUE)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param fish
##' @param species_attributes
##' @param what
##' @return
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

  plot(density(fish[, var_idx], na.rm = T), main = what)

  rug(fish[fish$attr == "reference", var_idx], col = "grey")
  rug(fish[fish$attr == "ok", var_idx], col = "black")
  rug(fish[fish$attr == "too_big", var_idx], col = "red")
  rug(fish[fish$attr == "too_small", var_idx], col = "red")

  abline(v = species_attributes[1, min_idx], col = "red", lty = 3)
  abline(v = species_attributes[1, max_idx], col = "red", lty = 3)
}
