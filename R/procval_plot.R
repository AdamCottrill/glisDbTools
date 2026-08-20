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
procval_plot <- function(
    src,
    spc,
    grp = "00",
    what = c("FLEN", "TLEN", "RWT", "Kflen", "Ktlen"),
    glis_fish = NULL) {
  what <- match.arg(what)

  sql012 <- sprintf(
    "SELECT * from FN012 where [spc] = '%s' AND GRP='%s'",
    spc,
    grp
  )
  species_attributes <- fetch_sql(src, sql012)

  # check for number of fn012 records
  # issue warn if  no with spc= and grp= could be found the target database.

  if (nrow(species_attributes) == 0) {
    msg <- sprintf(
      "No records were returned from the FN012 table of
        the target database for spc='%s' and grp='%s'",
      spc,
      grp
    )
    stop(msg)
  }

  sql125 <- sprintf(
    "SELECT [PRJ_CD], [SAM], [EFF], [SPC], [GRP], [FISH],
                     [FLEN], [TLEN], [RWT]
                     from [FN125] where [spc] ='%s' AND GRP='%s'",
    spc,
    grp
  )
  fish <- fetch_sql(src, sql125)

  if (nrow(fish) == 0) {
    msg <- sprintf(
      "No records were returned from the FN125 table of
       the target database for spc='%s' and grp='%s'",
      spc,
      grp
    )
    stop(msg)
  }

  fish$attr <- NA

  # check for number of fn125 records
  # issue warn if  no with spc= and grp= could be found the target database.

  if (!is.null(glis_fish)) {
    glis_fish <- glis_fish[, c(
      "PRJ_CD",
      "SAM",
      "EFF",
      "SPC",
      "GRP",
      "FISH",
      "FLEN",
      "TLEN",
      "RWT"
    )]
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
