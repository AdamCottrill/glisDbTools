##' Migrate a assessment project from Nearshore Master to GLIS Assessment Template
##'
##' This function is the workhorse of the UGLMU assessment project
##' mapper. It migrates the data for a single project into a
##' assessment project tempalte database that can then be run throught
##' process validate and uploaded to assessment project portal.  It
##' depends on an assocaited ms access file that contains queries to
##' make most of the transformations.  These queries are parameterized
##' to accept a prj_cd string, and are all named "get_tablename".
##' @title Poplate Template from Nearshore Master
##' @param prj_cds - the project code(s) of assessment project(s) to
##'   export into a template database.
##' @param src_dbase - path to the database with mapping queries
##' @param template_db - path to a copy of the current GLIS assessment
##'   template
##' @param fname - an option name to be used as name of the output
##'   file. If fname is not provided, one will be built by
##'   concatenating the first 3 project codes together.
##' @param lake - abbreviation that will be used to populate
##' @param verbose - should the append statements with submitted the
##'   the verbose flag.  Default is FALSE, but TRUE can be useful to
##'   debug database errors.
##' @param overwrite - should the target database be overwritten if it
##'   already exists?
##' @export
##' @return NULL
##' @author R. Adam Cottrill
nearshore_to_template <- function(
    prj_cds,
    src_dbase,
    template_db,
    fname = NA,
    lake = "HU",
    verbose = FALSE,
    overwrite = FALSE) {
  # TODO:
  process_type <- 1

  build_dir <- file.path(getwd(), "build")
  if (!dir.exists(build_dir)) {
    dir.create(build_dir)
  }

  if (is.na(fname)) {
    if (length(prj_cds) > 3) {
      msg <- paste0(
        "More than three project codes were submitted.\n",
        "Only the first three will we used name the populated template."
      )
      message(msg)
    }
    tmp <- prj_cds[1:min(length(prj_cds), 3)]
    fname <- paste(tmp, collapse = "-")
    fname <- sprintf("%s.accdb", fname)
  }

  trg_db <- file.path(build_dir, fname)

  check_db_setup(trg_db, template_db, overwrite)

  fn011 <- get_nearshore_fn011(prj_cds, src_dbase)
  missing <- setdiff(prj_cds, unique(fn011$PRJ_CD))
  if (length(missing)) {
    msg <- "Projects with the following project codes could not be found:\n"
    cat(msg)
    for (item in missing) {
      cat(sprintf("\t%s\n", item))
    }
  } else {
    msg <- "Popuplating Template Database for the following projects:\n"
    cat(msg)
    for (i in seq(1, nrow(fn011))) {
      cat(sprintf("\t'%s' (%s)\n", fn011$PRJ_NM[i], fn011$PRJ_CD[i]))
    }

    cat(sprintf("\tFN011 records: %s\n", nrow(fn011)))
  }

  date_format <- "%Y-%m-%d %H:%M:%S"
  fn011$PRJ_DATE0 <- as.Date(fn011$PRJ_DATE0, format = date_format)
  fn011$PRJ_DATE1 <- as.Date(fn011$PRJ_DATE1, format = date_format)
  fn011$LAKE <- lake

  append_data(trg_db, "FN011", fn011, verbose = verbose)

  fn022 <- get_nearshore_fn022(prj_cds, src_dbase)
  fn022$SSN_DATE0 <- as.Date(fn022$SSN_DATE0, format = date_format)
  fn022$SSN_DATE1 <- as.Date(fn022$SSN_DATE1, format = date_format)
  cat(sprintf("\tFN022 records: %s\n", nrow(fn022)))
  append_data(trg_db, "FN022", fn022, verbose = verbose)

  fn026 <- get_nearshore_fn026(prj_cds, src_dbase)
  cat(sprintf("\tFN026 records: %s\n", nrow(fn026)))
  append_data(trg_db, "FN026", fn026, verbose = verbose)

  fn026_subspace <- get_nearshore_fn026_subspace(prj_cds, src_dbase)
  cat(sprintf("\tFN026_subspace records: %s\n", nrow(fn026_subspace)))
  append_data(trg_db, "FN026_subspace", fn026_subspace, verbose = verbose)

  fn028 <- get_nearshore_fn028(prj_cds, src_dbase)
  cat(sprintf("\tFN028 records: %s\n", nrow(fn028)))

  fn028$EFFTM0_LT <- get_time(fn028$EFFTM0_LT)
  fn028$EFFTM0_GE <- get_time(fn028$EFFTM0_GE)
  # increment mode here, reset the counter if the project code changes:
  counter <- 1
  for (i in 1:nrow(fn028)) {
    if (i > 1) {
      if (fn028$PRJ_CD[i - 1] == fn028$PRJ_CD[i]) {
        counter <- counter + 1
      } else {
        counter <- 1
      }
    }
    fn028$MODE[i] <- sprintf("%02d", counter)
  }

  append_data(trg_db, "FN028", fn028, verbose = verbose)

  # Get list of gear/effort/process types from the glfishr package (requires VPN connection)
  gear_effort_process_types <- glfishr::get_gear_process_types()

  gear_effort_process_types <- gear_effort_process_types[
    gear_effort_process_types$GR %in% fn028$GR,
  ]
  cat(sprintf(
    "\tgear_effort_process_types records: %s\n",
    nrow(gear_effort_process_types)
  ))
  append_data(trg_db, "Gear_Effort_Process_Types", gear_effort_process_types)

  fn121 <- get_nearshore_fn121(prj_cds, src_dbase)
  cat(sprintf("\tFN121 records: %s\n", nrow(fn121)))

  # populate MODE, SSN, SUBSPACE:
  fn121 <- fn121_add_mode(fn121, fn028)

  fn121$SSN <- "00"
  fn121$SUBSPACE <- "11"
  fn121$PROCESS_TYPE <- process_type
  fn121$EFFDT0 <- as.Date(fn121$EFFDT0)
  fn121$EFFDT1 <- as.Date(fn121$EFFDT1)
  fn121$EFFTM0 <- get_time(fn121$EFFTM0)
  fn121$EFFTM1 <- get_time(fn121$EFFTM1)

  fn122 <- get_nearshore_fn122(prj_cds, src_dbase)
  cat(sprintf("\tFN122 records: %s\n", nrow(fn122)))

  cat("\tUpdating FN121.PROCESS_TYPE....\n")
  fn121 <- fn121_populate_process_type(
    fn028,
    fn121,
    fn122,
    gear_effort_process_types
  )
  append_data(trg_db, "FN121", fn121, verbose = verbose)

  # now we can append the FN122 records:
  append_data(trg_db, "FN122", fn122, verbose = verbose)

  fn123 <- get_nearshore_fn123(prj_cds, src_dbase)

  # before we can append the fn123 data, we need to build and insert our fn012 records.
  fn012 <- make_fn012(fn011)
  fn012 <- glfishr:::prune_unused_fn012(fn012, fn123)

  spc_grp_caught <- unique(fn123[, c("PRJ_CD", "SPC", "GRP")])
  fn012 <- merge(fn012, spc_grp_caught, all.y = TRUE)

  cat(sprintf("\tFN012 records: %s\n", nrow(fn012)))
  append_data(trg_db, "FN012", fn012, verbose = verbose)

  # now we can append our fn123 data:
  cat(sprintf("\tFN123 records: %s\n", nrow(fn123)))
  append_data(trg_db, "FN123", fn123, verbose = verbose)

  cat("\tUpdating FN122.waterhaul....\n")
  update_FN122_waterhaul(trg_db)

  fn125 <- get_nearshore_fn125(prj_cds, src_dbase)
  cat(sprintf("\tFN125 records: %s\n", nrow(fn125)))
  append_data(trg_db, "FN125", fn125, verbose = verbose)

  fn125_lamprey <- get_nearshore_fn125_lamprey(prj_cds, src_dbase)
  cat(sprintf("\tFN125_lamprey records: %s\n", nrow(fn125_lamprey)))
  if (nrow(fn125_lamprey)) {
    fn125_lamprey <- process_fn125_lamprey(fn125_lamprey)
    append_data(trg_db, "FN125_lamprey", fn125_lamprey, verbose = verbose)
  }

  fn125_tags <- get_nearshore_fn125_tags(prj_cds, src_dbase)
  fn125_xtags <- get_nearshore_fn125_xtags(prj_cds, src_dbase)
  if (nrow(fn125_xtags)) {
    msg <- paste0(
      sprintf(
        "\t **NOTE**: %s XTAGID values found!!.",
        nrow(fn125_xtags)
      ),
      "Check TAGSTAT and TAGDOC fields carefully.\n"
    )
    cat(msg)
    fn125_tags <- rbind(fn125_tags, fn125_xtags)
    fn125_tags <- fn125_tags[
      with(
        fn125_tags,
        order(PRJ_CD, SAM, EFF, SPC, GRP, FISH, FISH_TAG_ID)
      ),
    ]
    counter <- 1
    for (i in 1:nrow(fn125_tags)) {
      if (i > 1) {
        if (fn125_tags$FISH_TAG_ID[i - 1] == fn125_tags$FISH_TAG_ID[i]) {
          counter <- counter + 1
        } else {
          counter <- 1
        }
      }
      fn125_tags$FISH_TAG_ID[i] <- counter
    }
  }

  cat(sprintf("\tFN125_tag records: %s\n", nrow(fn125_tags)))
  append_data(trg_db, "FN125_tags", fn125_tags, verbose = verbose)

  fn126 <- get_nearshore_fn126(prj_cds, src_dbase)
  cat(sprintf("\tFN126 records: %s\n", nrow(fn126)))
  append_data(trg_db, "FN126", fn126, verbose = verbose)

  # get the FN125 preferred age data:
  fn125_ages <- get_nearshore_fn125_ages(prj_cds, src_dbase)

  if (file.exists("xagem2agemt.csv")) {
    # update any missing agemt values from their xagem using the values in this csv:
    xagem2agemt <- utils::read.csv("xagem2agemt.csv")
    fn125_ages <- merge(fn125_ages, xagem2agemt, by = "XAGEM", all.x = T)
    fn125_ages$AGEMT <- ifelse(
      !is.na(fn125_ages$AGEMT.x),
      fn125_ages$AGEMT.x,
      fn125_ages$AGEMT.y
    )
    fn125_ages <- fn125_ages[
      ,
      !(names(fn125_ages) %in% c("XAGEM", "AGEMT.x", "AGEMT.y"))
    ]
  } else {
    msg <- "Unable to find 'xagem2agemt.csv' skipping FN125 agemt updates."
    message(msg)
    fn125_ages$XAGEM <- NULL
  }

  fn127 <- get_nearshore_fn127(prj_cds, src_dbase)

  fn127 <- rbind(fn125_ages, fn127)
  cat(sprintf("\tSC127 records: %s\n", nrow(fn127)))
  if (nrow(fn127)) {
    fn127 <- fn127[
      with(fn127, order(PRJ_CD, SAM, EFF, SPC, GRP, FISH, AGEID)),
    ]
    fn127$AGEMT[is.na(fn127$AGEMT)] <- "99999"
    append_data(trg_db, "FN127", fn127, verbose = verbose)
  }

  msg <- sprintf(
    "Template populated from %s on %s",
    basename(src_dbase),
    Sys.time()
  )
  update_readme(trg_db, msg)

  msg <- paste0(
    sprintf(
      "Done. The populated database can be found here: \n\t%s.  \n",
      trg_db
    ),
    "You should be able to check it with Process Validate and upload it to the assessment portal.\n"
  )
  message(msg)
}
