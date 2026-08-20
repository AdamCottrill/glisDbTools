##' Migrate a creel project from Creesys 4.1 to GLIS Creel Template
##'
##' This function is the workhorse of the UGLMU creel mapper. It
##' migrates the data for a single project into a creel template
##' database that can then be run through Process Validate and
##' uploaded to creel portal.  It depends on an associated ms access
##' file that contains queries to make most of the transformations.
##' These queries are parameterized to accept a prj_cd string, and are
##' all named "get_tablename".
##' @title Populate Template from Creesys
##' @param prj_cd - the project code of the creel we want to migrate
##'   into a template database.
##' @param src_dbase - path to the database with mapping queries
##' @param template_db - path to a copy of the current GLIS creel
##'   template
##' @param lake - the Lake abbreviation that will be used to populate
##'   the F011 table
##' @param verbose - should the append statements with submitted the
##'   the verbose flag.  Default is FALSE, but TRUE can be useful to
##'   debug database errors.
##' @param overwrite - should the target database be overwritten if it
##'   already exists?
##' @export
##' @return NULL
##' @author R. Adam Cottrill
creesys_to_template <- function(
  prj_cd,
  src_dbase,
  template_db,
  lake = "HU",
  verbose = FALSE,
  overwrite = FALSE
) {
  build_dir <- file.path(getwd(), "build")
  if (!dir.exists(build_dir)) {
    dir.create(build_dir)
  }

  trg_db <- file.path(build_dir, paste0(prj_cd, ".accdb"))

  check_db_setup(trg_db, template_db, overwrite)

  fn011 <- get_creesys_fn011(prj_cd, src_dbase)

  if (nrow(fn011) == 0) {
    msg <- sprintf("Project with project code '%s' could not be found.", prj_cd)
    stop(msg)
  } else {
    msg <- sprintf(
      "Popuplate Template Database for:\n '%s' (%s)\n",
      fn011$PRJ_NM,
      prj_cd
    )
    cat(msg)
    cat(sprintf("\tSC011 records: %s\n", nrow(fn011)))
  }

  date_format <- "%Y-%m-%d %H:%M:%S"
  fn011$PRJ_DATE0 <- as.Date(fn011$PRJ_DATE0, format = date_format)
  fn011$PRJ_DATE1 <- as.Date(fn011$PRJ_DATE1, format = date_format)
  fn011$LAKE <- lake
  fn011$AUXILIARY_DATA <- FALSE
  append_data(trg_db, "FN011", fn011, verbose = verbose)

  # Lake Huron still does not have default fn012 values for creels, so
  # we will use one from on of the other lakes instead, and select rows
  # based on fish caught in this creel:

  default_sc012 <- glfishr::get_SC012_Protocol(list(lake = "ER"))
  default_sc012$LAKE <- NULL

  # a dummy row, just incase there are species in our data that were not
  # in the defautl 012
  fn012_row <- default_sc012[default_sc012$SPC == "081", ]
  fn012_row$SPC <- NULL
  fn012_row$SPC_NMCO <- "CHECK ME"

  fn022 <- get_creesys_fn022(prj_cd, src_dbase)
  fn022$SSN_DATE0 <- as.Date(fn022$SSN_DATE0, format = date_format)
  fn022$SSN_DATE1 <- as.Date(fn022$SSN_DATE1, format = date_format)
  cat(sprintf("\tSC022 records: %s\n", nrow(fn022)))
  append_data(trg_db, "FN022", fn022, verbose = verbose)

  fn023 <- get_creesys_fn023(prj_cd, src_dbase)
  cat(sprintf("\tSC023 records: %s\n", nrow(fn023)))
  append_data(trg_db, "FN023", fn023, verbose = verbose)

  fn024 <- get_creesys_fn024(prj_cd, src_dbase)
  cat(sprintf("\tSC024 records: %s\n", nrow(fn024)))
  fn024$PRDTM0 <- get_time(fn024$PRDTM0)
  fn024$PRDTM1 <- get_time(fn024$PRDTM1)
  append_data(trg_db, "FN024", fn024, verbose = verbose)

  fn025 <- get_creesys_fn025(prj_cd, src_dbase)
  cat(sprintf("\tSC025 records: %s\n", nrow(fn025)))
  fn025$DATE <- as.Date(fn025$DATE, format = date_format)
  append_data(trg_db, "FN025", fn025, verbose = verbose)

  fn026 <- get_creesys_fn026(prj_cd, src_dbase)
  cat(sprintf("\tSC026 records: %s\n", nrow(fn026)))
  append_data(trg_db, "FN026", fn026, verbose = verbose)

  fn026_subspace <- get_creesys_fn026_subspace(prj_cd, src_dbase)
  cat(sprintf("\tSC026_subspace records: %s\n", nrow(fn026_subspace)))
  append_data(trg_db, "FN026_subspace", fn026_subspace, verbose = verbose)

  fn028 <- get_creesys_fn028(prj_cd, src_dbase)
  cat(sprintf("\tSC028 records: %s\n", nrow(fn028)))
  append_data(trg_db, "FN028", fn028, verbose = verbose)

  fn111 <- get_creesys_fn111(prj_cd, src_dbase)
  fn111$DATE <- as.Date(fn111$DATE, format = date_format)
  fn111$SAMTM0 <- get_time(fn111$SAMTM0)
  cat(sprintf("\tSC111 records: %s\n", nrow(fn111)))
  append_data(trg_db, "FN111", fn111, verbose = verbose)

  fn112 <- get_creesys_fn112(prj_cd, src_dbase)
  cat(sprintf("\tSC112 records: %s\n", nrow(fn112)))
  fn112$ATYTM0 <- get_time(fn112$ATYTM0)
  fn112$ATYTM1 <- get_time(fn112$ATYTM1)
  append_data(trg_db, "FN112", fn112, verbose = verbose)

  fn121 <- get_creesys_fn121(prj_cd, src_dbase)
  cat(sprintf("\tSC121 records: %s\n", nrow(fn121)))
  fn121$ITVSEQ <- ifelse(
    is.na(fn121$ITVSEQ),
    as.numeric(rownames(fn121)),
    fn121$ITVSEQ
  )
  fn121$DATE <- as.Date(fn121$DATE, format = date_format)
  fn121$ITVTM0 <- get_time(fn121$ITVTM0)
  fn121$SAMTM0 <- get_time(fn121$SAMTM0)
  fn121$EFFDT0 <- as.Date(fn121$EFFDT0, format = date_format)
  fn121$EFFTM0 <- get_time(fn121$EFFTM0)
  fn121$EFFDT1 <- as.Date(fn121$EFFDT1, format = date_format)
  fn121$EFFTM1 <- get_time(fn121$EFFTM1)
  append_data(trg_db, "FN121", fn121, verbose = verbose)

  fn123 <- get_creesys_fn123(prj_cd, src_dbase)
  cat(sprintf("\tSC123 records: %s\n", nrow(fn123)))

  # before we can append the FN123 data, we need to add in the FN012
  spc_caught <- unique(fn123[, c("PRJ_CD", "SPC")])
  cat(sprintf(
    "\tBuilding SC012 table with %s species-grp combinations\n",
    nrow(spc_caught)
  ))

  fn012 <- merge(spc_caught, default_sc012, by = "SPC", all.x = TRUE)
  # check for unmatched fn012 records and fill those in with the default
  # row if necessary:
  missing <- fn012[is.na(fn012$GRP), c("PRJ_CD", "SPC")]
  if (nrow(missing) > 0) {
    fn012 <- fn012[!is.na(fn012$GRP), ]
    missing <- cbind(missing, fn012_row)
    fn012 <- rbind(fn012, missing)
  }
  append_data(trg_db, "FN012", fn012, verbose = verbose)

  # now we can append the fn123 data
  append_data(trg_db, "FN123", fn123, verbose = verbose)

  fn125 <- get_creesys_fn125(prj_cd, src_dbase)
  cat(sprintf("\tSC125 records: %s\n", nrow(fn125)))
  append_data(trg_db, "FN125", fn125, verbose = verbose)

  fn125_tags <- get_creesys_fn125_tags(prj_cd, src_dbase)
  # we mmight need to increment FISH_TAGID here
  cat(sprintf("\tSC125_TAGS records: %s\n", nrow(fn125_tags)))
  append_data(trg_db, "FN125_tags", fn125_tags, verbose = verbose)

  fn125_lamprey <- get_creesys_fn125_lamprey(prj_cd, src_dbase)
  cat(sprintf("\tSC125_Lamprey records: %s\n", nrow(fn125_lamprey)))
  if (nrow(fn125_lamprey)) {
    fn125_lamprey <- process_fn125_lamprey(fn125_lamprey)
    append_data(trg_db, "FN125_lamprey", fn125_lamprey, verbose = verbose)
  }

  fn126 <- get_creesys_fn126(prj_cd, src_dbase)
  cat(sprintf("\tSC126 records: %s\n", nrow(fn126)))
  if (nrow(fn126)) {
    append_data(trg_db, "FN126", fn126, verbose = verbose)
  }

  # get the FN125 preferred age data:
  fn125_ages <- get_creesys_fn125_ages(prj_cd, src_dbase)

  if (file.exists("xagem2agemt.csv")) {
    # update any missing agemt values from their xagem using
    # the values in this csv:
    xagem2agemt <- utils::read.csv("xagem2agemt.csv")
    fn125_ages <- merge(fn125_ages, xagem2agemt, by = "XAGEM", all.x = TRUE)
    fn125_ages$AGEMT <- ifelse(
      !is.na(fn125_ages$AGEMT.x),
      fn125_ages$AGEMT.x,
      fn125_ages$AGEMT.y
    )
    fn125_ages <- fn125_ages[,
      !(names(fn125_ages) %in% c("XAGEM", "AGEMT.x", "AGEMT.y"))
    ]
  } else {
    msg <- "Unable to find 'xagem2agemt.csv' skipping FN125 agemt updates."
    message(msg)
    fn125_ages$XAGEM <- NULL
  }

  fn127 <- get_creesys_fn127(prj_cd, src_dbase)

  fn127 <- rbind(fn125_ages, fn127)
  cat(sprintf("\tSC127 records: %s\n", nrow(fn127)))
  if (nrow(fn127)) {
    append_data(trg_db, "FN127", fn127, verbose = verbose)
  }

  # angler questions:
  angler_questions <- get_creesys_questions(prj_cd, src_dbase)
  cat(sprintf("\tAngler Questions records: %s\n", nrow(angler_questions)))

  if (nrow(angler_questions)) {
    append_data(trg_db, "AnglerQuestions", angler_questions, verbose = verbose)
  }

  # angler answers:
  angler_answers <- get_creesys_answers(prj_cd, src_dbase)
  cat(sprintf("\tAngler Answers records: %s\n", nrow(angler_answers)))

  if (nrow(angler_answers)) {
    append_data(trg_db, "AnglerAnswers", angler_answers, verbose = verbose)
  }

  if (toupper(fn023$DTP_NM[fn023$DTP == 1][1]) != "WEEKDAY") {
    msg <- paste0(
      "Looks like weekend and weekday daytypes are switched.",
      "\nSwitching them for you."
    )
    message(msg)
    switch_and_update_dtp(trg_db)
  }

  msg <- sprintf(
    "Template populated from %s on %s",
    basename(src_dbase),
    Sys.time()
  )
  update_readme(trg_db, msg)

  msg <- paste0(
    sprintf(
      "Done. The populated database can be found here: %s.  \n",
      trg_db
    ),
    "You should be able to check it with Process Validate and upload ",
    "to creel portal.\n"
  )
  message(msg)
}
