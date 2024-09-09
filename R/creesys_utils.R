##' Migrate a creel project from Creesys 4.1 to GLIS Creel Template
##'
##' This function is the workhorse of the UGLMU creel mapper. It
##' migrates the data for a single project into a creel tempalte
##' database that can then be run throught process validate and
##' uploaded to creel portal.  It depends on an assocaited ms access
##' file that contains queries to make most of the transformations.
##' These queries are parameterized to accept a prj_cd string, and are
##' all named "get_tablename".
##' @title Poplate Template from Creesys
##' @param prj_cd - the project code of the creel we want to migrate
##'   into a template database.
##' @param src_dbase - path to the database with mapping queries
##' @param template_db - path to a copy of the current GLIS creel
##'   template
##' @param lake - abbreviation that will be used to populate
##' @param verbose - should the append statements with submitted the
##'   the verbose flag.  Default is FALSE, but TRUE can be useful to
##'   debug database errors.
##' @param overwrite - should the target database be overwritten if it
##'   already exists?
##' @export
##' @return NULL
##' @author R. Adam Cottrill
creesys_to_template <- function(prj_cd, src_dbase, template_db, lake = "HU", verbose = FALSE, overwrite = FALSE) {
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
    msg <- sprintf("Popuplate Template Database for:\n '%s' (%s)\n", fn011$PRJ_NM, prj_cd)
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
  fn121$ITVSEQ <- ifelse(is.na(fn121$ITVSEQ),
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
    # update any missing agemt values from their xagem using the values in this csv:
    xagem2agemt <- utils::read.csv("xagem2agemt.csv")
    fn125_ages <- merge(fn125_ages, xagem2agemt, by = "XAGEM", all.x = T)
    fn125_ages$AGEMT <- ifelse(!is.na(fn125_ages$AGEMT.x), fn125_ages$AGEMT.x, fn125_ages$AGEMT.y)
    fn125_ages <- fn125_ages[, !(names(fn125_ages) %in% c("XAGEM", "AGEMT.x", "AGEMT.y"))]
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
    message("Looks like weekend and weekday daytypes are switched.\nSwitching them for you.")
    switch_and_update_dtp(trg_db)
  }

  populate_readme(trg_db, src_dbase)

  msg <- paste0(
    sprintf(
      "Done. The populated database can be found here: %s.  \n", trg_db
    ),
    "You should be able to check it with Process Validate and upload to creel portal.\n"
  )
  message(msg)
}



##' Fetch FN011 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN011 data in a format that matches the FN011 table in the upload
##' template.
##' @title Fetch FN011 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN011 data for the specified
##'   creel
##' @author R. Adam Cottrill
get_creesys_fn011 <- function(prj_cd, src_db) {
  # a function replace the Get_FN011 query from the mapper database.

  sql <- "SELECT YEAR, PRJ_CD, CONTMETH, PRJ_DATE0, PRJ_DATE1, PRJ_LDR, PRJ_NM, COMMENT0
        FROM fn011
        WHERE PRJ_CD='%s';"

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(stmt, src_db)
  return(dat)
}


##' Fetch FN022 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN022 data in a format that matches the FN022 table in the upload
##' template.
##' @title Fetch FN022 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN022 data for the specified
##'   creel
##' @author R. Adam Cottrill
get_creesys_fn022 <- function(prj_cd, src_db) {
  # a function replace the Get_FN022 query from the mapper database.


  sql <- "SELECT PRJ_CD, SSN, SSN_DATE0, SSN_DATE1, SSN_DES
    FROM FN022
    GROUP BY PRJ_CD, SSN, SSN_DATE0, SSN_DATE1, SSN_DES
    HAVING PRJ_CD='%s'
    ORDER BY PRJ_CD, SSN;
    "
  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(stmt, src_db)
  return(dat)
}



##' Fetch FN023 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN023 data in a format that matches the FN023 table in the upload
##' template.
##' @title Fetch FN023 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN023 data for the specified
##'   creel
##' @author R. Adam Cottrill
get_creesys_fn023 <- function(prj_cd, src_db) {
  # a function replace the Get_FN023 query from the mapper database.

  sql <- "SELECT PRJ_CD, SSN, DTP, DTP_NM, DOW_LST
        FROM FN023
        WHERE PRJ_CD='%s'
        ORDER BY PRJ_CD, SSN, DTP;"

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(stmt, src_db)
  return(dat)
}



##' Fetch FN024 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN024 data in a format that matches the FN024 table in the upload
##' template.
##' @title Fetch FN024 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN024 data for the specified
##'   creel
##' @author R. Adam Cottrill
get_creesys_fn024 <- function(prj_cd, src_db) {
  # a function replace the Get_FN024 query from the mapper database.

  sql <- "SELECT PRJ_CD, SSN, DTP, PRD, PRDTM0, PRD_DUR, PRDTM1, TIME_WT
        FROM FN024
        WHERE PRJ_CD='%s'
        ORDER BY PRJ_CD, SSN, DTP, PRD;"

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(stmt, src_db)
  return(dat)
}


##' Fetch FN025 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN025 data in a format that matches the FN025 table in the upload
##' template.
##' @title Fetch FN025 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN025 data for the specified
##'   creel
##' @author R. Adam Cottrill
get_creesys_fn025 <- function(prj_cd, src_db) {
  # a function replace the Get_FN025 query from the mapper database.

  sql <- "SELECT FN025.PRJ_CD, SSN, FN025.DATE, DTP1, 'Holiday' AS DESCRIPTION
        FROM FN022 INNER JOIN FN025 ON FN022.PRJ_CD = FN025.PRJ_CD
        WHERE FN025.PRJ_CD='%s' AND
        FN025.Date Between [SSN_DATE0] And [SSN_DATE1]
        ORDER BY FN025.PRJ_CD, FN025.Date;
        "

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(stmt, src_db)
  return(dat)
}




##' Fetch FN026 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN026 data in a format that matches the FN026 table in the upload
##' template.
##' @title Fetch FN026 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN026 data for the specified
##'   creel
##' @author R. Adam Cottrill
get_creesys_fn026 <- function(prj_cd, src_db) {
  # a function replace the Get_FN026 query from the mapper database.

  sql <- "SELECT PRJ_CD, SPACE, SPACE_DES, AREA_WT AS SPACE_WT, SPACE_SIZ, DD_LAT, DD_LON
        FROM FN026
        WHERE PRJ_CD='%s'
        ORDER BY PRJ_CD, Space;"

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(stmt, src_db)
  return(dat)
}



##' Fetch FN026_subspace data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN026_subspace data in a format that matches the FN026_subspace
##' table in the upload template.
##' @title Fetch FN026_subspace data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN026_subspace data for the
##'   specified creel
##' @author R. Adam Cottrill
get_creesys_fn026_subspace <- function(prj_cd, src_db) {
  # a function replace the Get_FN026_subspace query from the mapper database.

  sql <- "SELECT PRJ_CD, SPACE, SPACE AS SUBSPACE, SPACE_DES AS SUBSPACE_DES,
        SPACE_SIZ AS SUBSPACE_SIZ, DD_LAT, DD_LON, 0 AS SUBSPACE_WT
        FROM FN026
        WHERE PRJ_CD='%s'
        ORDER BY PRJ_CD, Space;"

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(stmt, src_db)
  return(dat)
}


##' Fetch FN028 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN028 data in a format that matches the FN028 table in the upload
##' template.
##' @title Fetch FN028 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN028 data for the specified
##'   creel
##' @author R. Adam Cottrill
get_creesys_fn028 <- function(prj_cd, src_db) {
  # a function replace the Get_FN028 query from the mapper database.

  sql <- "SELECT PRJ_CD, MODE, MODE_DES, ATYUNIT, ITVUNIT, CHKFLAG, COMMENT8
        FROM FN028
        WHERE PRJ_CD='%s';"

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(stmt, src_db)
  return(dat)
}



##' Fetch FN111 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN111 data in a format that matches the FN111 table in the upload
##' template.
##' @title Fetch FN111 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN111 data for the specified
##'   creel
##' @author R. Adam Cottrill
get_creesys_fn111 <- function(prj_cd, src_db) {
  # a function replace the Get_FN111 query from the mapper database.

  sql <- "SELECT PRJ_CD, SAMA, STRATUM, MODE, DATE, SAMTM0, COMMENT1, DOW,
        SPACE AS SUBSPACE, WEATHER AS WEATHER_EFFECT, ATYDATA, CREW,
        AIRTEM0 AS AIRTEM, SITEM0 AS SITEM, WIND, CLOUD_PC, PRECIP
        FROM FN111
        WHERE PRJ_CD='%s'
        ORDER BY PRJ_CD, SAMA, STRATUM;"

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(stmt, src_db)
  return(dat)
}


##' Fetch FN112 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN112 data in a format that matches the FN112 table in the upload
##' template.
##' @title Fetch FN112 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN112 data for the specified
##'   creel
##' @author R. Adam Cottrill
get_creesys_fn112 <- function(prj_cd, src_db) {
  # a function replace the Get_FN112 query from the mapper database.

  sql <- "SELECT PRJ_CD, SAMA, ATYTM0, ATYTM1, ATYCNT, ITVCNT, CHKCNT, COMMENT2
        FROM FN112
        WHERE PRJ_CD='%s'
        ORDER BY PRJ_CD, SAMA, ATYTM0;"

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(stmt, src_db)
  return(dat)
}


##' Fetch FN121 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN121 data in a format that matches the FN121 table in the upload
##' template.
##' @title Fetch FN121 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN121 data for the specified
##'   creel
##' @author R. Adam Cottrill
get_creesys_fn121 <- function(prj_cd, src_db) {
  # a function replace the Get_FN121 query from the mapper database.

  sql <- "
        select distinct
            FN121.PRJ_CD,
            FN121.SAM,
            FN121.SAMA,
            ITVSEQ,
            IIF(
                ISNULL(
                    [FN121].[STRATUM]
                ),
                [FN111].[STRATUM],
                [FN121].[STRATUM]
            ) AS STRATUM,
            DATE,
            DOW,
            ITVTM0,
            SPACE AS SUBSPACE,
            MODE,
            SAMTM0,
            EFFDT0,
            EFFTM0,
            EFFDT1,
            EFFTM1,
            EFFCMP,
            EFFDURC,
            EFFDUR,
            PERSONS,
            ANGLERS,
            RODS,
            COMMENT1,
            GRID AS GRID5,
            DD_LAT,
            DD_LON,
            ANGMETH,
            ANGGUID,
            ANGORIG,
            ANGVIS,
            ANGOP1,
            ANGOP2,
            ANGOP3,
            ANGOP4,
            ANGOP5
        from
            FN121
        inner join (
        SELECT PRJ_CD, SAMA, STRATUM
                FROM FN111
        ) as get_fn111 on
                FN121.SAMA = get_fn111.SAMA and
                FN121.PRJ_CD = get_fn111.PRJ_CD
        where FN121.PRJ_CD='%s'
        order by
            FN121.SAM,
            FN121.SAMA;"

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(stmt, src_db)
  return(dat)
}

##' Fetch FN123 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN123 data in a format that matches the FN123 table in the upload
##' template.
##' @title Fetch FN123 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN123 data for the specified
##'   creel
##' @author R. Adam Cottrill
get_creesys_fn123 <- function(prj_cd, src_db) {
  # a function replace the Get_FN123 query from the mapper database.

  sql <- "SELECT PRJ_CD, SAM, EFF, SPC, GRP, SEK, HVSCNT, RLSCNT,
        BIOCNT AS  MESCNT, MESWT, COMMENT3
        FROM FN123
        WHERE PRJ_CD='%s'
        ORDER BY PRJ_CD, SAM, EFF, Spc, GRP, SEK;
        "

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(stmt, src_db)
  return(dat)
}

##' Fetch FN125 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN125 data in a format that matches the FN125 table in the upload
##' template.
##' @title Fetch FN125 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN125 data for the specified
##'   creel
##' @author R. Adam Cottrill
get_creesys_fn125 <- function(prj_cd, src_db) {
  # a function replace the Get_FN125 query from the mapper database.

  sql <- "SELECT PRJ_CD, TRIM(STR([FN125].[SAM])) AS SAM, EFF, SPC, GRP, FISH, FLEN,
        TLEN, RWT, SEX, MAT, GON, CLIPC, GIRTH, AGEST, NODC, COMMENT5, TISSUE,
        0 AS FDSAM,
        '' AS EVISWT,
        '' AS GONWT,
        '' AS STOM_CONTENTS_WT
        FROM FN125
        WHERE PRJ_CD='%s'
        ORDER BY PRJ_CD, Trim(Str([FN125].[SAM])), EFF, Spc, GRP, FISH;
        "

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(stmt, src_db)
  return(dat)
}

##' Fetch FN125_tag data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' tag data from the FN125 table in a format that matches the
##' FN125_tags table in the upload template.
##' @title Fetch FN125 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN125 data for the specified
##'   creel
##' @author R. Adam Cottrill
get_creesys_fn125_tags <- function(prj_cd, src_db) {
  # a function replace the Get_FN125_tags query from the mapper database.

  sql <- "SELECT PRJ_CD, SAM, EFF, SPC, GRP, FISH, 1 AS FISH_TAG_ID,
          TAGID, TAGDOC, TAGSTAT, XCWTSEQ AS CWTSEQ, '' AS COMMENT_TAG
          FROM FN125
          WHERE PRJ_CD='%s' AND TAGID Is Not Null And TAGID<>'0';"

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(stmt, src_db)
  return(dat)
}



##' Fetch FN125_lamprey data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' lamprey data from the FN125 table in a format that can be
##' processed to matches the FN125_lampreys table in the upload
##' template.
##' @title Fetch FN125 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN125 data for the specified
##'   creel
##' @author R. Adam Cottrill
get_creesys_fn125_lamprey <- function(prj_cd, src_db) {
  # a function replace the Get_FN125_tags query from the mapper database.

  sql <- "SELECT PRJ_CD, TRIM(STR([FN125].[SAM])) AS SAM, EFF, SPC, GRP,
          FISH, 1 AS LAMID, LAMIJC, XLAM, COMMENT5 AS COMMENT_LAM
          FROM FN125
          WHERE (PRJ_CD='%1$s' AND LAMIJC IS NOT NULL) OR
          (PRJ_CD='%1$s' AND XLAM Is Not Null);"

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(stmt, src_db)
  return(dat)
}



##' Fetch FN126 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN126 data in a format that matches the FN126 table in the upload
##' template.
##' @title Fetch FN126 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN126 data for the specified
##'   creel
##' @author R. Adam Cottrill
get_creesys_fn126 <- function(prj_cd, src_db) {
  # a function replace the Get_FN126 query from the mapper database.

  sql <- "SELECT PRJ_CD, TRIM(STR([FN126].[SAM])) AS SAM, EFF, SPC,
        '00' AS GRP, FISH, FOOD, TAXON, FDCNT, '' AS FDMES,
        '' AS FDVAL, '' AS LF, '' AS COMMENT6
        FROM FN126
        WHERE PRJ_CD='%s'
        ORDER BY PRJ_CD, Trim(Str([FN126].[SAM])), EFF, Spc, '00', FISH, FOOD;"

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(stmt, src_db)
  return(dat)
}


##' Fetch FN125 Age data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' age data from the FN125 table in a format that matches the FN127
##' table in the upload template.  It automatically assigns
##' preferred=TRUE and sets ageid=125.
##' @title Fetch FN126 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN126 data for the specified
##'   creel
##' @author R. Adam Cottrill
get_creesys_fn125_ages <- function(prj_cd, src_db) {
  sql <- "SELECT
         PRJ_CD,
         TRIM(STR([FN125].[SAM])) AS SAM,
         EFF,
         SPC,
         GRP,
         FISH,
         125 AS AGEID,
         AGE AS AGEA,
         'TRUE' AS PREFERRED,
         AGEMT,
         XAGEM,
         CONF,
         '' AS NCA,
         EDGE,
         '' AS COMMENT7,
          '' AS AGESTRM,
          '' AS AGELAKE,
          '' AS SPAWNCHKCNT,
          IIF(
             ISNULL(
                 [AGEA]
             ),
             1,
             NULL
         ) AS AGE_FAIL
     FROM
         FN125
     where PRJ_CD='%s' and  XAGEM is not null
     order by
         PRJ_CD,
         Trim(Str([FN125].[SAM])),
         EFF,
         Spc,
         GRP,
         FISH;
     "

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(stmt, src_db)
  return(dat)
}

##' Fetch FN127 data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' FN127 data in a format that matches the FN127 table in the upload
##' template.
##' @title Fetch FN127 data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN127 data for the specified
##'   creel
##' @author R. Adam Cottrill
get_creesys_fn127 <- function(prj_cd, src_db) {
  # a function replace the Get_FN126 query from the mapper database.

  sql <- "SELECT
     PRJ_CD,
     TRIM(STR([FN127].[SAM])) AS SAM,
     EFF,
     SPC,
     GRP,
     FISH,
     IIF(ISNULL([FN127].[AGEID]),1,[FN127].[AGEID]) AS AGEID,
     AGEA,
     'FALSE' AS PREFERRED,
     AGEMT,
     CONF,
     '' AS NCA,
     EDGE,
     F7 AS COMMENT7,
     '' AS AGESTRM,
     '' AS AGELAKE,
     '' AS SPAWNCHKCNT,
     IIF(ISNULL([AGEA]),1,NULL) AS AGE_FAIL
       FROM FN127
     WHERE PRJ_CD='%s'
     ORDER BY PRJ_CD, TRIM(STR([FN127].[SAM])), EFF, SPC, GRP, FISH,
      IIF(ISNULL([FN127].[AGEID]),1,[FN127].[AGEID]);
     "

  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(stmt, src_db)
  return(dat)
}

##' Fetch questions data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' question data in a format that matches the question table in the upload
##' template.
##' @title Fetch Creel question data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the question data for the specified
##'   creel
##' @author R. Adam Cottrill
get_creesys_questions <- function(prj_cd, src_db) {
  sql <- "SELECT PRJ_CD, ANG_OP AS QUESTION_NUMBER, ANG_QUES AS QUESTION_TEXT
     FROM OPTIONQ
     WHERE PRJ_CD='%s';"


  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(stmt, src_db)
  return(dat)
}


##' Fetch answers data from Creesys Database
##'
##' This function will connect to the source database and extract the
##' answer data in a format that matches the answer table in the upload
##' template.
##' @title Fetch Creel answer data from Creesys Database
##' @param prj_cd - the project code of creel to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the answer data for the specified
##'   creel
##' @author R. Adam Cottrill
get_creesys_answers <- function(prj_cd, src_db) {
  # a function replace the Get_FN126 query from the mapper database.

  sql <- "SELECT PRJ_CD, ANG_OP AS QUESTION_NUMBER, ANG_AN AS ANSWER_NUMBER, ANG_ANS AS ANSWER_TEXT
     FROM OPTIONA
     WHERE PRJ_CD='%s';
     "
  stmt <- format_prj_cd_sql(sql, prj_cd)
  dat <- fetch_sql(stmt, src_db)
  return(dat)
}





##' Standarize daytype encoding
##'
##' This function will switch the encoding used to indicate week-days
##' and weekends in a creel.  These were left to the discression of
##' the project lead, and as a result are not consisten across all
##' creels.  This is a wrapper function that will switch the day-type
##' values in the FN023, FN024 and F025 tables, and update the strata
##' fields in the FN111 and F121 tables.
##' @title Standarize daytype encoding
##' @param trg_db  - path to the accdb with the template database that is
##'   being populated.
##' @return NULL
##' @author R. Adam Cottrill
switch_and_update_dtp <- function(trg_db) {
  check_accdb(trg_db)
  con <- RODBC::odbcConnectAccess2007(trg_db, uid = "", pwd = "", case = "nochange")
  switch_dtp(con, "FN023")
  switch_dtp(con, "FN024")
  switch_dtp(con, "FN025", "DTP1")
  update_stratum_dtp(con, "FN111")
  update_stratum_dtp(con, "FN121")
  RODBC::odbcClose(con)
}


##' Switch DTP value in target table
##'
##' this function will execute an update statement that will switch
##' the values of DTP in the specified table.  The field argument is
##' used to generalize the function to work with FN025 table too.
##' @title Switch DTP value in target table
##' @param con - an open ODBC connection to the target database.
##' @param table the table to run the update query against.
##' @param field - the field to change (DTP for the FN023 and FN024
##'   tables. DTP1 for the FN025 table)
##' @return NULL
##' @author R. Adam Cottrill
switch_dtp <- function(con, table, field = "DTP") {
  stmt <- "update [%s] set [%s]= iif([%s]='1', '2', '1')"
  sql <- sprintf(stmt, table, field, field)
  print(sprintf("updating %s", table))
  RODBC::sqlQuery(con, sql)
}





##' Update the DTP value in the Stratum of the target table
##'
##' This function will execute an update statement that will switch
##' the values of DTP in stratum field of the specified table.
##' @title Update the DTP value in the Stratum field
##' @param con - an open ODBC connection to the target database.
##' @param table the table to run the update query against.
##' @return NULL
##' @author R. Adam Cottrill
update_stratum_dtp <- function(con, table) {
  stmt <- "update [%s] set stratum =
IIf(Mid([stratum],4,1)=1,
Left([stratum],3) & '2' & Right([stratum],7),
Left([stratum],3) & '1' & Right([stratum],7))
"
  sql <- sprintf(stmt, table)
  print(sprintf("updating %s", table))
  RODBC::sqlQuery(con, sql)
}
