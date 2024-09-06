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
nearshore_to_template <- function(prj_cds, src_dbase, template_db, fname = NA, lake = "HU", verbose = FALSE, overwrite = FALSE) {
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

  gear_effort_process_types <- gear_effort_process_types[gear_effort_process_types$GR %in% fn028$GR, ]
  cat(sprintf("\tgear_effort_process_types records: %s\n", nrow(gear_effort_process_types)))
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
  fn121 <- fn121_populate_process_type(fn028, fn121, fn122, gear_effort_process_types)
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
    fn125_tags <- fn125_tags[with(
      fn125_tags,
      order(PRJ_CD, SAM, EFF, SPC, GRP, FISH, FISH_TAG_ID)
    ), ]
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
    fn125_ages$AGEMT <- ifelse(!is.na(fn125_ages$AGEMT.x), fn125_ages$AGEMT.x, fn125_ages$AGEMT.y)
    fn125_ages <- fn125_ages[, !(names(fn125_ages) %in% c("XAGEM", "AGEMT.x", "AGEMT.y"))]
  } else {
    msg <- "Unable to find 'xagem2agemt.csv' skipping FN125 agemt updates."
    message(msg)
    fn125_ages$XAGEM <- NULL
  }

  fn127 <- get_nearshore_fn127(prj_cds, src_dbase)

  fn127 <- rbind(fn125_ages, fn127)
  cat(sprintf("\tSC127 records: %s\n", nrow(fn127)))
  if (nrow(fn127)) {
    fn127 <- fn127[with(fn127, order(PRJ_CD, SAM, EFF, SPC, GRP, FISH, AGEID)), ]
    fn127$AGEMT[is.na(fn127$AGEMT)] <- "99999"
    append_data(trg_db, "FN127", fn127, verbose = verbose)
  }

  populate_readme(trg_db, src_dbase)

  msg <- paste0(
    sprintf(
      "Done. The populated database can be found here: \n\t%s.  \n", trg_db
    ),
    "You should be able to check it with Process Validate and upload it to the assessment portal.\n"
  )
  message(msg)
}



##' Fetch FN011 data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' FN011 data in a format that matches the FN011 table in the upload
##' template.
##' @title Fetch FN011 data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN011 data for the specified
##'   assessment project
##' @author R. Adam Cottrill
get_nearshore_fn011 <- function(prj_cds, src_db) {
  # a function replace the Get_FN011 query from the mapper database.

  sql <- "SELECT YEAR, PRJ_CD, PRJ_NM, PRJ_LDR, PRJ_DATE0, PRJ_DATE1,
          COMMENT0, PROTOCOL
          FROM IA011
          WHERE PRJ_CD in (%s)
          ORDER BY Year, PRJ_CD;"

  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )
  dat <- fetch_sql(stmt, src_db)
  return(dat)
}



##' Fetch FN022 data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' FN022 data in a format that matches the FN022 table in the upload
##' template.
##' @title Fetch FN022 data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN022 data for the specified
##'   assessment project
##' @author R. Adam Cottrill
get_nearshore_fn022 <- function(prj_cds, src_db) {
  # a function replace the Get_FN022 query from the mapper database.


  sql <- "SELECT PRJ_CD, '00' AS SSN, 'COMING SOON' AS SSN_DES,
           MIN(EFFDT0) AS SSN_DATE0, MAX(EFFDT1) AS SSN_DATE1
           FROM IA121 GROUP BY PRJ_CD, '00', 'COMING SOON'
          HAVING PRJ_CD in (%s);"

  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )
  dat <- fetch_sql(stmt, src_db)
  return(dat)
}



##' Fetch FN026 data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' FN026 data in a format that matches the FN026 table in the upload
##' template.
##' @title Fetch FN026 data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN026 data for the specified
##'   assessment project
##' @author R. Adam Cottrill
get_nearshore_fn026 <- function(prj_cds, src_db) {
  # a function replace the Get_FN026 query from the mapper database.

  sql <- "SELECT PRJ_CD,
          '00' AS [SPACE],
          'Space is ...' AS SPACE_DES,
          Avg(IA121.DD_LON) AS DD_LON,
          Avg(IA121.DD_LAT) AS DD_LAT,
          Int(Min([SIDEP])) AS SIDEP_GE,
          Int(Max([SIDEP])) + 1 AS SIDEP_LT,
          Int(Min([GRDEP])) AS GRDEP_GE,
          Int(Max([SIDEP])) + 1 AS GRDEP_LT,
          '' as SPACE_WT
          FROM IA121
          GROUP BY PRJ_CD, '00', 'Space is ...'
          HAVING PRJ_CD in (%s);
          "
  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(stmt, src_db)
  return(dat)
}





##' Fetch FN026_subspace data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' FN026_subspace data in a format that matches the FN026_subspace
##' table in the upload template.
##' @title Fetch FN026_subspace data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN026_subspace data for the
##'   specified assessment project
##' @author R. Adam Cottrill
get_nearshore_fn026_subspace <- function(prj_cds, src_db) {
  # a function replace the Get_FN026_subspace query from the mapper database.

  sql <- "SELECT PRJ_CD,
          '00' AS [SPACE],
          '11' as [SUBSPACE],
          'Subspace is ...' AS SUBSPACE_DES,
          Avg(IA121.DD_LON) AS DD_LON,
          Avg(IA121.DD_LAT) AS DD_LAT,
          Int(Min([SIDEP])) AS SIDEP_GE,
          Int(Max([SIDEP])) + 1 AS SIDEP_LT,
          Int(Min([GRDEP])) AS GRDEP_GE,
          Int(Max([SIDEP])) + 1 AS GRDEP_LT,
          1 as SUBSPACE_WT
          FROM IA121
          GROUP BY PRJ_CD
          HAVING PRJ_CD in (%s);
          "
  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(stmt, src_db)
  return(dat)
}




##' Fetch FN028 data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' FN028 data in a format that matches the FN028 table in the upload
##' template.
##' @title Fetch FN028 data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN028 data for the specified
##'   assessment project
##' @author R. Adam Cottrill
get_nearshore_fn028 <- function(prj_cds, src_db) {
  # a function replace the Get_FN028 query from the mapper database.

  sql <- "SELECT PRJ_CD,
          '01' AS MODE,
          GR,
          IIf(IsNull([IA121].[GRUSE]),'9',[IA121].[GRUSE]) AS GRUSE,
          IIf(IsNull([IA121].[ORIENT]),'9',[IA121].[ORIENT]) AS ORIENT,
          'Gear: ' & [GR] & ', Orient: ' & [ORIENT] & ', Gear use:' & IIf(IsNull([IA121].[GRUSE]),'9',[IA121].[GRUSE]) AS MODE_DES,
          Min(Round([EFFDUR],1)) AS EFFDUR_GE,
          Max(Int([EFFDUR])+1) AS EFFDUR_LT,
          Min(TimeSerial(Hour([EFFTM0]),0,0)) AS EFFTM0_GE,
          Max(TimeSerial(Hour([EFFTM0])+1,0,0)) AS EFFTM0_LT
          FROM IA121
          GROUP BY
          PRJ_CD,
          GR,
          IIf(IsNull([IA121].[GRUSE]),'9',[IA121].[GRUSE]),
          IIf(IsNull([IA121].[ORIENT]),'9',[IA121].[ORIENT]),
          'Gear: ' & [GR] & ', Orient: ' & [ORIENT] & ', Gear use:' & IIf(IsNull([IA121].[GRUSE]),'9',[IA121].[GRUSE])
          HAVING PRJ_CD in (%s);"
  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(stmt, src_db)
  return(dat)
}



##' Fetch FN121 data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' FN121 data in a format that matches the FN121 table in the upload
##' template.
##' @title Fetch FN121 data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN121 data for the specified
##'   assessment project
##' @author R. Adam Cottrill
get_nearshore_fn121 <- function(prj_cds, src_db) {
  # a function replace the Get_FN121 query from the mapper database.

  sql <- "SELECT
            PRJ_CD,
            Trim(Str([IA121].[SAM])) AS SAM,
            EFFDT0,
            EFFTM0,
            EFFDT1,
            EFFTM1,
            EFFDUR,
            EFFST,
            GR,
            IIf(IsNull([IA121].[GRUSE]),'9',[IA121].[GRUSE]) AS GRUSE,
            IIf(IsNull([IA121].[ORIENT]),'9',[IA121].[ORIENT]) AS ORIENT,
            SIDEP as SIDEP0,
            GRID AS GRID5,
            DD_LAT AS DD_LAT0,
            DD_LON AS DD_LON0,
            IIf([IA121].[DD_LAT1]=0,Null,[IA121].[DD_LAT1]) AS DD_LAT1,
            IIf([IA121].[DD_LON1]=0,Null,[IA121].[DD_LON1]) AS DD_LON1,
            Secchi as SECCHI0,
            COMMENT1,
            GRDEPMIN,
            XGRDEPMID as GRDEPMID,
            GRDEPMAX,
            XANGLE AS LEAD_ANGLE,
            XLEADUSE AS LEADUSE,
            XDISTOFF AS DISTOFF,
            SITEM as SITEM0,
            AIRTEM0,
            AIRTEM1,
            WIND AS WIND0,
            PRECIP AS  PRECIP0,
            CLOUD_PC AS CLOUD_PC0,
            XWAVEHT AS  WAVEHT0,
            XWEATHER,
            SITP,
            CREW,
            xslime as SLIME,
            '' as SECCHI1,
            '' as WIND1,
            '' as SITEM1,
            '' as SIDEP1,
            '' as PRECIP1,
            '' as CLOUD_PC1,
            '' as WAVEHT1,
            '' as VESSEL,
            '' as VESSEL_DIRECTION,
            '' as VESSEL_SPEED,
            '' as WARP,
            '' as BOTTOM,
            '' as COVER,
            '' as VEGETATION,
            '' as O2BOT0,
            '' as O2BOT1,
            '' as O2SURF0,
            '' as O2SURF1,
            '' as O2GR0,
            '' as O2GR1,
            IIf([GR] In ('GL51','GL38','GL64','NA12','ON22'),'3',
                IIf([GRTP]='GL' Or Left([GR],2)='GL','2','1')) AS PROCESS_TYPE
            FROM IA121
          WHERE PRJ_CD in (%s)
          ORDER BY
            PRJ_CD,
            Trim(Str([IA121].[SAM]));"

  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(stmt, src_db)
  return(dat)
}






##' Fetch FN122 data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' FN122 data in a format that matches the FN122 table in the upload
##' template.
##' @title Fetch FN122 data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN122 data for the specified
##'   assessment project
##' @author R. Adam Cottrill
get_nearshore_fn122 <- function(prj_cds, src_db) {
  # a function replace the Get_FN122 query from the mapper database.

  sql <- " SELECT PRJ_CD, Trim(Str([ia122].[SAM])) AS SAM, EFF, EFFDST,
           GRDEP as GRDEP0,
           '' as GRDEP1,
           GRTEM0, GRTEM1, 'FALSE' AS WATERHAUL, '' AS COMMENT2
           FROM IA122
           WHERE PRJ_CD in (%s)
           ORDER BY PRJ_CD, Trim(Str([ia122].[SAM])), EFF;
          "
  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(stmt, src_db)
  return(dat)
}



##' Fetch FN123 data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' FN123 data in a format that matches the FN123 table in the upload
##' template.
##' @title Fetch FN123 data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN123 data for the specified
##'   assessment project
##' @author R. Adam Cottrill
get_nearshore_fn123 <- function(prj_cds, src_db) {
  # a function replace the Get_FN123 query from the mapper database.

  sql <- "SELECT PRJ_CD, Trim(Str([ia123].[SAM])) AS SAM, EFF, SPC, GRP, CATCNT,
           BIOCNT, SUBCNT, SUBWT, COMMENT3, '' AS CATWT
           FROM IA123
           WHERE PRJ_CD in (%s)
           ORDER BY PRJ_CD, Trim(Str([ia123].[SAM])), EFF, SPC, GRP;
           "
  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(stmt, src_db)
  return(dat)
}





##' Fetch FN125 data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' FN125 data in a format that matches the FN125 table in the upload
##' template.
##' @title Fetch FN125 data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN125 data for the specified
##'   assessment project
##' @author R. Adam Cottrill
get_nearshore_fn125 <- function(prj_cds, src_db) {
  # a function replace the Get_FN125 query from the mapper database.

  sql <- "
      SELECT PRJ_CD,
      Trim(Str([ia125].[SAM])) AS SAM,
      EFF,
      SPC,
      GRP,
      FISH,
      FLEN,
      TLEN,
      RWT,
      SEX,
      MAT,
      GON,
      GONWT,
      CLIPC,
      GIRTH,
      AGEST,
      NODA,
      NODC,
      IIf(IsNull([ia125].[fate]),'K',[ia125].[fate]) AS FATE,
      COMMENT5,
      CLIPA,
      TISSUE,
      '' AS EVISWT,
      '' AS FDSAM,
      '' AS STOM_CONTENTS_WT
      FROM IA125
      WHERE PRJ_CD in (%s)
      ORDER BY PRJ_CD, Trim(Str([ia125].[SAM])), EFF, SPC, GRP, FISH;"
  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(stmt, src_db)
  return(dat)
}



##' Fetch FN125_tags data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' FN125_tags data in a format that matches the FN125_tags table in the upload
##' template.
##' @title Fetch FN125_tags data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN125_tags data for the specified
##'   assessment project
##' @author R. Adam Cottrill
get_nearshore_fn125_tags <- function(prj_cds, src_db) {
  # a function replace the Get_FN125 query from the mapper database.

  sql <- "SELECT PRJ_CD, SAM, EFF, SPC, GRP, FISH,
          1 AS FISH_TAG_ID,
          TAGID,
          TAGDOC,
          TAGSTAT,
          [xcwtseq] AS CWTSEQ,
          '' AS COMMENT_TAG
          FROM IA125
          WHERE PRJ_CD in (%s)
          AND TAGID Is Not Null
          And TAGID<>'0';"

  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(stmt, src_db)
  return(dat)
}


get_nearshore_fn125_xtags <- function(prj_cds, src_db) {
  # to fetch any tag data contained in XTAGID fields.  A warning will
  # be printed if this query returns any resutls because TAGDOC and
  # TAGSTAT will have to be verified in the populated template:
  sql <- "SELECT PRJ_CD, SAM, EFF, SPC, GRP, FISH,
          1 AS FISH_TAG_ID,
          XTAGID AS TAGID,
          TAGDOC,
          TAGSTAT,
          [XCWTSEQ] AS CWTSEQ,
          '' AS COMMENT_TAG
          FROM IA125
          WHERE PRJ_CD in (%s)
          AND XTAGID Is Not Null
          And XTAGID<>'0';"

  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(stmt, src_db)
  return(dat)
}




##' Fetch FN125_lamprey data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' lamprey data from the FN125 table in a format that can be
##' processed to matches the FN125_lampreys table in the upload
##' template.
##' @title Fetch FN125 data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN125 data for the specified
##'   assessment project
##' @author R. Adam Cottrill
get_nearshore_fn125_lamprey <- function(prj_cds, src_db) {
  # a function replace the Get_FN125_tags query from the mapper database.

  sql <- "SELECT PRJ_CD, Trim(Str([IA125].[SAM])) AS SAM, EFF, SPC, GRP,
          FISH, 1 AS LAMID, LAMIJC, XLAM, COMMENT5 AS COMMENT_LAM
          FROM IA125
          WHERE PRJ_CD in (%1$s) AND LAMIJC Is Not Null OR
          PRJ_CD in (%1$s) AND XLAM Is Not Null;"

  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(stmt, src_db)
  return(dat)
}


##' Fetch FN126 data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' FN126 data in a format that matches the FN126 table in the upload
##' template.  The query used in this function includes a union
##' statement to extract some diet data that is incorrectly stored in
##' the FN125 data for a small number of projects.
##'
##'
##' @title Fetch FN126 data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN126 data for the specified
##'   assessment project
##' @author R. Adam Cottrill
get_nearshore_fn126 <- function(prj_cds, src_db) {
  # a function replace the Get_FN126 query from the mapper database.

  sql <- "SELECT IA125.PRJ_CD,
          Trim(Str([IA125].[SAM])) AS SAM,
          IA125.EFF,
           IA125.SPC,
           IA125.GRP,
           IA125.FISH,
           1 AS FOOD,
           IA125.TAXON,
           IA125.FDCNT,
           '' AS FDMES,
           '' AS FDVAL,
           '' AS LIFESTAGE,
           '' AS COMMENT6
          FROM IA125
          WHERE IA125.PRJ_CD in (%1$s)
          AND IA125.TAXON Is Not Null
          UNION ALL
          SELECT IA126.PRJ_CD,
           Trim(Str([IA126].[SAM])) AS SAM,
           IA126.EFF,
           IA126.SPC,
           '00' AS GRP,
           IA126.FISH,
           IA126.FOOD,
           IA126.TAXON,
           IA126.FDCNT,
           '' AS FDMES,
           '' AS FDVAL,
           '' AS LIFESTAGE,
           '' AS COMMENT6
          FROM IA126
          WHERE IA126.PRJ_CD in (%1$s)
          "

  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(stmt, src_db)

  dat <- dat[with(dat, order(PRJ_CD, SAM, EFF, SPC, GRP, FISH, FOOD)), ]
  return(dat)
}




##' Fetch FN125 Age data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' age data from the FN125 table in a format that matches the FN127
##' table in the upload template.  It automatically assigns
##' preferred=TRUE and sets ageid=125.
##' @title Fetch FN126 data from Nearshore Database
##' @param  prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN126 data for the specified
##'   assessment project
##' @author R. Adam Cottrill
get_nearshore_fn125_ages <- function(prj_cds, src_db) {
  sql <- "select
         PRJ_CD,
         Trim(Str([IA125].[SAM])) as SAM,
         EFF,
         SPC,
         GRP,
         FISH,
         125 AS AGEID,
         AGE as AGEA,
         'TRUE' AS PREFERRED,
         AGEMT,
         XAGEM,
         CONF,
         NCA,
         EDGE,
         '' as COMMENT7,
          '' as AGESTRM,
          '' as AGELAKE,
          '' as SPAWNCHKCNT,
          IIf(
             isnull(
                 [AGEA]
             ),
             1,
             NULL
         ) as AGE_FAIL
     from
         IA125
     where PRJ_CD in (%s) and  XAGEM is not null
     order by
         PRJ_CD,
         Trim(Str([IA125].[SAM])),
         EFF,
         SPC,
         GRP,
         FISH;
     "

  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(stmt, src_db)

  return(dat)
}

##' Fetch FN127 data from Nearshore Database
##'
##' This function will connect to the source database and extract the
##' FN127 data in a format that matches the FN127 table in the upload
##' template.
##' @title Fetch FN127 data from Nearshore Database
##' @param prj_cds - the project code(s) of assessment project(s) to export
##' @param src_db - path to the accdb with the src data
##' @return - dataframe containing the FN127 data for the specified
##'   assessment project
##' @author R. Adam Cottrill
get_nearshore_fn127 <- function(prj_cds, src_db) {
  # a function replace the Get_FN126 query from the mapper database.

  sql <- "SELECT
     PRJ_CD,
     TRIM(STR([IA127].[SAM])) AS SAM,
     EFF,
     SPC,
     GRP,
     FISH,
     IIF(ISNULL([IA127].[AGEID]),1,[IA127].[AGEID]) AS AGEID,
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
       FROM IA127
     WHERE PRJ_CD in (%s)
     ORDER BY PRJ_CD, Trim(Str([IA127].[SAM])), EFF, SPC, GRP, FISH,
      IIf(IsNull([IA127].[ageid]),1,[IA127].[ageid]);
     "

  project_codes <- paste(sapply(prj_cds, sQuote), collapse = ", ")

  stmt <- format_prj_cd_sql(
    sql,
    project_codes
  )

  dat <- fetch_sql(stmt, src_db)

  return(dat)
}
