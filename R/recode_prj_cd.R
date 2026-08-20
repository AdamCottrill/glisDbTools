##' Build the name of target database
##'
##' This funciton attempt to build the name of the target database
##' from the provided arguments. A helper function that used by
##' several other funcitons in glisDbTools to isolate re-name logic in
##' a testable form. This funciton is not intented to be used directly
##' by most users.  If old project code appears in src_name it is
##' replaced with the new project code, otherwise the file is the new
##' project code followed by the accdb extension.  The returned file
##' names contains the complete path, using the directory of the src
##' db.
##' @title Build name of target database
##' @param src_db - the path to the source database
##' @param orig_prj_cd - the old project in the source database
##' @param new_prj_cd - the new project code that will be used to
##'   replace the original
##' @param trg_name - the (optional) name to be used for the target
##'   database. An 'accdb' extension is added if it is not included.
##' @return a string representing the path to the target database.
##' @author R. Adam Cottrill
build_trg_name <- function(src_db, orig_prj_cd, new_prj_cd, trg_name = NULL) {
  if (is.null(trg_name)) {
    fname <- basename(src_db)
    if (grepl(orig_prj_cd, src_db)) {
      trg_name <- gsub(orig_prj_cd, new_prj_cd, fname)
    } else {
      trg_name <- sprintf("%s.accdb", new_prj_cd)
    }
  } else {
    if (!grepl("\\.accdb$", trg_name)) trg_name <- paste0(trg_name, ".accdb")
  }

  trg_name <- gsub("/", "\\", trg_name)
  suppressWarnings(
    if (trg_name != normalizePath(trg_name)) {
      trg_name <- normalizePath(file.path(dirname(src_db), trg_name))
    }
  )
  return(trg_name)
}


##' Recode project code in template database
##'
##' This funciton will change the project code in a populated from one
##' project code to another.  The referential integrity that is build
##' into the GLIS template databases make it impossible to just change
##' projects in an adhoc fashion.
##' @title Re-code PRJ_CD in a Glis template
##' @param src_db - path to the populated glis template to be changed
##' @param orig_prj_cd - the original project code that needs to be
##'   changed
##' @param new_prj_cd - the new project code that will be used to
##'   replace the original project code
##' @param trg_name - the name of the new accdb with the re-coded
##'   project code.
##' @param overwrite - overwrite the target database (if it exists)?
##' @return NULL
##' @export
##' @author R. Adam Cottrill
recode_prj_cd <- function(
  src_db,
  orig_prj_cd,
  new_prj_cd,
  trg_name = NULL,
  overwrite = FALSE
) {
  # check the src_db
  check_accdb(src_db)
  # validate the project codes
  valid_prj_cd(orig_prj_cd)
  valid_prj_cd(new_prj_cd)

  if (orig_prj_cd == new_prj_cd) {
    msg <- sprintf(
      "The new project code (%s) cannot be same as the orig project code %s./n",
      new_prj_cd,
      orig_prj_cd
    )
    stop(msg)
  }

  trg_name <- build_trg_name(src_db, orig_prj_cd, new_prj_cd, trg_name)

  if (file.exists(trg_name) && !overwrite) {
    message_a <- sprintf("The target database: '%s' already exists.", trg_name)
    message_b <- "Please provide a different target or set overwrite=TRUE."
    stop(paste(message_a, message_b, sep = "\n"))
  } else {
    file.copy(src_db, trg_name, overwrite = overwrite)
  }

  tablenames <- get_tablenames(trg_name)

  # clear out the old data
  cat("Clearing data from:\n")
  for (i in length(tablenames):1) {
    table <- tablenames[i]
    payload <- clear_table_data(trg_name, table)
    cat(sprintf("\t%s\n", table))
  }

  skip <- c("")

  cat(sprintf("Fetching and inserting data into %s:\n", trg_name))
  # Get the new data
  for (table in tablenames) {
    if (!(table %in% skip)) {
      payload <- fetch_table_data(src_db, table, as.is = TRUE)
      if ("PRJ_CD" %in% names(payload)) {
        payload$PRJ_CD[payload$PRJ_CD == orig_prj_cd] <- new_prj_cd
      }
      cat(sprintf("\t%s: %s\n", table, nrow(payload)))
      if (nrow(payload)) {
        payload <- prep_date_time_fields(payload)
        append_data(trg_name, table, payload)
      }
    }
  }

  msg <- sprintf(
    "Recoded PRJ_CD from '%s' to '%s' on %s",
    orig_prj_cd,
    new_prj_cd,
    date()
  )

  update_readme(trg_name, msg)
}
