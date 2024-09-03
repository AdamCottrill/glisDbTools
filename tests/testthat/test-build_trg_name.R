test_that("original prj_cd in the filename is replaced with the new prj_cd", {
  src_db <- "C:\\1work\\Scrapbook\\build\\LHA_IS18_14L-FieldData.accdb"
  orig_prj_cd <- "LHA_IS18_14L"
  new_prj_cd <- "LHA_IA22_826"
  observed <- build_trg_name(src_db, orig_prj_cd, new_prj_cd)
  suppressWarnings(
    expected <- normalizePath(gsub(orig_prj_cd, new_prj_cd, src_db))
  )
  expect_equal(observed, expected)
})


test_that("new prj_cd is used if original is not in filename and not trg_name is provided", {
  src_db <- "C:\\1work\\Scrapbook\\build\\LHA_IS18_14L.accdb"
  orig_prj_cd <- "LHA_IA22_821"
  new_prj_cd <- "LHA_IA22_826"
  observed <- build_trg_name(src_db, orig_prj_cd, new_prj_cd)
  suppressWarnings(
    expected <- normalizePath(file.path(dirname(src_db), paste0(new_prj_cd, ".accdb")))
  )

  expect_equal(observed, expected)
})


test_that("the trg_name is used if one is provided.", {
  src_db <- "C:\\1work\\Scrapbook\\build\\LHA_IS18_14L.accdb"
  orig_prj_cd <- "LHA_IA22_821"
  new_prj_cd <- "LHA_IA22_826"
  trg_name <- "my_new_db.accdb"
  observed <- build_trg_name(src_db, orig_prj_cd, new_prj_cd, trg_name = trg_name)
  suppressWarnings(
    expected <- normalizePath(file.path(dirname(src_db), trg_name))
  )
  expect_equal(observed, expected)
})


test_that("the trg_name is used if one is provided accdb is added.", {
  src_db <- "C:\\1work\\Scrapbook\\build\\LHA_IS18_14L.accdb"
  orig_prj_cd <- "LHA_IA22_821"
  new_prj_cd <- "LHA_IA22_826"
  trg_name <- "my_new_db"
  observed <- build_trg_name(src_db, orig_prj_cd, new_prj_cd, trg_name = trg_name)
  suppressWarnings(
    expected <- normalizePath(file.path(dirname(src_db), paste0(trg_name, ".accdb")))
  )
  expect_equal(observed, expected)
})
