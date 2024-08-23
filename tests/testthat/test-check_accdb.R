test_that("check_accdb stops for nonexistant file", {
  filename <- "foobar.accdb"
  message <- sprintf(
    paste0(
      "Could not find the database '%s'. ",
      "Make sure it exists and try again."
    ),
    filename
  )
  expect_error(check_accdb(filename), message)
})


test_that("check_accdb stops for bad extension", {
  filename <- "foobar.txt"
  message <- sprintf(
    paste0(
      "The provided filename '%s' ",
      "does not appear to be an MS access (*.accdb) file."
    ),
    filename
  )

  # not sure why we need to do this:
  err <- expect_error(check_accdb(filename))
  expect_equal(err$message, message)
})


# mymock <- function(...) TRUE
#
# test_that("check_accdb returns true if accdb file exists", {
#   filename <- "foobar.accdb"
#   mockery::stub(mymock, file.exists, TRUE)
#   expect_true(check_accdb(filename))
# })
#
