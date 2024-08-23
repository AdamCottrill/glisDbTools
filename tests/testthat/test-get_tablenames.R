test_that("get_tablenames stops for nonexistant file", {
  filename <- "foobar.accdb"
  message <- sprintf(
    paste0(
      "Could not find the database '%s'. ",
      "Make sure it exists and try again."
    ),
    filename
  )
  expect_error(get_tablenames(filename), message)
})


test_that("get_tablenames stops for bad extension", {
  filename <- "foobar.txt"
  message <- sprintf(
    paste0(
      "The provided filename '%s' ",
      "does not appear to be an MS access (*.accdb) file."
    ),
    filename
  )

  # not sure why we need to do this:
  err <- expect_error(get_tablenames(filename))
  expect_equal(err$message, message)
})
