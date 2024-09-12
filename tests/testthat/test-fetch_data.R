test_that("fetch_sql stops for nonexistant file", {
  filename <- "foobar.accdb"
  message <- sprintf(
    paste0(
      "Could not find the database '%s'. ",
      "Make sure it exists and try again."
    ),
    filename
  )
  sql <- "select * from foo;"

  expect_error(fetch_sql(filename, sql), message)
})


test_that("fetch_sql stops for bad extension", {
  filename <- "foobar.txt"
  message <- sprintf(
    paste0(
      "The provided filename '%s' ",
      "does not appear to be an MS access (*.accdb) file."
    ),
    filename
  )

  sql <- "select * from foo;"
  # not sure why we need to do this:
  err <- expect_error(fetch_sql(filename, sql))
  expect_equal(err$message, message)
})
