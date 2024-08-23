library(mockr)
# mymock <- function(x, y) {y}

local({
  local_mock(get_trg_table_names = function(trg_db, table) c("FOO", "BAR", "BAZ"))

  test_that("check_table_names reports extra field names", {
    msg <- "The source data frame has extra fields: BETA"
    df <- data.frame(
      "FOO" = character(),
      "BAR" = character(),
      "BAZ" = character(),
      "BETA" = character()
    )

    expect_warning(
      diff <- check_table_names("fakedb.accdb", "FN011", df),
      msg
    )
    expect_equal(diff, c("BETA"))
  })


  test_that("check_table_names report missing field names", {
    msg <- "The source data frame is missing fields: BAR"
    df <- data.frame("FOO" = character(), "BAZ" = character())

    expect_warning(
      diff <- check_table_names("fakedb.accdb", "FN011", df),
      msg
    )
    expect_equal(diff, c("BAR"))
  })

  test_that("check_table_names reports nothing when names are equal", {
    df <- data.frame(
      "FOO" = character(),
      "BAR" = character(),
      "BAZ" = character()
    )

    expect_no_warning(
      diff <- check_table_names("fakedb.accdb", "FN011", df),
    )
    expect_equal(diff, character())
  })
})
