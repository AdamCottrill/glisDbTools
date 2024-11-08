one_sample <- data.frame(
  PRJ_CD = c("LSA_IA20_123"),
  SAM = c("1"),
  DD_LAT0 = c(45.0),
  DD_LON0 = c(-81.0),
  GRID5 = c("1111"),
  grid5_slug = c("1111")
)

two_samples <- data.frame(
  PRJ_CD = c("LSA_IA20_123", "LSA_IA20_123"),
  SAM = c("1", "9"),
  DD_LAT0 = c(45.0, 44.0),
  DD_LON0 = c(-81.0, -82.0),
  GRID5 = c("2222", "3333"),
  grid5_slug = c("2222", "3333")
)

# there is one record it should report:
# "Found 1 FN121 record with both DD_LAT0 and DD_LON0."

test_that("reports record correctly if there is just one sample", {
  expected <- "Found 1 FN121 record with both DD_LAT0 and DD_LON0."
  expect_output(report_populate_grid5(one_sample, overwrite = T), expected)
})


# there 2 than one record it should report:
test_that("reports records correctly if more than one sample", {
  expected <- "Found 2 FN121 records with both DD_LAT0 and DD_LON0."
  expect_output(report_populate_grid5(two_samples, overwrite = T), expected)
})




# there is one unique point it should report:
# "Found 1 unique point."
test_that("reports one point correctly - even with two sams", {
  two_samples$DD_LAT0[2] <- two_samples$DD_LAT0[1]
  two_samples$DD_LON0[2] <- two_samples$DD_LON0[1]
  expected <- "Found 1 unique point."
  expect_output(report_populate_grid5(two_samples, overwrite = T), expected)
})

test_that("reports more than one point correctly", {
  expected <- "Found 2 unique points."
  expect_output(report_populate_grid5(two_samples, overwrite = T), expected)
})



# if there is only one record that is matched it should report:
#

test_that("reports one matched fn121 record", {
  two_samples$grid5_slug[2] <- NA
  expected <- "Matched 1 FN121 record to a grid5"
  expect_output(report_populate_grid5(two_samples, overwrite = T), expected)
})


test_that("reports more than one matched FN121 records", {
  expected <- "Matched 2 FN121 records to a grid5"
  expect_output(report_populate_grid5(two_samples, overwrite = T), expected)
})



# if there are 2 (or more ) records that are matched it should report:
#

# If there is a record where the GRID5 and grid5_slug values don't match
# it should report:
test_that("reports one matched fn121 record", {
  two_samples$grid5_slug[2] <- "ZZZZ"
  expected <- paste0(
    "There is 1 FN121 record that has an existing grid5 that is ",
    "not consistent with its lat-lon."
  )
  expect_output(report_populate_grid5(two_samples, overwrite = T), expected)
})

test_that("the mis-matched record should be reported", {
  two_samples$grid5_slug[2] <- "ZZZZ"
  sam_two <- two_samples[2, ]
  # each of the field values separated by one or more whitespaces (\\s+)
  expected <- with(sam_two, sprintf(
    "%s\\s+%s\\s+%s\\s+%s\\s+%s\\s+%s",
    PRJ_CD, SAM, DD_LAT0, DD_LON0, GRID5, grid5_slug
  ))
  expect_output(report_populate_grid5(two_samples, overwrite = T), expected)
})

#
#  The values of that row from the dataframe should also be reported:
# 1 LHA_IA22_821  91 43.2372 -81.9030 hu-3828       3828


# If there is a record where the GRID5 and grid5_slug values don't match
# it should report:
test_that("reports more than one mis-matched fn121 record", {
  two_samples$grid5_slug[1] <- "ZZZZ"
  two_samples$grid5_slug[2] <- "ZZZZ"
  expected <- paste0(
    "There are 2 FN121 records that have an existing grid5 that is ",
    "not consistent with their lat-lon."
  )
  expect_output(report_populate_grid5(two_samples, overwrite = T), expected)
})


test_that("the mis-matched records should be reported", {
  two_samples$grid5_slug[1] <- "ZZZZ"
  two_samples$grid5_slug[2] <- "ZZZZ"
  sam_one <- two_samples[1, ]
  sam_two <- two_samples[2, ]
  # each of the field values separated by one or more whitespaces (\\s+)

  expected <- with(sam_one, sprintf(
    "%s\\s+%s\\s+%s\\s+%s\\s+%s\\s+%s",
    PRJ_CD, SAM, DD_LAT0, DD_LON0, GRID5, grid5_slug
  ))
  expect_output(report_populate_grid5(two_samples, overwrite = T), expected)

  expected <- with(sam_two, sprintf(
    "%s\\s+%s\\s+%s\\s+%s\\s+%s\\s+%s",
    PRJ_CD, SAM, DD_LAT0, DD_LON0, GRID5, grid5_slug
  ))
  expect_output(report_populate_grid5(two_samples, overwrite = T), expected)
})


# if there is one mismatch and overwrite is TRUE, the report should include the statement:
test_that("one mis-match with overwrite = TRUE", {
  two_samples$grid5_slug[2] <- "ZZZZ"
  expected <- "It was updated to the predicted value."
  expect_output(report_populate_grid5(two_samples, overwrite = T), expected)
})


# if there is more than one mismatch and overwrite is TRUE, the report should include the statement:
# "They were updated to the predicted value."
test_that("two or more mis-matches with overwrite = TRUE", {
  two_samples$grid5_slug[1] <- "ZZZZ"
  two_samples$grid5_slug[2] <- "ZZZZ"
  expected <- "They were updated to their predicted values."
  expect_output(report_populate_grid5(two_samples, overwrite = T), expected)
})


# if there is one mismatch and overwrite is FALSE, the report should include the statement:
# "It was NOT updated. Set overwrite=TRUE to update it to the predicted value."
test_that("one mis-match with overwrite = TRUE", {
  two_samples$grid5_slug[2] <- "ZZZZ"
  expected <- "It was NOT updated. Set overwrite=TRUE to update it to the predicted value."
  expect_output(report_populate_grid5(two_samples, overwrite = F), expected)
})


# if there is more than one mismatch and overwrite is FALSE, the report should include the statement:
# "They were NOT updated. Set overwrite=TRUE to update them to their predicted values."
test_that("two or more mis-matches with overwrite = TRUE", {
  two_samples$grid5_slug[1] <- "ZZZZ"
  two_samples$grid5_slug[2] <- "ZZZZ"
  expected <- "They were NOT updated. Set overwrite=TRUE to update them to their predicted values."
  expect_output(report_populate_grid5(two_samples, overwrite = F), expected)
})
