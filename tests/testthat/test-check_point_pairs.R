pts0 <- data.frame(
  SLUG = c("foo", "bar", "baz"),
  DD_LAT0 = c(45.4, 44.6, 44.2),
  DD_LON0 = c(-81.5, -82.3, -81.2)
)

pts1 <- data.frame(
  SLUG = c("foo", "bar", "baz"),
  DD_LAT1 = c(45.5, 44.5, 44.3),
  DD_LON1 = c(-81.2, -82.5, -81.3)
)

test_that("valid point pairs are returned without error or warning", {
  expected <- merge(pts0, pts1)
  expect_equal(expected, check_point_pairs(pts0, pts1))
})

test_that("missing lat0 throws warning", {
  pts0$DD_LAT0[1] <- NA
  pts <- merge(pts0, pts1, by = "SLUG")
  expected <- pts[!is.na(pts$DD_LAT0), ]
  msg <- "There was a problem with the following point pairs:\n"
  expect_warning(obs <- check_point_pairs(pts0, pts1), msg)
  expect_equal(expected, obs)
})


test_that("missing lon0 throws warning", {
  pts0$DD_LON0[1] <- NA
  pts <- merge(pts0, pts1, by = "SLUG")
  expected <- pts[!is.na(pts$DD_LON0), ]
  msg <- "There was a problem with the following point pairs:\n"
  expect_warning(obs <- check_point_pairs(pts0, pts1), msg)
  expect_equal(expected, obs)
})



test_that("missing lat1 throws warning", {
  pts1$DD_LAT1[1] <- NA
  pts <- merge(pts0, pts1, by = "SLUG")
  expected <- pts[!is.na(pts$DD_LAT1), ]
  msg <- "There was a problem with the following point pairs:\n"
  expect_warning(obs <- check_point_pairs(pts0, pts1), msg)
  expect_equal(expected, obs)
})


test_that("missing lon1 throws warning", {
  pts1$DD_LON1[1] <- NA
  pts <- merge(pts0, pts1, by = "SLUG")

  expected <- pts[!is.na(pts$DD_LON1), ]

  msg <- "There was a problem with the following point pairs:\n"
  expect_warning(obs <- check_point_pairs(pts0, pts1), msg)
  expect_equal(expected, obs)
})
