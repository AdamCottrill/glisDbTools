pts <- data.frame(
  SLUG = c("foo", "bar", "baz"),
  DD_LAT = c(45.4, 44.6, 44.2),
  DD_LON = c(-81.5, -82.3, -81.2)
)


## test_that("valid points are returned without error", {
##   expect_equal(pts, check_points(pts))
## })

test_that("missing lat issues warning", {
  pts$DD_LAT[1] <- NA
  expected <- pts[-1, ]
  msg <- "There was a problem with the following points:\n"
  expect_warning(obs <- check_points(pts), msg)
  expect_equal(expected, obs)
})



test_that("missing lon issues warning", {
  pts$DD_LON[1] <- NA
  expected <- pts[-1, ]
  msg <- "There was a problem with the following points:\n"
  expect_warning(obs <- check_points(pts), msg)
  expect_equal(expected, obs)
})


test_that("large lon issues warning", {
  pts$DD_LON[1] <- -65.5
  expected <- pts[-1, ]
  msg <- "There was a problem with the following points:\n"
  expect_warning(obs <- check_points(pts), msg)
  expect_equal(expected, obs)
})


test_that("positive lon issues warning", {
  pts$DD_LON[1] <- 81.1
  expected <- pts[-1, ]
  msg <- "There was a problem with the following points:\n"
  expect_warning(obs <- check_points(pts), msg)
  expect_equal(expected, obs)
})


test_that("very negative lon issues warning", {
  pts$DD_LON[1] <- -90.2
  expected <- pts[-1, ]
  msg <- "There was a problem with the following points:\n"
  expect_warning(obs <- check_points(pts), msg)
  expect_equal(expected, obs)
})


test_that("large lat issues warning", {
  pts$DD_LAT[1] <- 50.55
  expected <- pts[-1, ]
  msg <- "There was a problem with the following points:\n"
  expect_warning(obs <- check_points(pts), msg)
  expect_equal(expected, obs)
})


test_that("small lat issues warning", {
  pts$DD_LAT[1] <- 39.2
  expected <- pts[-1, ]
  msg <- "There was a problem with the following points:\n"
  expect_warning(obs <- check_points(pts), msg)
  expect_equal(expected, obs)
})
