test_that("good project code is TRUE ", {
  expect_true(valid_prj_cd("LHA_IA09_005"))
})

test_that("good project codes are valid ", {
  expect_true(valid_prj_cd("LHA_IA09_005, LHA_IA09_006"))
})

test_that("good project code with quotes is TRUE ", {
  expect_true(valid_prj_cd("'LHA_IA09_005'"))
})

test_that("good project codes with quotes are valid ", {
  expect_true(valid_prj_cd("'LHA_IA09_005', 'LHA_IA09_006'"))
})




test_that("bad project code is TRUE ", {
  expect_false(valid_prj_cd("LH_IA09_005"))
})

test_that("bad project codes are valid ", {
  expect_false(valid_prj_cd("LH_IA09_005, LH_IA09_006"))
})

test_that("bad project code with quotes is TRUE ", {
  expect_false(valid_prj_cd("'LH_IA09_005'"))
})

test_that("bad project codes with quotes are valid ", {
  expect_false(valid_prj_cd("'LH_IA09_005', 'LH_IA09_006'"))
})
