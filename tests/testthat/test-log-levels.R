test_that("standard_levels_accessible",{

  expect_false(is.null(FATAL))
  expect_false(is.null(ERROR))
  expect_false(is.null(WARN))
  expect_false(is.null(SUCCESS))
  expect_false(is.null(INFO))
  expect_false(is.null(DEBUG))
  expect_false(is.null(TRACE))
})
