LogDispatchTester <- R6::R6Class(
  classname = "LogDispatchTester",
  inherit = LogDispatch,

  public = list(

    initialize = function() {
    },

    get_system_metrics = function() {
      private$system_context
    }

  ),
  private = list()
)

test_that("log_single_instance", {

  inst_n <- LogDispatch$new()
  inst_m <- LogDispatch$new()

  expect_true(identical(inst_n, inst_m))
})

test_that("has_system_metrics", {

  log <- LogDispatchTester$new()

  expect_named(log$system['sysname'])
  expect_gt(nchar(log$system['sysname']), 0)

  expect_named(log$system['release'])
  expect_gt(nchar(log$system['release']), 0)

  expect_named(log$system['version'])
  expect_gt(nchar(log$system['version']), 0)

  expect_named(log$system['nodename'])
  expect_gt(nchar(log$system['nodename']), 0)

  expect_named(log$system['machine'])
  expect_gt(nchar(log$system['machine']), 0)

  expect_named(log$system['login'])
  expect_gt(nchar(log$system['login']), 0)

  expect_named(log$system['user'])
  expect_gt(nchar(log$system['user']), 0)

  expect_named(log$system['r-ver'])
  expect_gt(nchar(log$system['r-ver']), 0)
})
