ElementTester <- R6::R6Class(
  classname = "ElementTester",
  inherit = Element,

  public = list(

    initialize = function() {
      super$initialize()
    },

    get_log_layout = function() {
      private$log_layout
    },

    test_get_classname = function() {
      private$cls_name
    }

  ),

  private = list()
)

test_that("is_abstract", {

  expect_error({
    element <- Element$new()
  }, class = "is_abstract_error")
})

test_that("has_correct_classname", {
  tester <- ElementTester$new()

  cls_name <- tester$class_name()

  expect_equal(cls_name, "ElementTester")
})

test_that("element_log_layout", {
  tester <- ElementTester$new()

  layout <- tester$get_log_layout()

  fn_context <- function() {
    tester$trace("test: {test_var}")
  }

  fn_context()

  expect_equal(cls_name, "ElementTester")
})

test_that("can_log_trace", {
  tester <- ElementTester$new()

  tester$trace("test")

  expect_equal(cls_name, "ElementTester")
})
