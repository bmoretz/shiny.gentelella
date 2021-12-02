TestElement <- R6::R6Class(
  classname = "TestElement",
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
    element <- TestElement$new()
  }, class = "is_abstract_error")
})

test_that("has_correct_classname", {
  tester <- TestElement$new()

  cls_name <- tester$class_name()

  expect_equal(cls_name, "TestElement")
})

test_that("element_log_layout", {
  tester <- TestElement$new()

  # layout <- tester$get_log_layout()
  # fn_context <- function() {
  #   tester$trace("test: {test_var}")
  # }
  #
  # fn_context()

  # expect_equal(cls_name, "TestElement")
})

test_that("can_log_trace", {
  # tester <- TestElement$new()
  #
  # tester$trace("test")
  #
  # expect_equal(cls_name, "TestElement")
})

test_that('element_logs_formatted', {

})
