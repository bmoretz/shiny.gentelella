ElementTester <- R6::R6Class(
  classname = "ElementTester",
  inherit = Element,

  public = list(

    initialize = function() {
      super$initialize()
    },

    test_get_classname = function() {

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
