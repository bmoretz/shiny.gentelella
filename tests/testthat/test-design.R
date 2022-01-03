##########################################################
# get_class_name
##########################################################

Base <- R6::R6Class(
  classname = "Base",

  public = list(
    initialize = function() {
      private$.cls_name <- get_class_name()
    },

    class_name = function() {
      private$.cls_name
    }
  ),

  private = list(
    .cls_name = NULL
  )
)

Derived <- R6::R6Class(
  classname = "Derived",
  inherit = Base,

  public = list(),
  private = list()
)

base <- Base$new(); derived <- Derived$new()

test_that("class_name_works", {

  expect_equal(base$class_name(), "Base")
  expect_equal(derived$class_name(), "Derived")
})
