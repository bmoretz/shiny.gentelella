TestSingleton <- R6::R6Class(
  classname = "TestSingleton",
  inherit = Singleton,

  public = list(

    get_public_bindings = function() {
      private$public_bind_env
    },

    get_private_bindings = function() {
      private$private_bind_env
    }
  ),
  active = list(),
  private = list()
)

inst_a <- TestSingleton$new();

inst_a$get_public_bindings()

test_that("singleton_instance", {
  inst_a <- TestSingleton$new(); inst_b <- TestSingleton$new()

  expect_true(identical(inst_a, inst_b))
})

Obj <- R6::R6Class(
  classname = "Obj",

  public = list(
    initialize = function() {
      cls_name <- get_class_name()

      private$cls_name <- cls_name
    }
  ),

  private = list(
    cls_name = NULL
  )
)

obj <- Obj$new()

dynGet("public_bind_env", obj)
dynGet("active_bind_env", obj)
dynGet("private_bind_env", obj)

dynGet("Obj")

lapply(.bindings, function(binding) {
  binding$value <- dynGet(binding$key)
})
