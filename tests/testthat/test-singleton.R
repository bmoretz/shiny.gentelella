  TestSingleton <- R6::R6Class(
    classname = "TestSingleton",
    inherit = Singleton,

    public = list(

      initialize = function() {
        super$initialize()
      },

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

  test_that("public_identical", {
    inst_a <- TestSingleton$new(); inst_b <- TestSingleton$new()

    inst_a_pub <- inst_a$get_public_bindings()
    inst_b_pub <- inst_b$get_public_bindings()

    expect_true(identical(inst_a_pub, inst_b_pub))
  })

  test_that("private_identical", {
    inst_a <- TestSingleton$new(); inst_b <- TestSingleton$new()

    inst_a_priv <- inst_a$get_private_bindings()
    inst_b_priv <- inst_b$get_private_bindings()

    expect_true(identical(inst_a$get_private_bindings(),
                          inst_b$get_private_bindings()))
  })

  test_that("singleton_instance", {
    inst_a <- TestSingleton$new(); inst_b <- TestSingleton$new()

    expect_true(identical(inst_a, inst_b))
  })

