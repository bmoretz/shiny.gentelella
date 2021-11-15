NavigationMenuTester <- R6::R6Class(
  classname = "NavigationMenuTester",
  inherit = NavigationMenu,

  public = list(
    initialize = function(nav_definition) {
      super$initialize(nav_definition)
    },

    test_parse_menu = function() {
      private$parse_navigation()
    }
  ),

  private = list()
)

mock_menu_definition <- system.file("test-data",
                                    "mock-nav-menu.yml",
                                    package = "shiny.gentelella")

mock_menu <- NavigationMenuTester$new(mock_menu_definition)

test_that("menu_file_loads", {
  actual <- mock_menu$test_parse_menu()

  expect_gt(length(actual), 0)
})

test_that("top_level_parses", {
  actual <- mock_menu$test_parse_menu()

  expect_equal(length(actual), 1)
})
