NavigationMenuTester <- R6::R6Class(
  classname = "NavigationMenuTester",
  inherit = NavigationMenu,

  public = list(

    initialize = function(nav_definition) {
      super$initialize(nav_definition)
    },

    test_parse_navigation = function(nav_file) {
      private$parse_navigation(nav_file)
    },

    test_create_container = function(menu) {
      private$create_container(menu)
    },

    test_create_menu_sections = function(menu) {
      private$create_menu_sections(menu)
    },

    test_construct = function() {
      private$construct()
    },

    test_populate_dropdown = function(items) {
      private$populate_dropdown(items)
    },

    test_handle_dropdown = function(menu_item) {
      private$handle_dropdown(menu_item)
    },

    test_handle_static = function(menu_item) {
      private$handle_static(menu_item)
    },

    get_html = function() {
      private$html
    },

    get_definition = function() {
      private$nav_definition
    },

    set_logging = function() {
      private$set_logging()
    }
  ),

  private = list()
)

nav_definition <- system.file("test-data",
                              "mock-nav-menu.yml",
                              package = "shiny.gentelella")

test_that("menu_file_parses", {

  mock_menu <- NavigationMenuTester$new(nav_definition)
  mock_menu$set_logging()

  nav_file <- mock_menu$get_definition()
  expect_true(file.exists(nav_file))

  navigation <- mock_menu$test_parse_navigation(nav_file)

  expect_equal(class(navigation), 'list')
  expect_equal(length(navigation), 2)

  # menu sections
  general <- navigation[[1]]

  expect_equal(general$title, "General")

  # home drop-down
  home <- general$dropdown[[1]]

  expect_equal(length(home), 3)

  expect_equal(home$text, "Home")
  expect_equal(home$class, "fa-home")

  # home menu items
  home_menu_items <- home$items

  expect_equal(home_menu_items[[1]]$text, "Dashboard1")
  expect_equal(home_menu_items[[1]]$url, "index.html")

  expect_equal(home_menu_items[[2]]$text, "Dashboard2")
  expect_equal(home_menu_items[[2]]$url, "index2.html")

  expect_equal(home_menu_items[[3]]$text, "Dashboard3")
  expect_equal(home_menu_items[[3]]$url, "index3.html")

  live_on <- navigation[[2]]

  expect_equal(live_on$title, "LIVE ON")

  landing_page <- live_on$static[[1]]

  expect_equal(landing_page$text, "Landing Page")
  expect_equal(landing_page$class, "fa-laptop")

  comming_soon <- landing_page$item[[1]]

  expect_equal(comming_soon$text, "Coming Soon")
  expect_equal(comming_soon$url, "javascript:void(0)")
})

test_that("menu_generates", {

  mock_menu <- NavigationMenuTester$new(nav_definition)
  mock_menu$set_logging()

  parsed <- mock_menu$test_parse_navigation(nav_definition)

  html <- mock_menu$test_create_container(parsed)

  expect_equal(nchar(html), c(name = 3, attribs = 111, children = 1723))
})

test_that("menu_constructs", {

  mock_menu <- NavigationMenuTester$new(nav_definition)
  mock_menu$set_logging()

  mock_menu$test_construct()

  html <- mock_menu$get_html()

  expect_equal(nchar(html), c(name = 3, attribs = 111, children = 1723))
})

test_that("dropdown_menu_populates", {

  mock_menu <- NavigationMenuTester$new(nav_definition)
  mock_menu$set_logging()

  nav_file <- mock_menu$get_definition()
  expect_true(file.exists(nav_file))

  navigation <- mock_menu$test_parse_navigation(nav_file)

  dropdown <- navigation[[1]]$dropdown

  items <- dropdown[[1]]$items

  actual <- mock_menu$test_populate_dropdown(items)

  expect_equal(class(actual), 'list')
  expect_equal(length(actual), 3)

  expect_equal(actual[[1]]$name, 'li')
  expect_equal(actual[[1]]$attribs$class, "sub_menu")

  actual_link <- actual[[1]]$children[[1]]

  expect_equal(actual_link$name, 'a')
  expect_equal(actual_link$children[[1]], "Dashboard1")
})

test_that("static_menu_populates", {

  mock_menu <- NavigationMenuTester$new(nav_definition)
  mock_menu$set_logging()

  nav_file <- mock_menu$get_definition()
  expect_true(file.exists(nav_file))

  navigation <- mock_menu$test_parse_navigation(nav_file)

  static_item <- navigation[[2]]$static

  actual <- mock_menu$test_handle_static(static_item)

  expect_equal(class(actual), 'shiny.tag')
  expect_equal(length(actual), 3)

  expect_equal(actual$attribs$class, 'sub_menu')
})
