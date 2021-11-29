ExampleDashboard <- R6::R6Class(
  classname = "ExampleDashboard",
  inherit = Dashboard,
  public = list(

    #' @field meta page attributes
    #' and configuration wrapper.
    meta = list(
      title = "Example Dashbaord",
      nav_definition = system.file("example",
                                   "navigation.yml",
                                   package = "shiny.gentelella")
    ),

    #' @description
    #' Create a new UI Page object.
    #' @return A new `ExampleDashboard` object.
    initialize = function() {
      super$initialize()
    },

    #' @description user-interface wrapper.
    ui = function() {
      self$layout()
    },

    #' @description server-side wrapper.
    #' @param input form input
    #' @param output render output
    #' @param session page session
    server = function(input, output, session) {

      private$set_logging()
      #log_trace("processing server events")
    }
  ),
  private = list()
)
