#' @title Page
#'
#' @description
#' R6 Class representing a UI Page.
#'
#' @details
#' An object that is meant to be the base type for
#' all ui pages. Pages are meant to represent an
#' entire page in the UI as serves as a container
#' for controls.
#'
#' @export
Page <- R6::R6Class(
  classname = "Page",
  inherit = Element,
  public = list(

    #' @description
    #' Create a new UI Page object.
    #' @return A new `Page` object.
    initialize = function() {
      super$initialize()
    },

    #' @description Wrapper around fluid layout page.
    page = function() {
      return(list(
        ui = self$layout(),
        server = self$server()
      ))
    },

    #' @description server-side wrapper.
    #' @param input form input
    #' @param output render output
    #' @param session page session
    server = function(input, output, session) {}

  ),
  private = list()
)
