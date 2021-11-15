#' An object that is meant to be the base type for
#' all portal pages.
#' @export
Dashboard <- R6::R6Class(
  inherit = Element,
  public = list(

    #' @description Initialize a portal page.
    initialize = function() {
      super$initialize()

      private$nav_menu <- NavigationMenu$new()
    },

    #' @description Wrapper around fluid layout page.
    page = function() {
      return(list(
        ui = self$fluid_layout(),
        server = self$server()
      ))
    },

    #' @description Wrapper around fluid layout page.
    fluid_layout = function() {
      tagList(
        htmlTemplate(
          system.file("theme/templates/index.html"),
          title = self$title,
          # navigation_menu = self$nav_menu$ui(),
          # tab_menu = self$tabs(),
          main_container = self$content()
        )
      )
    }
  ),
  private = list(
    nav_menu = NULL
  )
)
