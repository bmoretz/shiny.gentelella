#' An object that is meant to be the base type for
#' all portal pages.
#' @export
Dashboard <- R6::R6Class(
  inherit = Page,
  public = list(

    #' @description Initialize a portal page.
    initialize = function() {
      super$initialize()

      private$nav_menu <- NavigationMenu$new(self$meta$nav_definition)
    },

    #' @description Wrapper around a dashboard templated
    #' layout.
    layout = function() {
      tagList(
        htmlTemplate(
          system.file("theme/templates/index.html",
                      package = "shiny.gentelella"),
          title = self$meta$title,
          navigation_menu = private$nav_menu$ui(),
          # tab_menu = self$tabs(),
          # main_container = self$content()
        )
      )
    }

  ),
  private = list(
    nav_menu = NULL
  )
)
