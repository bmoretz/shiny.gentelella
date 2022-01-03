#' @title Dashboard
#'
#' @include page.R
#'
#' R6 Class representing a Dashboard layout.
#'
#' @description
#' An object that is meant to represent a standardized
#' dashboard page layout with [NavigationMenu], Settings,
#' Summary and a customizable container for the main
#' content section.
#'
#' @references \code{\link{Page}}
#' @importFrom shiny htmlTemplate tagList
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
