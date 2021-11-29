#' @title Application
#'
#' R6 Class representing the shiny application.
#'
#' @description
#'
#' The application instance.
#'
#' @details
#' This is a singleton (locked) instance that represents the
#' application. This variable is used to perform application
#' level (global) tasks, such as manage resources and
#' register pages for dispatch.
#'
#' @docType class
#' @family Application
#' @importFrom R6 R6Class
#' @export
Application <- R6::R6Class(
  classname = "Application",

  public = list(

    #' @description
    #' Creates a new instance of a log config.
    #' @return A new `Application` object.
    initialize = function() {
      private$resource_mgr <- ResourceMgr$new()

      thematic::thematic_shiny(font = "auto")
    }
  ),

  private = list(

    theme_dirs = c(
      "assets",
      "scripts",
      "images",
      "templates"
    ),

    resource_mgr = NULL,

    set_logging = function() {
      cls_layout <- private$log_config$generate_layout(self)
      #log_layout(cls_layout)
      #invisible(self)
    },

    map_theme_resources = function() {

      theme_dir <- system.file("theme")
    }
  )
)
