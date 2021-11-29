#' @title LogLayoutGenerator
#'
#' @description
#' R6 Class to encapsulate common logging configurations.
#'
#' @details
#' An object to encapsulate common logging  configurations,
#' formatters, generators, colorizers, etc. Designed to
#' be used via composition inside UI elements (pages,
#' controls, etc) as parent object is expected to have
#' UI Element interface (id, cls_name).
#'
#' @docType class
#' @family Logging
#' @importFrom crayon combine_styles make_style italic bold reset
#' @importFrom crayon silver magenta cyan
#' @export
LogLayoutGenerator <- R6::R6Class(
  classname = "LogLayoutGenerator",

  public = list(

    #' @description
    #' Creates a new instance of a log config.
    #' @return A new `LogLayoutGenerator` object.
    initialize = function() {
    },

    #' @description
    #' appends a metric to the log layout.
    #' @param ... layout style
    #' @param metric log metric
    #' @param env parent env
    #' @return A reference to self.
    add_metric = function(..., metric,
                      env = parent.frame()) {

      layout <- LogLayout$new()
      formatted <- layout$format_metric(..., metric, env)

      # private$format <- c(private$format, formatted)
      #
      # invisible(self)
    },

    #' @description
    #' returns the generated layout.
    #' @return generated layout.
    get_layout = function() {
      private$format
    }
  ),

  private = list(
    layout = NULL,
    format = list()
  )
)
