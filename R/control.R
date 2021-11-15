#' R6 Class representing a UI Control
#'
#' An object that is meant to be the base type for
#' all ui controls. Controls are meant to represent
#' self-contained functional units (e.g., charts,
#' menus, tables, etc.).
#'
#' @export
Control <- R6::R6Class(
  classname = "Control",
  inherit = Element,
  public = list(
    #' @description
    #' Create a new UI Control object.
    #' @return A new `Control` object.
    initialize = function() {
      super$initialize()
    },

    log_layout = function() {
      logger::layout_glue_generator(
        format = paste0(private$log_context, '/',
                        '{fn} - {time} {level}: {msg}\n'))
    }
  ),
  private = list(
  )
)
