#' R6 Class representing a UI Element
#'
#' An object that is meant to be the base type for
#' all user-interface elements. The primary driver
#' for having a base type for all UI elements is
#' that every ui should have a unique id for
#' shiny module dispatch, and basic descriptors
#' that assist with robust logging.
#'
#' @export
#' @importFrom uuid UUIDgenerate
Element <- R6::R6Class(
  public = list(

    #' @description
    #' Create a new UI Element object.
    #' @return A new `Element` object.
    initialize = function() {
      private$id <- uuid::UUIDgenerate()
      private$log_context <- paste0(class(self)[1], '/', private$id)
    }
  ),
  private = list(
    id = NULL,
    log_context = NULL
  )
)
