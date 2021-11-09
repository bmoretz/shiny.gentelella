#' An object that is meant to be the base type for
#' all user-interface elements.
#' @export
UIElement <- R6::R6Class(
  public = list(
    #' @description Initialize ui element.
    initialize = function() {
      private$id <- uuid::UUIDgenerate()
    }
  ),
  private = list(
    id = NULL
  )
)
