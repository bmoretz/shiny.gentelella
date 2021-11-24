#' @title Element
#'
#' R6 Class representing a UI Element
#'
#' @description
#'
#' This is the abstract base class for all UI
#' elements.
#'
#' @details
#' An object that is meant to be the base type for
#' all user-interface elements. The primary driver
#' for having a base type for all UI elements is
#' that every ui should have a unique id for
#' shiny module dispatch, and basic descriptors
#' that assist with robust logging.
#'
#' @docType class
#' @family Controls
#' @export
#' @importFrom uuid UUIDgenerate
#' @importFrom R6 R6Class
#' @importFrom stringr str_detect str_remove
#' @importFrom logger log_layout
Element <- R6::R6Class(
  classname = "Element",
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @return A new `Element` object.
    initialize = function() {

      cls_name <- private$get_classname()

      if(cls_name == "Element")
        abstract_class_error(cls_name, call = "initialize")

      private$id <- uuid::UUIDgenerate()
      private$log_config <- LogConfig$new()
      private$cls_name <- cls_name
    },

    #' @description
    #' Get accessor for (top-level) class name.
    #' @returns top-level class name
    class_name = function() {
      private$cls_name
    },

    #' @description
    #' Get accessor for object's uuid.
    #' @returns element's uuid
    identifier = function() {
      private$id
    }

  ),
  private = list(

    id = NULL,
    cls_name = NULL,
    log_config = NULL,

    set_logging = function() {
      cls_layout <- private$log_config$generate_layout(self)
      logger::log_layout(cls_layout)
      invisible(self)
    },

    get_classname = function() {
      calls <- as.character(sys.calls())
      calls <- calls[max(which(stringr::str_detect(calls, "\\$new\\(.*\\)")))]
      stopifnot(length(calls) == 1)
      invisible(stringr::str_remove(calls, "\\$new\\(.*\\)"))
    }
  )
)
