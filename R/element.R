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
#' @importFrom uuid UUIDgenerate
#' @importFrom R6 R6Class
#' @importFrom stringr str_detect str_remove
#' @export
Element <- R6::R6Class(
  classname = "Element",
  public = list(

    #' @description
    #' Creates a new instance class.
    #' @return A new `Element` object.
    initialize = function() {

      cls_name <- get_class_name()

      if(cls_name == "Element")
        abstract_class_error(cls_name, call = "initialize")

      private$id <- uuid::UUIDgenerate()
      private$cls_name <- cls_name
      private$log_layout <- LogLayouts$generate_ui_layout(self)
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

  active = list(),

  private = list(

    id = NULL,
    cls_name = NULL,
    log_layout = NULL,

    set_logging = function() {
      cls_layout <- LogLayouts$generate_ui_layout(self)
      #log_layout(cls_layout)
      invisible(self)
    }
  )
)
