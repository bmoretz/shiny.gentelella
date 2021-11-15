#' @title Element
#'
#' @include control.R
#'
#' R6 Class representing a UI Element
#'
#' @description
#'
#' This is the abstract base class for [Control] and [Page].
#' [NavigationMenu] inherit from [Dashboard].
#'
#' An object that is meant to be the base type for
#' all user-interface elements. The primary driver
#' for having a base type for all UI elements is
#' that every ui should have a unique id for
#' shiny module dispatch, and basic descriptors
#' that assist with robust logging.
#'
#' @details
#' An Element is the base object for all UI objects.
#'
#' @docType class
#' @family Controls
#' @export
#' @importFrom uuid UUIDgenerate
#' @importFrom  R6 R6Class
#' @importFrom logger log_layout
Element <- R6::R6Class(
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @return A new `Element` object.
    initialize = function() {
      private$id <- uuid::UUIDgenerate()
      private$cls_name <- class(self)[1]
      private$log_config <- LogConfig$new()
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
    }
  )
)
