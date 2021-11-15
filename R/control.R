#' @title Control
#'
#' @include element.R
#'
#' R6 Class representing a UI Control that
#' extends an [Element].
#'
#' @description
#' An object that is meant to be the base type for
#' all ui controls. Controls are meant to represent
#' self-contained functional units (e.g., charts,
#' menus, tables, etc.).
#'
#' @details
#' Controls are modularized containers for UI widgets.
#'
#' @docType class
#' @export
#' @importFrom R6 R6Class
Control <- R6::R6Class(
  classname = "Control",
  inherit = Element,
  public = list(

    #' @description
    #' Creates a new instance of this
    #' [Element] class.
    #' @return A new `Control` object.
    initialize = function() {
      super$initialize()
    }

  ),
  private = list()
)
