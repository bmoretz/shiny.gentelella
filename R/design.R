#' @title get_class_name
#'
#' @description
#' Used to determine the run-time class name of
#' an object.
#'
#' @details
#' This function determines the type of the instantiated
#' object at run-time, which has a tremendous amount of
#' utility, especially objects with deeply nested inheritance.
#' Must be called within the object's constructor ([initialize]).
#'
#' @return run-time class name
#' @export
#'
#' @examples
#' Base <- R6::R6Class(
#'   classname = "Base",
#'
#'   public = list(
#'     initialize = function() {
#'       private$.cls_name <- get_class_name()
#'     },
#'
#'    class_name = function() {
#'       private$.cls_name
#'     }
#'   ),
#'
#'   private = list(
#'     .cls_name = NULL
#'   )
#' )
#
#' Derived <- R6::R6Class(
#'   classname = "Derived",
#'   inherit = Base,
#'
#'   public = list(),
#'   private = list()
#' )
#'
#' base <- Base$new(); derived <- Derived$new()
#'
#' print(base$class_name())
#' print(derived$class_name())
#' @importFrom stringr str_remove str_detect
get_class_name = function() {
  calls <- as.character(sys.calls())
  calls <- calls[max(which(stringr::str_detect(calls, "\\$new\\(.*\\)")))]
  stopifnot(length(calls) == 1)
  invisible(stringr::str_remove(calls, "\\$new\\(.*\\)"))
}
