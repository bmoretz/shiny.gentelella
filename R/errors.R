
#' Abstract Class Error
#'
#' @param cls_name name of class that was instantiated.
#' @param call function call to show in the condition.
#' @param ... var args
#'
#' @return error condition
#' @export
#'
#' @examples
#'
#'\dontrun{
#' ABC <- R6::R6Class(
#'   public = list(
#'     classname = "ABC",
#'
#'     initialize = function() {
#'
#'       cls_name <- get_classname()
#'
#'       if(cls_name == "ABC")
#'         abstract_class_error(cls_name, call = "initialize")
#'
#'      ...continue initialize
#'     },
#' )
#'}
#' @importFrom glue glue
abstract_class_error <- function(cls_name, call = sys.call(-1),
                                 ...) {

  err_msg <- "Type '{cls_name}' is an abstract base class
              and therefore cannot be instantiated directly."

  stop(structure(
    list(
      message = strwrap(glue::glue(err_msg)),
      call = call,
      ...
    ),
    class = c("is_abstract_error",
              "error",
              "condition")
  ))
}
