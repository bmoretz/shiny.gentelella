
#' Abstract Class Error
#'
#' @param cls_name name of class that was instantiated.
#' @param call function call to show in the condition.
#' @param ... var args
#'
#' @return error condition
#'
#' @examples
#'
#' ABC <- R6::R6Class(
#'   classname = "ABC",
#'   public = list(
#'     initialize = function() {
#'
#'       cls_name <- get_class_name()
#'
#'       if(cls_name == "ABC")
#'         abstract_class_error(cls_name, call = "initialize")
#'     }
#'   )
#' )
#'
#' result <- tryCatch(expr = {
#'   ABC$new()
#' },
#' error = function(e) {
#'  message(e)
#'  NULL
#'})
#' @export
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

#' Resource Not Found Error
#'
#' @param resource_path path of the resource that was attempted to load.
#' @param call function call to show in the condition.
#' @param ... var args
#'
#' @return error condition
#' @export
#' @importFrom glue glue
resource_not_found_error <- function(resource_path, call = sys.call(-1),
                                     ...) {

  err_msg <- "Resource '{resource_path}' was not found at the specified
              location and therefore unable to load."

  stop(structure(
    list(
      message = strwrap(glue::glue(err_msg)),
      call = call,
      ...
    ),
    class = c("resource_not_found_error",
              "error",
              "condition")
  ))
}
