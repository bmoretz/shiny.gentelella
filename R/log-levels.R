#' @title LogLevel
#'
#' @description S3 object to represent a typed & predefined log level.
#'
#' @param name name of the log level is the string representation.
#' @param severity log severity is used in threshold evaluations when
#' viewing a message of this type.
#'
#' @section Severity:
#'
#' \itemize{
#'  \item{"FATAL"} : {The FATAL level designates very severe error events that will presumably lead the application to abort.}
#'  \item{"ERROR"} : {The ERROR level designates error events that might still allow the application to continue running.}
#'  \item{"WARN"} : {The WARN level designates potentially harmful situations.}
#'  \item{"SUCCESS"} : {The SUCCESS level designates that the operation was unencumbered.}
#'  \item{"INFO"} : {The INFO level designates informational messages that highlight the progress of the application at coarse-grained level.}
#'  \item{"DEBUG"} : {The DEBUG Level designates fine-grained informational events that are most useful to debug an application.}
#'  \item{"TRACE"} : {The TRACE Level designates finer-grained informational events than the DEBUG}
#' }
#'
#' @return new [LogLevel]
#' @export
LogLevel <- function(name, severity) {
  if(!is.integer(severity) || severity < 0)
    stop("invalid severity level")

  if(!is.character(name) || nchar(name) == 0)
    stop("invalid log level")

  obj <- list(Name = name,
              Severity = severity)

  class(obj) <- append(class(obj), "LogLevel")

  obj
}

#' @title get_name
#'
#' @description gets the name of the log level.
#'
#' @param level log level
#'
#' @return log level name
#' @export
get_name <- function(level) {
  UseMethod("get_name", level)
}

#' @title get_name
#'
#' @description gets the name of the log level.
#'
#' @param level log level
#'
#' @return log level name
#' @export
get_level.LogLevel <- function(level) {
  return(level$name)
}

#' @title get_name
#'
#' @description gets the name of the log level.
#'
#' @param x log level
#' @param ... ignored
#' @return log level name
#' @export
#' @method as.character LogLevel
as.character.LogLevel <- function(x, ...) {
  return(x$Level)
}

#' get_level
#'
#' @param level log level
#'
#' @return log level
#' @export
get_severity <- function(level) {
  UseMethod("get_severity", level)
}

#' Gets the severity of a log level.
#'
#' @param x log level
#' @param ... ignored
#' @return log level
#' @export
#' @method as.integer LogLevel
as.integer.LogLevel <- function(x, ...) {
  return(x$Severity)
}

#' Gets the severity of a log level.
#'
#' @param level log level
#'
#' @return
#' @export
#'
#' @examples
#' get_severity(FATAL)
get_severity <- function(level) {
  return(level$Severity)
}

FATAL   = LogLevel(name = "FATAL",   severity = 100L)
ERROR   = LogLevel(name = "ERROR",   severity = 200L)
WARN    = LogLevel(name = "WARN",    severity =  300L)
SUCCESS = LogLevel(name = "SUCCESS", severity = 350L)
INFO    = LogLevel(name = "INFO",    severity = 400L)
DEBUG   = LogLevel(name = "DEBUG",   severity =  500L)
TRACE   = LogLevel(name = "TRACE",   severity = 600L)
