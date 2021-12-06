#' @title LogDispatch
#'
#' @description
#' R6 Class that dispatches log messages throughout the application.
#'
#' @details
#' This object is designed to a centralized logging dispatcher that
#' renders log messages with the appropriate context of the calling
#' object. The [LogLayout] object is used to generate log message
#' layouts (render formats), which are used by the [LogDispatcher]
#' to render highly-customizable and detailed log messages.
#'
#' @section Metrics:
#'
#' System Context
#'
#' \itemize{
#'  \item{"sysname"} : {The operating system name.}
#'  \item{"release"} : {The OS release.}
#'  \item{"version"} : {The OS version.}
#'  \item{"nodename"} : {A name by which the machine is known on the network (if any).}
#'  \item{"machine"} : {A concise description of the hardware, often the CPU type.}
#'  \item{"login"} : {The user's login name, or "unknown" if it cannot be ascertained.}
#'  \item{"user"} : {The name of the real user ID, or "unknown" if it cannot be ascertained.}
#'  \item{"r-ver"} : {R Version [major].[minor]}
#' }
#'
#' @seealso LogLevel
#' @docType class
#' @family Logging
#' @importFrom R6 R6Class
#' @export
LogDispatch <- R6::R6Class(
  classname = "LogDispatch",
  inherit = Singleton,
  lock_objects = F,
  lock_class = F,
  cloneable = F,

  public = list(

    #' @field settings log settings
    settings = NA,

    #' @field system log system context
    system = NA,

    #' @description
    #' Creates a new instance of a log config.
    #' @return A new `LogLayout` object.
    initialize = function() {
      super$initialize()

      self$system <- sys_context()
    },

    #' @description
    #' Evaluates a log msg
    #' @return reference to self to support chaining.
    log = function(level, msg,
                   layout = "default") {

      call_context <- sys.call(-1)

      evaluated <- value(layout)

      private$dispatcher(level, msg, evaluated)

      #invisible(self)
      call_context
    },

    #' @description
    #' Adds dynamic function as a short-cut to
    #' log a message with a configured level.
    #' @return reference to self to support chaining.
    add_log_level = function(level) {
      name <- level_name(level)

      self[[name]] <- rlang::as_function(~ print(paste(.x, .y)))
    }
  ),

  active = list(),

  private = list(

    system_context = NULL,

    dispatcher = structure(function(level, msg,
                                     ...,
                                     .logcall = sys.call(),
                                     .topcall = sys.call(-1),
                                     .topenv = parent.frame()) {

      with(c(self$system,
             .logcall = .logcall,
             .topcall = sys.call(-1),
             .topenv = parent.frame()),
           {
             cat(glue::glue(..., envir = .topenv))
           }
      )
    }, generator = quote(dispatcher))
  )
)
