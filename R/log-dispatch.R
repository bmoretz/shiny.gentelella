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
  cloneable = F,

  public = list(

    #' @description
    #' Creates a new instance of a log config.
    #' @return A new `LogLayout` object.
    initialize = function() {
      super$initialize()

      private$system_context <- sys_context()
    },

    #' Log a trace level message
    #'
    #' @param msg message to log
    #' @return reference to self
    trace = function(msg) {
      private$dispatch(TRACE, msg)
    },

    #' Log an information level message
    #'
    #' @param msg message to log
    #'
    #' @return reference to self
    info = function(msg) {
      private$dispatch(INFO, msg)
    },

    #' Log a debug level message
    #'
    #' @param msg message to log
    #'
    #' @return reference to self
    debug = function(msg) {
      private$dispatch(DEBUG, msg)
    },

    #' Log a warn level message
    #'
    #' @param msg message to log
    #'
    #' @return reference to self
    warn = function(msg) {
      private$dispatch(WARN, msg)
    },

    #' Log an error level message
    #'
    #' @param msg message to log
    #'
    #' @return reference to self
    error = function(msg) {
      private$dispatch(ERROR, msg)
    },

    #' Log a fatal level message
    #'
    #' @param msg message to log
    #'
    #' @return reference to self
    fatal = function(msg) {
      private$dispatch(FATAL, msg)
    },

    #' Log a success level message
    #'
    #' @param msg message to log
    #'
    #' @return reference to self
    succes = function(msg) {
      private$dispatch(SUCCESS, msg)
    },

    system = function() {
      private$system_context
    }
  ),

  active = list(),

  private = list(

    system_context = NULL,

    dispatch = function(level, msg) {

      structure(function(level, msg, namespace = NA_character_,
                         .logcall = sys.call(), .topcall = sys.call(-1), .topenv = parent.frame()) {

        if(!inherits(level, 'log_level')) {
          unknown_severity_warning(level)
        }

        with(private$system_context(log_level = level, namespace = namespace,
                                        .logcall = .logcall, .topcall = .topcall, .topenv = .topenv),
             cat("",
                 glue::glue(private$object_format()),
                 glue::glue(private$msg_format()),
                 sep = "\r\n")
        )

      }, generator = deparse(match.call()))
    },

    get_dispatch_context = function(log_level = NULL,
                                    namespace = NA_character_,
                                    .logcall = sys.call(),
                                    .topcall = sys.call(-1),
                                    .topenv = parent.frame()) {

      list(
        ns        = namespace,
        topenv    = environmentName(topenv(.topenv)),
        fn        = deparse_to_one_line(.topcall[[1]]),
        call      = deparse_to_one_line(.topcall),

        pid       = Sys.getpid(),

        node       = sysinfo[['nodename']],
        arch       = sysinfo[['machine']],
        os_name    = sysinfo[['sysname']],
        os_release = sysinfo[['release']],
        os_version = sysinfo[['version']],
        user       = sysinfo[['user']]
      )
    }
  )
)
