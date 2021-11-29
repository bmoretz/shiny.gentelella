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
    },

    #' Log a trace level message
    #'
    #' @param msg message to log
    #' @param format log format
    #' @return reference to self
    trace = function(msg, format) {
      private$dispatch(trace, msg, format)
    },

    #' Log an information level message
    #'
    #' @param msg message to log
    #'
    #' @return reference to self
    info = function(msg) {
      private$log_layout(levels$INFO, msg)
      invisible(self)
    },

    #' Log a debug level message
    #'
    #' @param msg message to log
    #'
    #' @return reference to self
    debug = function(msg) {
      private$log_layout(levels$DEBUG, msg)
      invisible(self)
    },

    #' Log a warn level message
    #'
    #' @param msg message to log
    #'
    #' @return reference to self
    warn = function(msg) {
      private$log_layout(levels$WARN, msg)
      invisible(self)
    },

    #' Log an error level message
    #'
    #' @param msg message to log
    #'
    #' @return reference to self
    error = function(msg) {
      private$log_layout(levels$ERROR, msg)
      invisible(self)
    },

    #' Log a fatal level message
    #'
    #' @param msg message to log
    #'
    #' @return reference to self
    fatal = function(msg) {
      private$log_layout(levels$FATAL, msg)
      invisible(self)
    },

    #' Log a success level message
    #'
    #' @param msg message to log
    #'
    #' @return reference to self
    succes = function(msg) {
      private$log_layout(levels$SUCCESS, msg)
      invisible(self)
    }
  ),

  active = list(

    #' @field system returns a lazy-loaded field to store
    #' system level logging metrics.
    system = function(value) {

      if(missing(value)) {
        private$system_context <- private$get_system_context()
      }

      private$system_context
    }
  ),

  private = list(

    system_context = NULL,

    get_system_context = function() {
      sys_info <- private$get_system_info()
      r_ver <- private$get_r_version()

      invisible(c(sys_info, r_ver))
    },

    dispatch = function(level, msg, format) {

      structure(function(level, msg, namespace = NA_character_,
                         .logcall = sys.call(), .topcall = sys.call(-1), .topenv = parent.frame()) {

        if(!inherits(level, 'log_level')) {
          unknown_severity_warning(level)
        }

        with(private$get_system_context(log_level = level, namespace = namespace,
                                        .logcall = .logcall, .topcall = .topcall, .topenv = .topenv),
             cat("",
                 glue::glue(private$object_format()),
                 glue::glue(private$msg_format()),
                 sep = "\r\n")
        )

      }, generator = deparse(match.call()))
    },

    get_system_info = function() {
      lapply(Sys.info(), FUN = function(var) var)
    },

    get_r_version = function() {
      c('r-ver' = paste0(R.Version()[c('major', 'minor')], collapse = '.'))
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

        time      = Sys.time(),
        levelr    = log_level,
        level     = attr(log_level, 'level'),

        pid       = Sys.getpid(),

        #ns_pkg_version = tryCatch(as.character(packageVersion(namespace)), error = function(e) NA_character_),

        ## stuff from Sys.info
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

