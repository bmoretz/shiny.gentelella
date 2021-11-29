#' @title LogLayout
#'
#' @description
#' R6 Class to encapsulate common logging configurations.
#'
#' @details
#' An object to encapsulate common logging  configurations,
#' formatters, generators, colorizers, etc. Designed to
#' be used via composition inside UI elements (pages,
#' controls, etc) as parent object is expected to have
#' UI Element interface (id, cls_name).
#'
#' @docType class
#' @family Logging
#' @importFrom crayon combine_styles make_style italic bold reset
#' @importFrom crayon silver magenta cyan
#' @export
LogLayout <- R6::R6Class(
  classname = "LogLayout",
  inherit = Singleton,
  cloneable = F,

  public = list(

    #' @description
    #' Creates a new instance of a log config.
    #' @return A new `LogLayout` object.
    initialize = function() {
      super$initialize()
    },

    #' @description
    #' generates a customized logging
    #' layout for passed in object.
    #' @param cls class to generate log layout for.
    #' @returns customized log layout for class instance.
    generate_ui_layout = function(cls) {
      private$class_layout_generator(cls)
    },

    #' @description
    #' formats a log metric by colorizing it based
    #' on the log severity.
    #' @param level log severity.
    #' @param metric metric to log.
    #' @param ... additional styles
    #' @returns log metric layout colorized by severity.
    format_metric_colorized_severity = function(level, metric, ...) {
      paste0('{crayon::bold(LogFormats$colorize_by_level(level, metric))}')
    },

    #' @description
    #' formats a log metric.
    #' @param ... format styles
    #' @param metric logging metric
    #' @param env environment for style evaluation
    #' @returns log metric layout.
    format_metric = function(..., metric,
                             env = parent.frame()) {
      structure(
        list(),
        style = crayon::combine_styles(...),
        metric = paste0('{', metric, '}'),
        class = 'fmt_metric'
      )
    },

    #' @description
    #' formats a log literal value.
    #' @param ... format styles
    #' @param literal log value
    #' @param env environment for style evaluation
    #' @returns log metric layout.
    format_literal = function(..., literal,
                              env = parent.frame()) {
      structure(
        list(),
        style = crayon::combine_styles(...),
        value = literal,
        class = 'fmt_literal'
      )
    },

    #' @description
    #' inserts a new line in the format.
    #' @returns log layout newline.
    new_line = function() {
      structure(
        list(),
        class = 'fmt_newline'
      )
    },

    #' @description
    #' formats a log entry
    #' @param ... items to format
    #' @param include_spacer indicate if there should be an empty line
    #' preceding the entry.
    #' @returns formatted log entry.
    format_entry = function(..., include_spacer = T) {
      ifelse(include_spacer, cat("", ...), cat(...))
    },

    #' @description
    #' formats a log metric by colorizing it based
    #' on the log severity.
    #' @param level log severity.
    #' @param metric metric to log.
    #' @returns log metric layout colorized by severity.
    colorize_by_level = function(level, metric) {

      color <- switch(
        attr(level, 'level'),
        'FATAL'   = crayon::combine_styles(crayon::bold, crayon::make_style('red1')),
        'ERROR'   = crayon::combine_styles(crayon::bold, crayon::make_style('red4')),
        'WARN'    = crayon::combine_styles(crayon::bold, crayon::make_style('darkorange')),
        'SUCCESS' = crayon::combine_styles(crayon::bold, crayon::make_style('green4')),
        'INFO'    = crayon::combine_styles(crayon::bold, crayon::make_style('dodgerblue4')),
        'DEBUG'   = crayon::combine_styles(crayon::bold, crayon::make_style('deepskyblue4')),
        'TRACE'   = crayon::combine_styles(crayon::bold, crayon::make_style('gray50')),
        unknown_severity_warning(level)
      )

      paste0(color(metric), crayon::reset(''))
    },

    #' @description
    #' formats a log metric by colorizing it based
    #' on the log severity in gray scale format.
    #' @param level log severity.
    #' @param metric metric to log.
    #' @returns log metric layout colorized by severity.
    grayscale_by_level = function(level, metric) {

      color <- switch(
        attr(level, 'level'),
        'FATAL'   = crayon::make_style('gray100'),
        'ERROR'   = crayon::make_style('gray90'),
        'WARN'    = crayon::make_style('gray80'),
        'SUCCESS' = crayon::make_style('gray70'),
        'INFO'    = crayon::make_style('gray60'),
        'DEBUG'   = crayon::make_style('gray50'),
        'TRACE'   = crayon::make_style('gray40'),
        unknown_severity_warning(level)
      )

      paste0(color(msg), crayon::reset(''))
    }
  ),

  active = list(),

  private = list(

    palette = list(),

    default_palette = function() {
        list(
          FATAL = list(Color = crayon::combine_styles(crayon::bold, crayon::make_style('red1')),
                       Grayscale = crayon::make_style('gray100')),

          ERROR = list(Color = crayon::combine_styles(crayon::bold, crayon::make_style('red4')),
                       Grayscale = crayon::make_style('gray90')),

          WARN = list(Color = crayon::combine_styles(crayon::bold, crayon::make_style('darkorange')),
                      Grayscale = crayon::make_style('gray80')),

          SUCCESS = list(Color = crayon::combine_styles(crayon::bold, crayon::make_style('green4')),
                         Grayscale = crayon::make_style('gray70')),

          INFO = list(Color = crayon::combine_styles(crayon::bold, crayon::make_style('dodgerblue4')),
                      Grayscale = crayon::make_style('gray60')),

          DEBUG = list(Color = crayon::combine_styles(crayon::bold, crayon::make_style('deepskyblue4')),
                       Grayscale = crayon::make_style('gray50')),

          DEBUG = list(Color = crayon::combine_styles(crayon::bold, crayon::make_style('gray50')),
                       Grayscale = crayon::make_style('gray40'))
        )
    },

    object_format = function() {
      paste(
        '{crayon::combine_styles(crayon::cyan, crayon::italic)(cls_name)}',
        '[{crayon::combine_styles(crayon::italic, crayon::silver)(id)}]'
      )
    },

    msg_format = function() {
      paste(
        '{crayon::bold(private$colorize_log_levels(level, levelr))}',
        '[{crayon::combine_styles(crayon::italic, crayon::silver)(format(time, "%Y-%m-%d %H:%M:%S"))}]',
        '({crayon::combine_styles(crayon::bold, crayon::magenta)(fn)})',
        ' : ',
        '{crayon::bold(private$colorize_msg_levels(msg, levelr))}'
      )
    },

    class_layout_generator = function(cls) {

      cls_name <- cls$class_name()
      id <- cls$identifier()

       cat("",
           glue::glue(private$object_format()),
           glue::glue(private$msg_format()),
           sep = "\r\n")
    }
  )
)
