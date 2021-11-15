#' R6 Class to encapsulate common logging configurations.
#'
#' An object to encapsulate common logging  configurations,
#' formatters, generators, colorizers, etc. Designed to
#' be used via composition inside UI elements (pages,
#' controls, etc) as parent object is expected to have
#' UI Element interface (id, cls_name).
#'
#' @export
#' @importFrom crayon combine_styles make_style italic bold reset
#' @importFrom crayon silver magenta cyan
#' @importFrom logger get_logger_meta_variables
LogConfig <- R6::R6Class(
  classname = "LogConfig",

  public = list(

    #' @description
    #' Creates a new instance of a log config.
    #' @return A new `LogConfig` object.
    initialize = function() {
    },

    #' @description
    #' generates a customized logging
    #' layout for passed in object.
    #' @param cls class to generate log layout for.
    #' @returns customized log layout for class instance.
    generate_layout = function(cls) {
      private$class_layout_generator(cls)
    }
  ),

  private = list(

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

      cls_name <- cls$class_name(); id <- cls$identifier()

      structure(function(level, msg, namespace = NA_character_,
                         .logcall = sys.call(), .topcall = sys.call(-1), .topenv = parent.frame()) {

        if(!inherits(level, 'loglevel')) {
          stop('Invalid log level, see ?log_levels')
        }

        with(logger::get_logger_meta_variables(log_level = level, namespace = namespace,
                                               .logcall = .logcall, .topcall = .topcall, .topenv = .topenv),
             cat(paste0(
               glue::glue(private$object_format()), "\n"),
               glue::glue(private$msg_format()))
        )

      }, generator = deparse(match.call()))
    },

    colorize_log_levels = function(msg, level) {

      color <- switch(
        attr(level, 'level'),
        'FATAL'   = crayon::combine_styles(crayon::bold, crayon::make_style('red1')),
        'ERROR'   = crayon::combine_styles(crayon::bold, crayon::make_style('red4')),
        'WARN'    = crayon::combine_styles(crayon::bold, crayon::make_style('darkorange')),
        'SUCCESS' = crayon::combine_styles(crayon::bold, crayon::make_style('green4')),
        'INFO'    = crayon::combine_styles(crayon::bold, crayon::make_style('dodgerblue4')),
        'DEBUG'   = crayon::combine_styles(crayon::bold, crayon::make_style('deepskyblue4')),
        'TRACE'   = crayon::combine_styles(crayon::bold, crayon::make_style('gray50')),
        stop('Unknown log level')
      )

      paste0(color(msg), crayon::reset(''))
    },

    colorize_msg_levels = function(msg, level) {

      color <- switch(
        attr(level, 'level'),
        'FATAL'   = crayon::make_style('gray100'),
        'ERROR'   = crayon::make_style('gray90'),
        'WARN'    = crayon::make_style('gray80'),
        'SUCCESS' = crayon::make_style('gray70'),
        'INFO'    = crayon::make_style('gray60'),
        'DEBUG'   = crayon::make_style('gray50'),
        'TRACE'   = crayon::make_style('gray40'),
        stop('Unknown log level')
      )

      paste0(color(msg), crayon::reset(''))
    }
  )
)
