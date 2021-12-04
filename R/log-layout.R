#' Format Layout
#'
#' @description
#' Base type for log format objects.
#' @param style [crayon] that the layout will use in log generation.
#'
#' @family Log Layout
#' @return new log format
#' @export
new_fmt_layout <- function(style) {

  stopifnot(class(style) != "crayon")

  structure(
    list(),
    style = style,
    class = c("fmt_layout")
  )
}

#' Gets the value of a format object.
#'
#' @param fmt object to extract value from.
#' @param ... further arguments passed to or from other methods.
#'
#' @return object's value
#' @export
value <- function(fmt, ...) {
  UseMethod("value", fmt)
}

#' Formatted Metric
#'
#' @description
#' Inserts a formatted log metric.
#'
#' @param style that the layout will use in log generation
#' @param metric the metric to log.
#'
#' @seealso [LogDispatch]
#' @family Log Layout
#' @return a new formatted metric
#' @export
#'
#' @examples
#' new_fmt_metric(bold $ green, "sysname")
#'
#' new_fmt_metric(bold $ red, "release")
new_fmt_metric = function(style, metric) {
  stopifnot(class(style) == "crayon")

  if(!is.character(metric) || nchar(metric) == 0)
    stop("invalid log metric specified")

  structure(
    list(),
    style = style,
    metric = metric,
    class = c('fmt_layout', 'fmt_metric')
  )
}

#' Gets the style of a format object.
#'
#' @param fmt object to extract value from.
#'
#' @return object's value
#' @export
style <- function(fmt) {
  UseMethod("style", fmt)
}

#' Gets the style of a format object.
#'
#' @param fmt object to extract value from.
#' @param ... further arguments passed to or from other methods.
#'
#' @return object's value
#' @export
style.fmt_layout <- function(fmt, ...) {
  attr(fmt, 'style')
}

#' Gets the value of a format object.
#'
#' @param fmt object to extract value from.
#' @param ... further arguments passed to or from other methods.
#'
#' @return object's value
#' @export
value.fmt_metric <- function(fmt, ...) {
  style(fmt)(paste0('{', attr(fmt, 'metric'), '}'))
}

#' Formatted Literal
#'
#' @description
#' Placeholder for a formatted literal in a log layout.
#'
#' @param style format style (crayon)
#' @param literal log value
#'
#' @family Log Layout
#' @returns log metric layout.
#' @examples
#' new_fmt_literal(red $ bold, "literal text")
#'
#' new_fmt_literal(blue $ italic, "literal text")
new_fmt_literal <- function(style, literal) {
  structure(
    list(),
    style = style,
    value = literal,
    class = c('fmt_literal', 'fmt_layout')
  )
}

#' Gets the value of a format object.
#'
#' @param fmt object to extract value from.
#' @param ... further arguments passed to or from other methods.
#'
#' @return object's value
#' @export
value.fmt_literal <- function(fmt, ...) {
  style(fmt)(attr(fmt, 'value'))
}

#' Formatted Timestamp
#'
#' @description
#' Placeholder for a formatted timestamp in a log layout.
#'
#' @param style format style (crayon)
#' @param format timestamp format, defaults to: %x %H:%M:%S %z,
#' e.g., 12/04/21 14:31:25 -0500
#'
#' @family Log Layout
#' @returns log metric layout.
#' @examples
#' fmt_timestamp(red $ bold, "%Y-%m-%d %H:%M:%S")
#'
#' fmt_timestamp(blue $ italic, "%x %H:%M:%S %z")
new_fmt_timestamp <- function(style,
                              format = "%x %H:%M:%S %z") {
  structure(
    list(),
    style = style,
    format = format,
    value = rlang::as_function(~ format(Sys.time(), .)),
    class = c('fmt_timestamp', 'fmt_layout')
  )
}

#' Gets the format of a format object.
#'
#' @param fmt object to extract value from.
#' @param ... further arguments passed to or from other methods.
#'
#' @return object's value
#' @export
format.fmt_timestamp <- function(fmt, ...) {
  attr(fmt, 'format')
}

#' Gets the value of a format object.
#'
#' @param fmt object to extract value from.
#' @param ... further arguments passed to or from other methods.
#'
#' @return object's value
#' @export
value.fmt_timestamp <- function(fmt, ...) {
  s <- attr(ts, 'style')
  v <- attr(ts, 'value')
  f <- attr(ts, 'format')

  s(v(f))
}

#' Formatted Line Break
#'
#' @description
#' Placeholder for a new line in a log layout.
#'
#' @family Log Layout
#' @returns log layout newline.
#' @example
#' new_fmt_line_break()
new_fmt_line_break <- function() {
  structure(
    list(),
    style = crayon::black,
    value = '\n',
    class = c('fmt_newline', 'fmt_layout')
  )
}

#' Gets the value of a format object.
#'
#' @param fmt object to extract value from.
#' @param ... further arguments passed to or from other methods.
#'
#' @return object's value
#' @export
value.fmt_newline <- function(fmt, ...) {
  attr(fmt, 'value')
}

#' Formatted Log Level
#'
#' @description
#' Placeholder for the formatted log level in a log layout.
#'
#' @family Log Layout
#' @returns log level info.
#' @example
#' new_fmt_log_level()
new_fmt_log_level <- function() {
  structure(
    list(),
    style = crayon::black,
    value = glue::as_glue("{level_info(level)}"),
    class = c('fmt_level_info', 'fmt_layout')
  )
}

#' Gets the value of a log level object.
#'
#' @param fmt object to extract value from.
#' @param ... further arguments passed to or from other methods.
#'
#' @return object's value
#' @export
value.fmt_level_info <- function(fmt, ...) {
  attr(fmt, 'value')
}

#' @title
#' Formatted Messaged, based on log level
#'
#' @description
#' Placeholder for the log msg in a log layout.
#'
#' @family Log Layout
#' @returns log layout newline.
new_fmt_log_msg <- function() {
  structure(
    list(),
    style = crayon::black,
    value = glue::as_glue("{format(level, msg = {msg})}"),
    class = c('new_fmt_log_msg', 'fmt_layout')
  )
}

#' Gets the value of a log msg object.
#'
#' @param fmt object to extract value from.
#' @param ... further arguments passed to or from other methods.
#'
#' @return object's value
#' @export
value.new_fmt_log_msg <- function(fmt, ...) {
  attr(fmt, 'value')
}

#' Log Layout
#'
#' @description
#' a class that stores a collection of log format objects
#' and understands how to associate a given format to
#' a class of objects.
#'
#' @param ... collection of format objects to initialize with.
#' @param sep format entry separator, defaults to a single space.
#' @param association objects to associate this log format with.
#' @family Log Layout
#' @return object's value
#' @export
new_log_layout <- function(...,
                           sep = ' ',
                           association = character()) {
  new_layout <- structure(
    list(),
    format = list(...),
    separator = sep,
    association = association,
    class = c('log_layout')
  )

  if(identical(character(), character())) {
    return(new_layout)
  }

  layouts <- attr(new_log_layout, 'layouts')

  if(is.null(layouts)) {
    layouts <- list()
  }

  layouts[[association]] <- new_layout

  attr(new_log_layout, 'layouts') <<- layouts

  new_layout
}

#' Generic override for length of a log layout.
#'
#' @param x log format
#' @param ... further arguments passed to or from other methods.
#' @return number of formats in the layout.
#' @export
length.log_layout <- function(x, ...) {
  length(attr(x, 'format'))
}

#' A container for a set of formatters that specify the log
#' entry layout.
#'
#' @param layout collection of format objects to initialize with.
#' @param association class of objects to associate this format with.
#' @param ... further arguments passed to or from other methods.
#' @family Log Layout
#' @return evaluated log layout
#' @export
value.log_layout = function(layout, ...) {
  separator <- attr(layout, 'separator')

  paste0(unlist(sapply(attr(layout, 'format'),
                       function(fmt) value(fmt))), sep = separator, collapse = '')
}
