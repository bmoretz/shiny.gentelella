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
#' Inserts a block of formatted literal text.
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

#' Formatted Line Break
#'
#' @description
#' Inserts a new line in the format.
#'
#' @family Log Layout
#' @returns log layout newline.
new_fmt_line_break <- function() {
  structure(
    list(),
    style = crayon::black,
    value = '\n',
    class = c('fmt_newline', 'fmt_layout')
  )
}

#' Formatted Log Level
#'
#' @description
#' Inserts a new line in the format.
#'
#' @family Log Layout
#' @returns log layout newline.
new_fmt_log_level <- function(level) {
  stopifnot(class(level) == 'log_level')

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

#' Log Layout
#'
#' @description
#' a class that stores a collection of log format objects
#' and understands how to associate a given format to
#' a class of objects.
#'
#' @param ... collection of format objects to initialize with.
#' @param association objects to associate this log format with.
#' @family Log Layout
#' @return object's value
#' @export
new_log_layout <- function(...,
                           association = character()) {
  new_layout <- structure(
    list(),
    format = list(...),
    association = association,
    class = c('log_layout')
  )

  if(nchar(association) == 0) {
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

#' A container for a set of formatters that specifiy the log
#' entry layout.
#'
#' @param ... collection of formatter objects to initialize with.
#' @param association class of objects to associate this format with.
#' @param ... further arguments passed to or from other methods.
#' @family Log Layout
#' @return evaluated log layout
#' @export
value.log_layout = function(layout, ...) {
  paste0(unlist(sapply(attr(layout, 'format'),
                       function(fmt) value(fmt))), collapse = '')
}
