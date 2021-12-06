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

#' Formatted Function Call
#'
#' @description
#' Placeholder for the formatted calling function in a log layout.
#'
#' @param style format style (crayon)
#' @param levels levels to look back
#'
#' @family Log Layout
#' @returns log function call layout.
#' @examples
#' new_fmt_func_call(crayon::cyan$italic, 2)
#'
#' new_fmt_func_call(crayon::magenta$italic, 2)
new_fmt_func_call <- function(style, level) {
  structure(
    list(),
    style = style,
    level = level,
    class = c('fmt_func_call', 'fmt_layout')
  )
}

#' Gets the value of a format object.
#'
#' @param fmt object to extract value from.
#' @param ... further arguments passed to or from other methods.
#'
#' @return object's value
#' @export
value.fmt_func_call <- function(fmt, ...) {
  level <- attr(fmt, 'level')
  call_stack <- get_call_stack()
  deparse(call_stack[[level]])
}

#' Formatted Call Stack
#'
#' @description
#' Placeholder for the formatted call stack in a log layout.
#'
#' @param style format style (crayon)
#' @param level level to pull from call stack.
#' @family Log Layout
#' @returns log function call layout.
#' @examples
#' new_fmt_func_call(crayon::cyan$italic, 2)
#'
#' new_fmt_func_call(crayon::magenta$italic, 2)
new_fmt_call_stack <- function(style, levels) {
  structure(
    list(),
    style = style,
    levels = levels,
    class = c('fmt_call_stack', 'fmt_layout')
  )
}

#' Format Call Stack
#'
#' @description
#' Formats a raw call stack list.
#'
#' @param fmt fmt object
#' @param ... further arguments passed to or from other methods.
#' @family Log Layout
#' @returns formatted call stack
format.fmt_call_stack = function(fmt, ...) {
  paste0(sapply(rev(result),
                function(level) trim(deparse(level))),
         sep = "",
         collapse = ";")
}

#' Gets the value of a format object.
#'
#' @param fmt object to extract value from.
#' @param ... further arguments passed to or from other methods.
#'
#' @return object's value
#' @export
value.fmt_call_stack <- function(fmt, ...) {
  levels <- attr(fmt, 'level')
  call_stack <- get_call_stack()
  ret_value <- call_stack[levels:length(call_stack)]

  style(fmt)(ret_value)
}

#' Formatted Call Stack
#'
#' @description
#' Placeholder for the formatted call stack in a log layout.
#'
#' @param offset number of call levels to offset
#'
#' @family Log Layout
#' @returns formatted call stack
get_call_stack = function() {
  # number of levels deep
  n_levels <- length(sys.parents()) * -1
  # account for call to lapply
  frames <- seq(from = 0, to = n_levels - 1, by = -1)
  # get all frames
  call_stack <- lapply(frames, function(frame) sys.call(which = frame))
  # find where get_call_stack() is invoked
  look_back <- as.logical(match(call_stack, "get_call_stack()", nomatch = F))
  # subset to non-utility calls
  start <- which(look_back, arr.ind = T) + 1; end <- length(call_stack)
  call_stack[start:end]
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
  v <- attr(fmt, 'value')
  f <- attr(fmt, 'format')

  style(fmt)(v(f))
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

  format <- attr(layout, 'format')
  separator <- attr(layout, 'separator')

  range <- 1:(length(format))
  is_break <- sapply(format, function(fmt) 'fmt_newline' %in% class(fmt))
  groups <- split(range, with(rle(is_break), rep(cumsum(!values), lengths)))
  new_lines <- which(is_break, arr.ind = T)

  output <- character(0)

  for(group in groups) {

    rng <- unlist(unname(group))
    has_break <- as.logical(max(new_lines %in% rng))

    if(has_break == T) {
      rng <- rng[-length(rng)]
    }

    result <- paste(sapply(format[rng], function(fmt)
      value(fmt)), sep = separator, collapse = separator)

    output <- paste0(output, result)

    if(has_break) {
      output <- paste0(output,
                       character(0),
                       separator = "\n")
    }
  }

  output
}
