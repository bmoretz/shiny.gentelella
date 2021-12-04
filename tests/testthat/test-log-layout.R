test_that("literal_output_style", {

  literal <- new_fmt_literal(crayon::red$bold, "literal value")

  fmt_style <- style(literal)

  expect_true(!is.null(fmt_style))

  style_info <- unlist(attributes(fmt_style))

  expect_equal(style_info['class'], c(class = "crayon"))

  expect_equal(style_info['_styles.bold.open'], c(`_styles.bold.open` = '\033[1m'))
  expect_equal(style_info['_styles.bold.close'], c(`_styles.bold.close` = '\033[22m'))
  expect_equal(style_info['_styles.red.open'], c(`_styles.red.open` = '\033[31m'))
  expect_equal(style_info['_styles.bold.open'], c(`_styles.bold.open` = '\033[1m'))
  expect_equal(style_info['_styles.red.palette'], c(`_styles.red.palette` = '2'))

  value <- value(literal)

  expect_true(!is.null(value))
  expect_equal(value, "\033[31m\033[1mliteral value\033[22m\033[39m")
})

test_that("metric_output", {

  metric <- new_fmt_metric(crayon::red$bold, "sysname")

  fmt_style <- style(metric)

  expect_true(!is.null(fmt_style))

  style_info <- unlist(attributes(fmt_style))

  expect_equal(style_info['class'], c(class = "crayon"))

  expect_equal(style_info['_styles.bold.open'], c(`_styles.bold.open` = '\033[1m'))
  expect_equal(style_info['_styles.bold.close'], c(`_styles.bold.close` = '\033[22m'))
  expect_equal(style_info['_styles.bold.open'], c(`_styles.bold.open` = '\033[1m'))

  expect_equal(style_info['_styles.red.palette'], c(`_styles.red.palette` = '2'))
  expect_equal(style_info['_styles.red.open'], c(`_styles.red.open` = '\033[31m'))

  value <- value(metric)

  expect_true(!is.null(value))
  expect_equal(value, "\033[31m\033[1m{sysname}\033[22m\033[39m")
})

test_that("newline_output", {
  newline <- new_fmt_line_break()

  expect_true(!is.null(newline))

  value <- value(newline)

  expect_equal(value, "\n")
})

test_that("timestamp_output_dflt_fmt", {

  ts <- new_fmt_timestamp(crayon::silver$italic)

  expect_true(!is.null(ts))

  fmt_style <- style(ts)

  expect_true(!is.null(fmt_style))
  expect_equal(class(fmt_style), "crayon")

  style_info <- unlist(attributes(fmt_style))

  expect_equal(style_info['class'], c(class = "crayon"))

  expect_equal(style_info['_styles.italic.open'], c(`_styles.italic.open` = '\033[3m'))
  expect_equal(style_info['_styles.italic.close'], c(`_styles.italic.close` = '\033[23m'))
  expect_equal(style_info['_styles.italic.open'], c(`_styles.italic.open` = '\033[3m'))

  expect_equal(style_info['_styles.silver.open'], c(`_styles.silver.open` = '\033[90m'))
  expect_equal(style_info['_styles.silver.palette'], c(`_styles.silver.palette` = '9'))

  actual_time <- format(Sys.time(), format = "%x %H:%M:%S %z")
  evaluated_time <- attr(ts, 'value')(format(ts))

  expect_equal(actual_time, evaluated_time)

  actual_value <- value(ts)
  expected_value <- paste0("\033[90m\033[3m",
                           format(Sys.time(), "%x %H:%M:%S %z"),
                           "\033[23m\033[39m")

  expect_equal(actual_value, expected_value)
})

test_that("timestamp_output_custom_fmt", {

  cust_format <- "%Y-%m-%d %H:%M:%S"

  ts <- new_fmt_timestamp(crayon::silver$italic, cust_format)

  fmt_style <- style(ts)

  expect_true(!is.null(ts))

  fmt_style <- style(ts)

  expect_true(!is.null(fmt_style))
  expect_equal(class(fmt_style), "crayon")

  style_info <- unlist(attributes(fmt_style))

  expect_equal(style_info['class'], c(class = "crayon"))

  expect_equal(style_info['_styles.italic.open'], c(`_styles.italic.open` = '\033[3m'))
  expect_equal(style_info['_styles.italic.close'], c(`_styles.italic.close` = '\033[23m'))
  expect_equal(style_info['_styles.italic.open'], c(`_styles.italic.open` = '\033[3m'))

  expect_equal(style_info['_styles.silver.open'], c(`_styles.silver.open` = '\033[90m'))
  expect_equal(style_info['_styles.silver.palette'], c(`_styles.silver.palette` = '9'))

  actual_format <- format(ts)
  expect_equal(actual_format, cust_format)

  actual_time <- format(Sys.time(), cust_format)
  evaluated_time <- attr(ts, 'value')(format(ts))

  expect_equal(actual_time, evaluated_time)

  actual_value <- value(ts)
  expected_value <- paste0("\033[90m\033[3m",
                           format(Sys.time(), cust_format),
                                  "\033[23m\033[39m")

  expect_equal(actual_value, expected_value)
})

test_that("log_output_format", {

  fmt_sysname <- new_fmt_metric(crayon::green$bold, "sysname")
  fmt_release <- new_fmt_metric(crayon::red$bold, "release")
  fmt_seperator <- new_fmt_line_break()
  fmt_text1 <- new_fmt_literal(crayon::blue$italic, "literal text")

  layout <- new_log_layout(fmt_sysname,
                              fmt_release,
                              fmt_seperator,
                              fmt_text1)

  expect_equal(length(layout), 4)

  evaluated <- value(layout)

  expect_equal(evaluated, "\033[1m\033[32m{sysname}\033[39m\033[22m\033[1m\033[31m{release}\033[39m\033[22m\n\033[1m\033[34mliteral text\033[39m\033[22m")
})

test_that("loading_log_config_works", {

  log_config_file <- system.file("test-data",
                                 "mock-log-layout.yml",
                               package = "shiny.gentelella")

  config <- load_log_configuration(log_config_file)

  expect_equal(config$settings$threshold, "TRACE")

  with(config, {
    expect_true(!is.null(levels$TRACE))
    expect_true(!is.null(levels$DEBUG))
    expect_true(!is.null(levels$INFO))
    expect_true(!is.null(levels$SUCCESS))
    expect_true(!is.null(levels$WARN))
    expect_true(!is.null(levels$ERROR))
    expect_true(!is.null(levels$FATAL))
  })
})

# these test don't actually perform test,
# rather provide a simple way to tweak
# the logging output visually.

# display_log_layout <- function(level, message) {
#   log_tester <- LogLayoutTester$new()
#   layout <- log_tester$test_get_layout()
#
#   fn_context <- function() {
#     layout(level, message)
#   }
#
#   if(test_layouts)
#     fn_context()
# }

# log_tester <- LoggingLayoutTester$new()
#
# log_msg <- 'log test msg'
#
# formatted_output <- capture_output_lines({
#   layout <- log_tester$test_get_layout()
#   log_layout(layout = layout)
#   layout(info log_msg)
# })
#
# lines <- stringr::str_split(formatted_output, "\n")
#
# expect_equal(length(lines), 3)
#
# context_info <- lines[[2]]
#
# expect_true(stringr::str_detect(context_info, log_tester$class_name()))
# expect_true(stringr::str_detect(context_info, log_tester$identifier()))
#
# message_info <- lines[[3]]
#
# })
