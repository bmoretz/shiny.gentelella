test_layouts <- F

# these test don't actually perform test,
# rather provide a simple way to tweak
# the logging output visually.

display_log_layout <- function(level, message) {
  log_tester <- LogLayoutTester$new()
  layout <- log_tester$test_get_layout()

  fn_context <- function() {
    layout(level, message)
  }

  if(test_layouts)
    fn_context()
}

test_that("log_layout_trace", {

  T
  expect_null(display_log_layout("this is a trace msg layout"))
})

#
# test_that("log_layout_info", {
#   expect_null(display_log_layout("this is an info msg layout"))
# })
#
# test_that("log_layout_debug", {
#   expect_null(display_log_layout("this is a debug msg layout"))
# })
#
# test_that("log_layout_warn", {
#   expect_null(display_log_layout("this is a warn msg layout"))
# })
#
# test_that("log_layout_error", {
#   expect_null(display_log_layout("this is an error msg layout"))
# })
#
# test_that("log_layout_fatal", {
#   expect_null(display_log_layout(this is a fatal msg layout"))
# })
#
# test_that("log_layout_fatal", {
#   expect_null(display_log_layout("this is a success msg layout"))
# })

test_that("literal_output_style", {

  literal <- new_fmt_literal(bold $ red, "literal value")

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
  expect_equal(value, "\033[1m\033[31mliteral value\033[39m\033[22m")
})

test_that("metric_output", {

  metric <- new_fmt_metric(bold $ red, "sysname")

  fmt_style <- style(metric)

  expect_true(!is.null(fmt_style))

  style_info <- unlist(attributes(fmt_style))

  expect_equal(style_info['class'], c(class = "crayon"))

  expect_equal(style_info['_styles.bold.open'], c(`_styles.bold.open` = '\033[1m'))
  expect_equal(style_info['_styles.bold.close'], c(`_styles.bold.close` = '\033[22m'))
  expect_equal(style_info['_styles.red.open'], c(`_styles.red.open` = '\033[31m'))
  expect_equal(style_info['_styles.bold.open'], c(`_styles.bold.open` = '\033[1m'))
  expect_equal(style_info['_styles.red.palette'], c(`_styles.red.palette` = '2'))

  value <- value(metric)

  expect_true(!is.null(value))
  expect_equal(value, "\033[1m\033[31m{sysname}\033[39m\033[22m")
})

test_that("newline_output", {
  newline <- new_fmt_line_break()

  expect_true(!is.null(newline))

  value <- value(newline)

  expect_equal(value, "\n")
})

test_that("log_output_format", {

  c <- class(FATAL)

  class(FATAL)

  is.primitive(c)

  test <- "literal"
  element <- TestElement$new()

  class(TestElement)
  attributes(element)
  attr(element, 'class')

  R6::is.R6(LogDispatch)
  fmt_sysname <- new_fmt_metric(bold $ green, "sysname")
  fmt_release <- new_fmt_metric(bold $ red, "release")
  fmt_seperator <- new_fmt_line_break()
  fmt_text1 <- new_fmt_literal(bold $ blue, "literal text")

  layout <- new_log_layout(fmt_sysname,
                              fmt_release,
                              fmt_seperator,
                              fmt_text1)

  expect_equal(length(layout), 4)

  evaluated <- value(layout)

  expect_equal(evaluated, "\033[1m\033[32m{sysname}\033[39m\033[22m\033[1m\033[31m{release}\033[39m\033[22m\n\033[1m\033[34mliteral text\033[39m\033[22m")
})

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
