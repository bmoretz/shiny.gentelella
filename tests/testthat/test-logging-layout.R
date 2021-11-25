LoggingLayoutTester <- R6::R6Class(
  classname = "LoggingLayoutTester",
  inherit = Element,

  public = list(

    initialize = function() {
      super$initialize()
    },

    set_logging = function() {
      private$set_logging()
    },

    test_get_layout = function() {
      private$log_config$generate_layout(self)
    },

    get_generator = function() {
      private$layout_generator()
    },

    log_fn = function(input) {
      logger::log
    }
  ),

  private = list()
)

test_layouts <- F

# these test don't actually perform test,
# rather provide a simple way to tweak
# the logging output visually.

display_log_layout <- function(level, message) {
  log_tester <- LoggingLayoutTester$new()
  layout <- log_tester$test_get_layout()

  fn_context <- function() {
    layout(level, message)
  }

  if(test_layouts)
    fn_context()
}

test_that("log_layout_trace", {
  expect_null(display_log_layout(logger::TRACE, "this is a trace msg layout"))
})

test_that("log_layout_info", {
  expect_null(display_log_layout(logger::INFO, "this is an info msg layout"))
})

test_that("log_layout_debug", {
  expect_null(display_log_layout(logger::DEBUG, "this is a debug msg layout"))
})

test_that("log_layout_warn", {
  expect_null(display_log_layout(logger::WARN, "this is a warn msg layout"))
})

test_that("log_layout_error", {
  expect_null(display_log_layout(logger::ERROR, "this is an error msg layout"))
})

test_that("log_layout_fatal", {
  expect_null(display_log_layout(logger::FATAL, "this is a fatal msg layout"))
})

test_that("log_layout_fatal", {
  expect_null(display_log_layout(logger::SUCCESS, "this is a success msg layout"))
})


test_that("log_output_format", {

  log_tester <- LoggingLayoutTester$new()

  log_msg <- 'log test msg'

  formatted_output <- capture_output_lines({
    layout <- log_tester$test_get_layout()
    logger::log_layout(layout = layout)
    layout(logger::INFO, log_msg)
  })

  lines <- stringr::str_split(formatted_output, "\n")

  expect_equal(length(lines), 3)

  context_info <- lines[[2]]

  expect_true(stringr::str_detect(context_info, log_tester$class_name()))
  expect_true(stringr::str_detect(context_info, log_tester$identifier()))

  message_info <- lines[[3]]

  expect_true(stringr::str_detect(message_info, log_msg))
})
