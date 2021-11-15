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

    get_generator = function() {
      private$layout_generator()
    },

    log_fn = function(input) {
      logger::log
    }
  ),

  private = list()
)

test_that("log_layout_formats", {

  # log_msg <- 'log test msg'
  #
  # output <- testthat::capture_output_lines({
  #   log_tester <- LoggingLayoutTester$new()
  #   log_tester$set_logging()
  #   logger::log_info(log_msg)
  # })
  #
  # lines <- stringr::str_split(output, "\n")
  #
  # print(lines)
  # expect_equal(length(lines), 2)

  # context <- lines[[1]]
  #
  # has_class_info <- stringr::str_count(context, log_tester$class_name()) > 0
  #
  # expect_true(has_class_info)
  #
  # message <- lines[[2]]
  #
  # log_level <- attr(logger::INFO, "level")
  #
  # has_level_info <- stringr::str_count(message, log_level) > 0
  # expect_true(has_level_info)
  #
  # has_msg_info <- stringr::str_count(message, log_msg) > 0
  # expect_true(has_msg_info)
})

test_that("log_layout_colors", {

  # this test is simply a way to
  # view all logging outputs visually
  # so we can tweak specifics.

  # output <- testthat::capture_output_lines({
  #
  #   log_tester <- LoggingLayoutTester$new()
  #   log_tester$set_logging()
  #
  #   logger::log_threshold(logger::TRACE)
  #
  #   logger::log_trace("trace")
  #   logger::log_info("info")
  #   logger::log_debug("debug")
  #   logger::log_warn("warn")
  #   logger::log_error("error")
  #   logger::log_fatal("fatal")
  #   logger::log_success("success")
  # })
  #
  # print(output)
  #
  # n_lines <- length(output)
  #
  # expect_equal(n_lines, 8)
})
