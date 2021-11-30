LogLayoutTester <- R6::R6Class(
  classname = "LogLayoutTester",
  inherit = LogLayout,

  public = list(

    initialize = function() {

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

    get_default_palette = function() {
      private$default_palette()
    }
  ),

  private = list()
)

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

#
# test_that("log_layout_trace", {
#   expect_null(display_log_layout("this is a trace msg layout"))
# })
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

test_that("log_output_format", {


  # fmt_sysname <- new_fmt_metric(crayon::combine_styles(crayon::italic,
  #                                                      crayon::make_style("darkorange")),
  #                               "sysname")
  #
  # fmt_release <- new_fmt_metric(bold $ red,
  #                                 metric = 'release')
  #
  # fmt_seperator <- new_fmt_line_break()
  #
  # fmt_text1 <- new_fmt_literal(crayon::blue $ italic, "literal text")
  #
  # class(fmt_seperator)[1]
  # sysname <- "Linuix"
  # release <- "Ubuntu"
  #
  # generated <- list(fmt_sysname,
  #                   fmt_release,
  #                   fmt_seperator,
  #                   fmt_text1)
  #
  #
  # partition <- function(layout) {
  #
  #   generated[] ==
  #
  #   seperators <- sapply(generated,
  #                        function(e) e != fmt_newline)
  #
  #   groups <- partition(seperators)
  #   format <- lapply(groups, function(size){
  #     vector(mode="character", length = size)
  # })
  #
  # row <- 1; col <- 1
  # # set fmt objects
  # for(fmt_obj in generated) {
  #
  #     fmt_type <- class(fmt_obj)
  #
  #     switch(fmt_type,
  #        "fmt_metric" = {
  #          style <- attr(fmt_obj, 'style')
  #          metric <- attr(fmt_obj, 'metric')
  #          format[[row]][[col]] <- style(metric)
  #        },
  #        "fmt_literal" = {
  #          style <- attr(fmt_obj, 'style')
  #          literal <- attr(fmt_obj, 'value')
  #          format[[row]][[col]] <- style(literal)
  #        }
  #     )
  # }
  #
  # fmt <- paste(msg, collapse = "")
  #
  # Logger$trace("testing", line)
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
