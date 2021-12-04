LogDispatchTester <- R6::R6Class(
  classname = "LogDispatchTester",
  inherit = LogDispatch,

  public = list(

    initialize = function() {
    },

    get_system_metrics = function() {
      private$system_context
    }

  ),
  private = list()
)

test_that("log_single_instance", {

  inst_n <- LogDispatch$new()
  inst_m <- LogDispatch$new()

  expect_true(identical(inst_n, inst_m))
})

test_that("has_system_metrics", {

  log <- LogDispatchTester$new()

  expect_named(log$system['sysname'])
  expect_gt(nchar(log$system['sysname']), 0)

  expect_named(log$system['release'])
  expect_gt(nchar(log$system['release']), 0)

  expect_named(log$system['version'])
  expect_gt(nchar(log$system['version']), 0)

  expect_named(log$system['nodename'])
  expect_gt(nchar(log$system['nodename']), 0)

  expect_named(log$system['machine'])
  expect_gt(nchar(log$system['machine']), 0)

  expect_named(log$system['login'])
  expect_gt(nchar(log$system['login']), 0)

  expect_named(log$system['user'])
  expect_gt(nchar(log$system['user']), 0)

  expect_named(log$system['r-ver'])
  expect_gt(nchar(log$system['r-ver']), 0)
})

test_that("log_dispatch_01", {
  log <- LogDispatchTester$new()

  log$trace("test")
})

# fmt_sysname <- new_fmt_metric(crayon::bold $ green, "sysname")
# fmt_release <- new_fmt_metric(crayon::bold $ red, "release")
# fmt_seperator <- new_fmt_line_break()
# fmt_text1 <- new_fmt_literal(crayon::bold $ blue, "literal text")
#
# layout <- new_log_layout(fmt_sysname,
#                          fmt_release,
#                          fmt_seperator,
#                          fmt_text1)
#
# expect_equal(length(layout), 4)
#
# evaluated <- value(layout)
#
# level <- INFO
#
# dispatcher <- function(level, msg) {
#
#   system_context <- sys_context()
#
#   structure(function(level, msg, namespace = NA_character_,
#                      .logcall = sys.call(), .topcall = sys.call(-1), .topenv = parent.frame()) {
#
#     if(!inherits(level, 'log_level')) {
#       unknown_severity_warning(level)
#     }
#
#     with(system_context(log_level = level, namespace = namespace,
#                                 .logcall = .logcall, .topcall = .topcall, .topenv = .topenv),
#       glue::glue(evaluated, envir = .topenv)
#     )
#
#   }, generator = deparse(match.call()))
# }
#
# dispatch(INFO, "test")

dispatcher <- structure(function(level, msg,
                                 ...,
                                 .logcall = sys.call(),
                                    .topcall = sys.call(-1),
                                      .topenv = parent.frame()) {

  with(c(sys_context(), .logcall = .logcall,
                          .topcall = sys.call(-1),
                            .topenv = parent.frame()),
       {
           glue::glue(..., envir = .topenv)
    }
  )
}, generator = quote(dispatcher))


fmt_sysname <- new_fmt_metric(crayon::green$bold, "sysname")
fmt_release <- new_fmt_metric(crayon::red$bold, "release")
fmt_seperator <- new_fmt_line_break()
fmt_level <- new_fmt_log_level()
fmt_text1 <- new_fmt_literal(crayon::blue$italic, "literal text")
fmt_timestamp <- new_fmt_timestamp(crayon::silver$italic, "%x %H:%M:%S %z")
fmt_msg <- new_fmt_log_msg()

layout <- new_log_layout(fmt_sysname,
                         fmt_release,
                         fmt_seperator,
                         fmt_level,
                         fmt_timestamp,
                         fmt_text1,
                         fmt_msg)

evaluated <- value(layout)

dispatcher(TRACE, "log msg", evaluated)


ivory <- crayon::make_style("ivory")
bgMaroon <- crayon::make_style("maroon", bg = TRUE)
fancy <- crayon::combine_styles(crayon::blurred, ivory)

cat(
  fancy("This will have some fancy colors"),
  "\n",
  ivory("This will have some fancy colors")
)

?crayon::blurred

