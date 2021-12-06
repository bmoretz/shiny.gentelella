LogDispatchTester <- R6::R6Class(
  classname = "LogDispatchTester",
  inherit = LogDispatch,

  public = list(

    initialize = function() {
      super$initialize()
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

  #log$trace("test")
})

test_that("can_add_log_level", {
  log <- LogDispatchTester$new()

  log$trace("test")

  level_name(INFO)

  log$trace()
  log$add_log_level(INFO)
  #log$trace("test")
})


new_log_layout(new_fmt_log_level(),
               new_fmt_timestamp(),
               fmt_version,
               new_fmt_line_break(),
               fmt_text1,
               fmt_text2,
               fmt_text3,
               new_fmt_line_break(),
               fmt_machine,
               fmt_nodename,
               fmt_user,
               new_fmt_line_break(),
               fmt_text4,
               fmt_text5,
               fmt_text6,
               sep = '-',
               association = "default")


paste(deparse(as.list(.logcall)[-1][[i]]), params[[i]], sep = ': ')

outer <- function() {
  inner <- function() {
    sys.call(-1)
  }

  inner()
}

deparse_to_one_line <- function(x) {
  gsub('\\s+(?=(?:[^\\\'"]*[\\\'"][^\\\'"]*[\\\'"])*[^\\\'"]*$)', ' ',
       paste(deparse(x), collapse = ' '),
       perl = TRUE)
}

?deparse
outer()


fmt_sysname <- new_fmt_metric(crayon::bold $ green, "sysname")
fmt_release <- new_fmt_metric(crayon::bold $ red, "release")
fmt_seperator <- new_fmt_line_break()
fmt_text1 <- new_fmt_literal(crayon::blue$italic, "literal text")


layout <- new_log_layout(
  new_fmt_log_level(),
  new_fmt_metric(crayon::green$bold, "sysname"),
  new_fmt_metric(crayon::red$bold, "release"),
  new_fmt_metric(crayon::red$bold, "func"),
  new_fmt_line_break(),
  new_fmt_literal(crayon::blue$italic, "literal text"),
  new_fmt_log_msg()
)

context <- sys_context()

test_log <- function() {
  context[["fn"]] <- deparse(sys.call())
  with(context,{
    glue::glue("{fn}")
  })
}

test_log()

log$log(INFO, "log msg", layout)


context[["fn"]]

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

# dispatcher <- structure(function(level, msg,
#                                  ...,
#                                  .logcall = sys.call(),
#                                     .topcall = sys.call(-1),
#                                       .topenv = parent.frame()) {
#
#   with(c(sys_context(), .logcall = .logcall,
#                           .topcall = sys.call(-1),
#                             .topenv = parent.frame()),
#        {
#            glue::glue(..., envir = .topenv)
#     }
#   )
# }, generator = quote(dispatcher))

