#' Load Log Levels
#'
#' @param file_name loads logging levels from a yml configuration file.
#' @family Logging
#' @return initialized log levels from the specified config.
#' @export
load_log_configuration <- function(file_name, envir = parent.frame()) {

  log_config <- yaml::read_yaml(file_name, eval.expr = T)

  attach_log_levels(log_config$levels, envir)

  Logger$settings <- log_config$settings

  invisible()
}

attach_log_levels <- function(levels, envir) {

  for(level in levels) {

    new_level <- new_log_level(name = level$name,
                               severity = level$severity,
                               log_style = level$log_style,
                               msg_style = level$msg_style)

    assign(level$name, new_level, envir = envir)
  }

  invisible()
}
