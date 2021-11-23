#' @title Initialization
#'
#' Package initialization routine.
#'
#' @param libname library name
#' @param pkgname package name
#' @importFrom config get
#' @importFrom logger log_formatter formatter_glue log_threshold
.onLoad = function (libname, pkgname) {

  logger::log_formatter(logger::formatter_glue)

  config_file <- system.file("config.yml", package = pkgname)

  assign('config', config::get(file = config_file), envir = topenv())
  assign('namespace', pkgname, envir = topenv())

  logger::log_threshold(config$logging$threshold, namespace = pkgname)

  invisible()
}
