#' Global Application
App <- NULL; LogLayouts <- NULL; Logger <- NULL

#' @title Initialization
#'
#' Package initialization routine.
#'
#' @param libname library name
#' @param pkgname package name
#' @importFrom config get
.onLoad = function (libname, pkgname) {

  config_file <- system.file("config.yml", package = pkgname)

  assign('config', config::get(file = config_file), envir = topenv())
  assign('namespace', pkgname, envir = topenv())

  #log_threshold(config$logging$threshold, namespace = pkgname)

  assign('App', Application$new(), envir = topenv())
  lockBinding('App', env = topenv())

  assign('LogLayouts', LogLayout$new(), envir = topenv())
  lockBinding('LogLayouts', env = topenv())

  assign('Logger', LogDispatch$new(), envir = topenv())
  lockBinding('Logger', env = topenv())

  invisible()
}
