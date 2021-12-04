get_system_info = function() {
  lapply(Sys.info(), FUN = function(var) var)
}

get_r_version = function() {
  c('r_ver' = paste0(R.Version()[c('major', 'minor')], collapse = '.'))
}

sys_context <- function() {
  sys_info <- get_system_info()
  r_ver <- get_r_version()

  sys_context <- c(sys_info, r_ver)

  class(sys_context) <- c('sys_context')

  sys_context
}
