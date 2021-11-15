get_build_output_path <- function() {
  invisible(dirname(here::here()))
}

get_local_pkg_name <- function() {
  package_dir <- here::here()
  working_dir <- paste0(dirname(current_dir), .Platform$file.sep)

  invisible(stringi::stri_replace_first_fixed(package_dir, working_dir, ""))
}

build_local <- function() {

  pkg_name <- get_local_pkg_name()

  devtools::load_all()

  packages <- devtools::package_info()
  package <- packages[packages$package == pkg_name,]

  version <- package$loadedversion

  devtools::build()

  build_result <- file.path(get_build_output_path(), glue::glue("{pkg_name}_{version}.tar.gz"))

  stopifnot(file.exists(build_result))

  invisible(build_result)
}

renv_restore_local <- function() {
  build_output <- build_local()

  dir_env_local <- here::here("renv", "local")

  existing_files <- list.files(dir_env_local)

  for(file in existing_files) {
    file.remove(file.path(dir_env_local, file))
  }

  stopifnot(file.copy(build_result, dir_env_local))

  renv::restore(prompt = F)

  stopifnot(file.remove(build_result))
}

renv::install(build_local())
