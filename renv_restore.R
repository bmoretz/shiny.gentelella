restore_local_renv <- function(package_name) {

  devtools::load_all()

  packages <- devtools::package_info()
  package <- packages[packages$package == "shiny.gentelella",]

  version <- package$loadedversion

  devtools::build()

  build_result <- here::here(glue::glue("{package_name}_{version}.tar.gz"))

  dir_env_local <- here::here("renv", "local")

  existing_files <- list.files(dir_env_local)

  for(file in existing_files) {
    file.remove(file.path(dir_env_local, file))
  }

  stopifnot(file.copy(build_result, dir_env_local))

  renv::restore(prompt = F)

  stopifnot(file.remove(build_result))
}

restore_local_renv(package_name = "shiny.gentelella")
