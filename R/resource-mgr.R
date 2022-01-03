#' @title Resource Manager
#'
#' @description
#' R6 class that encapsulates common resource management
#' operations.
#'
#' @details
#' R6 class that is designed to be a singleton (there
#' is only a single instance globally) to provide
#' a clean wrapper over common resource management
#' tasks.
#'
#' @export
ResourceMgr <- R6::R6Class(
  classname = "ResourceMgr",
  public = list(

    #' @description
    #' Create a new UI Page object.
    #' @return A new `ResourceMgr` object.
    initialize = function() {
    },

    #' @description
    #' Maps a package path to a shiny resource path
    #' @param prefix exposed path in the website
    #' @param directory file system directory (mapped in a pkg)
    #' @param warn_empty warn if no files are found.
    map_path = function(prefix, directory, warn_empty = FALSE) {

      mapped_path <- self$map_resource(directory)

      n_files <- length(list.files(path = mapped_path))

      if(n_files == 0 & warn_empty) {
        #log_warn("No resources to add from resource path '{directory}' because it's empty.")
      } else {
        addResourcePath(prefix, mapped_path)
        #log_trace("added resource path: '{directory}' as '{prefix}', {n_files} total resources.")
      }
    },

    #' @description
    #' Maps a package resource to a local file system resource
    #' @param path file system patch
    #' @param package package where the resource lives, if none
    #' specified will use self.
    #' @returns fully qualified file system path
    map_resource = function(path, package = NULL) {
      system.file(path,
                  package = ifelse(is.null(package), namespace, package))
    }
  ),
  private = list()
)
