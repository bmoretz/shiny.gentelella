#' @title Singleton
#'
#' @description
#' R6 class that encapsulates the process of setting up
#' an object as singleton instance.
#'
#' @details
#' A singleton is a common object-oriented design
#' pattern \link{https://en.wikipedia.org/wiki/Singleton_pattern}.
#' Even though some refer to this design as an 'anti-pattern, it
#' provides a great deal of utility, especially in the context
#' of building user-interface applications.
#'
#' @docType class
#' @family Design Patterns
#' @export
#' @examples
#' ```{r}
#' ExampleSingleton <- R6::R6Class(
#' classname = "ExampleSingleton",
#' inherit = Singleton,
#' public = list(
#'
#'   initialize = function() {
#'     super$initialize()
#'   },
#'
#'   append = function(item) {
#'     private$items = c(private$items, item)
#'   },
#'
#'   clear = function() {
#'     private$items = list()
#'   },
#'
#'   display = function() {
#'     cat(stringr::str_flatten(private$items, collapse = "\n"))
#'   }
#' ),
#' active = list(),
#' private = list(
#'   items = list()
#' )
#' )
#'
#' s1 <- ExampleSingleton$new(); s2 <- ExampleSingleton$new(); s3 <- ExampleSingleton$new()
#'
#'
#' s1$append("testing from inst 1")
#'
#' s2$append("from inst 2")
#'
#' s3$display()
#'
#' s1$append("inst 1 again")
#'
#' s2$display()
#'
#' ```
#' @importFrom R6 R6Class
Singleton <- R6::R6Class(
  classname = "Singleton",
  cloneable = F,
  public = list(

    #' @description
    #' Initializes a single instance of the type.
    #' @return The `Singleton` object.
    initialize = function() {

      if(is.null(private$public_bind_env)) {
        private$create_singleton(get_class_name())
      } else {
        self <- private$instance
        private$set_instance()
      }
    },

    #' @description
    #' Gets the run-time name of the class.
    #' @return The `Singleton` object.
    class_name = function() {
      private$cls_name
    }
  ),

  private = list(
    cls_name = NULL,

    # overrides from base R6
    public_bind_env = NULL,
    private_bind_env = NULL,

    create_singleton = function(cls_name) {
      # Get type information
      Type <- base::get(cls_name)

      private$public_bind_env <- base::dynGet("public_bind_env")
      private$private_bind_env <- base::dynGet("private_bind_env")

      Type$set('private',
               'public_bind_env',
               private$public_bind_env,
               overwrite = TRUE)

      Type$set('private',
               'private_bind_env',
               private$private_bind_env,
               overwrite = TRUE)

      private$cls_name <- cls_name
    },

    set_instance = function() {
      private$copy_env("public_bind_env",
                       private$public_bind_env)

      private$copy_env("private_bind_env",
                       private$private_bind_env)
    },

    copy_env = function(key, value){
      n <- sys.nframe()
      for(idx in seq_len(n-1)) {
        parent_env <- parent.frame(idx)
        parent_keys <- ls(parent_env)

        if(any(key %in% parent_keys))
          base::assign(key, value, envir = parent_env)
      }

      invisible()
    }
  )
)

