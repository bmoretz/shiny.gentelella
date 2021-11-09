#' Generates a dashboard navigation menu from a yaml
#' definition file.
#' @export
NavigationMenu <- R6::R6Class(
  classname = "NavigationMenu",
  inherit = UIElement,
  public = list(

    #' @description Initialize a dashboard navigation menu.
    initialize = function(nav_definition) {
      super$initialize()

      private$nav_path <- nav_definition
    },

    ui = function() {}
  ),

  private = list(

    menu_types = c("dropdown",
                  "static"),

    nav_path = NULL,
    menu = NULL,

    parse_navigation = function() {
      tryCatch(
        expr = {
          navigation <- yaml::read_yaml(private$nav_path)
          private$menu <- navigation$menu
        },
        error = function(e) {

        }
      )
    },

    get_navigation_type = function(nav_item) {
      attributes <- names(menu_item)
      menu_type <- match(private$menu_types, attributes)
      invisible(attributes[sum(menu_type, na.rm = T)])
    },

    create_menu = function(menu) {
      ns <- NS(private$id)

      tags$div(
        id=ns("sidebar-menu"),
        class="main_menu_side hidden-print main_menu",
        tagList(
          private$create_menu_sections(menu)
        )
      )
    },

    create_menu_sections = function(menu) {
      for(idx in seq_along(menu)) {

        menu_item <- menu[[idx]]
        menu_style <- ifelse(idx==1, " active", "")

        tagList(
          withTags(
            div(class=glue("menu_section{menu_style}"),
              tagList(
                h3(menu$title),
                ul(class="nav side-menu",
                   tagList(
                     private$create_menu_item(menu_item)
                   )
                )
              )
            )
          )
        )
      }
    },

    create_menu_item = function(menu_item) {
      nav_type <- get_navigation_type(menu_item)

      switch(nav_type,
             'dropdown' = {
               private$handle_dropdown(menu_item)
             },
             'static' = {
               private$hanle_static(menu_item)
             })
    },

    handle_dropdown = function(menu_item) {
      submenu <- menu_item$dropdown[[1]]

      li(class="",
        taglist(
          a(
            taglist(
              i(class=glue("fa {submenu$class}"),
                submenu$text
              ),
              span(class="fa fa-chevron-down")
            )
          ),
          tagList(
            private$create_children(submenu)
          )
        )
      )
    },

    create_children = function(submenu) {
      for(idx in seq_along(submenu$items)) {
        child_menu <- submenu$items[[idx]]

        ul(class="nav child_menu",
           style="display: block;",
           tagList(
             li(class="current-page",
                a(href=child_menu$url,
                  child_menu$text)
             )
           )
        )
      }
    },

    handle_static = function(element) {

    }
  )
)
