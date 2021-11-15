#' Generates a dashboard navigation menu from a yaml
#' definition file.
#' @export
NavigationMenu <- R6::R6Class(
  classname = "NavigationMenu",
  inherit = Control,
  public = list(

    #' @description
    #' Create a navigation menu based
    #' on the structure of the yml
    #' definition file.
    #' @param nav_definition the yml file that specifies how the
    #' navigation menu should be laid out.
    #' @return A new `NavigationMenu` object.
    initialize = function(nav_definition) {
      super$initialize()

      private$nav_definition <- nav_definition
    },

    construct = function() {

      logger::log_layout(self$log_layout(), namespace = namespace)

      logger::log_trace("parsing navigation menu yml...")
      navigation_menu <- private$parse_navigation()

      logger::log_trace("creating navigation menu")
      private$create_container(navigation_menu)
    },

    ui = function() {}
  ),

  private = list(

    menu_types = c("dropdown",
                  "static"),

    nav_definition = NULL,

    parse_navigation = function() {
      tryCatch(
        expr = {

          logger::log_trace("parsing navigation definition: '{private$nav_definition}'")

          yaml::read_yaml(private$nav_definition)

        },
        error = function(e) {

        }
      )
    },

    create_container = function(menu) {

      logger::log_trace("creating menu container")

      ns <- NS(private$id)

      div(
        id=ns("sidebar-menu"),
        class="main_menu_side hidden-print main_menu",
        tagList(
          private$create_menu_sections(menu)
        )
      )
    },

    create_menu_sections = function(menu) {

      sections <- tagList()

      for(idx in seq_along(menu)) {

        menu_item <- menu[[idx]]
        menu_style <- ifelse(idx==1, " active", "")

        logger::log_trace("creating menu section: '{menu_item$title}'")

        section <- tagList(
          withTags(
            div(class=glue::glue("menu_section{menu_style}"),
                tagList(
                  h3(menu_item$title),
                  ul(class="nav side-menu",
                     private$create_menu_item(menu_item)
                  )
                )
            )
          )
        )

        sections <- c(sections, section)
      }

      print(sections)

      sections
    },

    get_navigation_type = function(nav_item) {
      attributes <- names(nav_item)
      menu_type <- match(private$menu_types, attributes)
      invisible(attributes[sum(menu_type, na.rm = T)])
    },

    create_menu_item = function(menu_item) {

      nav_type <- private$get_navigation_type(menu_item)
      logger::log_trace("creating menu item of type: {nav_type}")

      menu <- switch(nav_type,
         'dropdown' = {
           private$handle_dropdown(menu_item)
         },
         'static' = {
           private$hanle_static(menu_item)
         })

      print(menu)

      menu
    },

    handle_dropdown = function(menu_item) {

      submenu <- menu_item$dropdown[[1]]

      logger::log_trace("creating drop-down menu: {submenu$text}")

      withTags(
        li(
          tagList(
            a(
              tagList(
                i(class=paste("fa", submenu$class),
                  submenu$text
                ),
                span(class="fa fa-chevron-down")
              )
            ),
            ul(class="nav child_menu",
               style="display: block;",
                private$populate_dropdown(submenu$items)
            )
          )
        )
      )
    },

    populate_dropdown = function(dropdown_items) {

      items <- tagList()

      logger::log_trace("populating drop-down items")

      for(idx in seq_along(dropdown_items)) {

        child_item <- dropdown_items[[idx]]

        logger::log_trace("creating drop-down item: {child_item$text}")

        items <- c(items,
                   withTags(
                     li(
                       a(href=child_item$url,
                         child_item$text))
                   ))
      }

      items
    },

    handle_static = function(menu_item) {

      logger::log_trace("creating static menu: {submenu$text}")

      withTags(
        li(
          a(href=menu_item$href,
            menu_item$url)
        )
      )
    }
  )
)
