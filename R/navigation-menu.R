#' @title Navigation Menu
#'
#' @description
#' Generates a dashboard navigation menu from a yaml
#' definition file.
#'
#' @export
#' @importFrom glue glue
#' @importFrom yaml read_yaml
#' @importFrom logger log_trace log_info log_debug log_warn log_error log_success
#' @importFrom shiny NS tags tagList withTags
#' @importFrom shiny div span h3 a
NavigationMenu <- R6::R6Class(
  classname = "NavigationMenu",
  inherit = Control,
  public = list(

    #' @description
    #' Create a navigation menu based
    #' on the structure of the yml
    #' definition file.
    #' @param nav_definition menu navigation file (yml)
    #' @return A new `NavigationMenu` object.
    initialize = function(nav_definition) {
      super$initialize()
      private$nav_definition <- nav_definition
    },

    #' @description
    #' Constructs the navigation menu UI
    #' from yml definition.
    ui = function() {

      private$set_logging()

      if(!private$constructed) {
        private$construct()
      }

      private$html
    }
  ),

  private = list(

    menu_types = c("dropdown",
                  "static"),

    html = NULL,
    nav_definition = NULL,
    constructed = F,

    construct = function() {

      Logger$trace("creating navigation menu: {private$nav_definition}")
      # parse the menu definition from yml
      navigation <- private$parse_navigation(private$nav_definition)

      # convert from object representation to html
      private$html <- private$create_container(navigation)
      private$constructed <- T

      invisible(self)
    },

    parse_navigation = function(nav_file) {
      tryCatch(
        expr = {

          if(!file.exists(nav_file))
            stop(glue::glue("unable to locate menu definition file: {nav_file}"))

          logger::log_trace("parsing navigation definition: '{nav_file}'")
          structure <- yaml::read_yaml(nav_file)
          logger::log_success("parsed menu navigation file")
          invisible(structure$menu)
        },
        error = function(c) {
          logger::log_error(c)
        },
        warning = function(c) {
          logger::log_warn(c)
        },
        message = function(c) {
          logger::log_info(c)
        }
      )
    },

    create_container = function(menu) {

      logger::log_trace("creating menu container")

      ns <- shiny::NS(private$id)

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
           private$handle_static(menu_item)
         })

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

        item <- tagList(
          withTags(
            li(class="sub_menu",
              a(href=child_item$url,
                  child_item$text)
            )
          )
        )

        items <- c(items, item)
      }

      items
    },

    handle_static = function(menu_item) {

      static_item <- menu_item$static[[1]]

      logger::log_trace("creating static menu: {static_item$text}")

      submenu_item <- static_item$item[[1]]

      withTags(
        li(class="sub_menu",
            a(href=submenu_item$url,
              tagList(
                i(class=paste("fa", static_item$class)),
                static_item$text,
                span(class="label label-success pull-right",
                     submenu_item$text)
              )
            )
          )
        )
    }
  )
)
