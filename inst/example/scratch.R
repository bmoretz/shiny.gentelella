library(shiny)

devtools::load_all()

menu_types = c("dropdown",
               "static")

create_container = function(menu) {
  ns <- shiny::NS(uuid::UUIDgenerate())
  
  logger::log_info("create container")
  
  div(
    id=ns("sidebar-menu"),
    class="main_menu_side hidden-print main_menu",
    tagList(
      create_menu_sections(menu)
    )
  )
}

create_menu_sections = function(menu) {
  
  sections <- tagList()
  
  for(idx in seq_along(menu)) {
    
    menu_item <- menu[[idx]]
    menu_style <- ifelse(idx==1, " active", "")
    
    section <- tagList(
      withTags(
        div(class=glue::glue("menu_section{menu_style}"),
            tagList(
              h3(menu_item$title),
              ul(class="nav side-menu",
                 create_menu_item(menu_item)
              )
            )
        )
      )
    )
    
    sections <- c(sections, section)
  }
  
  sections
}

get_navigation_type = function(nav_item) {
  attributes <- names(nav_item)
  menu_type <- match(menu_types, attributes)
  invisible(attributes[sum(menu_type, na.rm = T)])
}

create_menu_item = function(menu_item) {
  nav_type <- get_navigation_type(menu_item)
  
  menu <- switch(nav_type,
         'dropdown' = {
           handle_dropdown(menu_item)
         },
         'static' = {
           handle_static(menu_item)
         })
  
  print(menu_item)
  
  menu
}

handle_dropdown = function(menu_item) {
  
  submenu <- menu_item$dropdown[[1]]
  
  tags$li(
    tagList(
      tags$a(
        tagList(
          tags$i(class=glue::glue("fa {submenu$class}"),
            submenu$text
          ),
          tags$span(class="fa fa-chevron-down")
        )
      ),
      tags$ul(class="nav child_menu",
         style="display: block;",
         tagList(
           populate_dropdown(submenu$items)
         )
      )
    )
  )
}

handle_static = function(element) {
  
}

populate_dropdown = function(dropdown_items) {
  
  for(idx in seq_along(dropdown_items)) {
    
    child_item <- dropdown_items[[idx]]
    
    tags$li(
      tags$a(href=child_item$url,
        child_item$text)
    )
  }
}

navigation_path <- "navigation.yml"
navigation <- yaml::read_yaml(navigation_path)
     
html <- create_container(menu)
html

