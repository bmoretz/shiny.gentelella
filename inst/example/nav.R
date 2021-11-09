navigation <- yaml::read_yaml("navigation.yml")

menu <- navigation$menu

menu_types <- c('dropdown', 'static')

get_navigation_type <- function(menu_item) {
  attributes <- names(menu_item)
  menu_type <- match(menu_types, attributes)
  invisible(attributes[sum(menu_type, na.rm = T)])
}

create_navigation_menu <- function(menu_item) {
  
  nav_type <- get_navigation_type(menu_item)
  
  switch(nav_type,
         'dropdown' = {
          print('create dropdown')
         },
         'static' = {
           print('create static')
         })
}

create_menu = function(title) {
  ns <- shiny::NS(private$id)
  
  shiny::tags$div(
    id=ns("sidebar-menu"),
    class="main_menu_side hidden-print main_menu"
  )
}

for(index in seq_along(navigation$menu)) {
  
  menu_item <- navigation$menu[[index]]
  
  ui <- create_navigation_menu(menu_item)
}

menu_item <- menu[[1]]

for(idx in seq_along(menu_item$dropdown)) {
  
}

submenu <- menu_item$dropdown[[1]]

for(idx in seq_along(submenu$items)) {
  child_menu <- submenu$items[[idx]]
  print(child_menu)
}


c(
  person(given = "Brandon",
         family = "Moretz",
         role = c("aut", "cre"),
         email = "bmoretz@ionicsolutions.net",
         comment = c(ORCID = "0000-0001-8832-5173")
  ),
  person(
    family = "colorlib", 
    role = c("ctb", "cph"),
    comment = "https://github.com/puikinsh/gentelella")
)