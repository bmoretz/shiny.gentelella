library(shiny)

devtools::load_all()

# Run the application 
# shinyApp(ui = ui, server = server)

nav <- NavigationMenu$new("navigation.yml")

html <- nav$construct()
html