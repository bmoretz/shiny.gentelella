library(shiny)
library(shiny.gentelella)

source(here::here("inst", "example", "example-dashboard.R"))

example <- ExampleDashboard$new()

resource_mgr$map_path("assets", "theme/assets")
resource_mgr$map_path("scripts", "theme/scripts")
resource_mgr$map_path("images", "theme/images")

# Run the application
app <- shinyApp(ui = example$ui(),
                server = example$server)

runApp(app, port = 8081)

