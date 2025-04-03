# app.R
# Main application file for Supply Chain Dashboard

# Load global functions and libraries
source("global.R")

# Load UI and server definitions
source("ui.R")
source("server.R")

# Create and launch Shiny app
shinyApp(ui = ui, server = server)