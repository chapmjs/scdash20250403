# ui.R
# User interface for Supply Chain Dashboard

ui <- fluidPage(
  useShinyjs(),
  
  # Custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  # Dashboard title
  titlePanel("Supply Chain Dashboard"),
  
  # Main panel with dashboard content
  fluidRow(
    column(width = 12,
           # Title block that shows Part # (DESCRIPTION) in capital letters
           div(
             id = "title-block",
             textOutput("titleBlock")
           ),
           
           # Date information section
           fluidRow(
             column(width = 4, 
                    div(class = "date-section",
                        h4("Current Month:"),
                        textOutput("currentMonthDisplay")
                    )
             ),
             column(width = 8)  # Placeholder for spacing
           ),
           
           # Part selection and data controls
           fluidRow(
             column(width = 4,
                    div(class = "control-panel",
                        selectInput("partNumber", "Part Number:", choices = NULL),
                        div(class = "button-row",
                            actionButton("retrieveData", "Retrieve Data", class = "btn-primary action-button"),
                            actionButton("saveData", "Save/Update Data", class = "btn-success action-button")
                        )
                    )
             ),
             column(width = 4,
                    div(class = "forecast-selector",
                        selectInput("forecastType", "Forecast Type for Shortage Warning:", 
                                    choices = c("High", "Expected", "Low"),
                                    selected = "Expected")
                    )
             ),
             column(width = 4)  # Placeholder for future controls
           ),
           
           # Main dashboard content
           fluidRow(
             # Left side - Part information
             column(width = 3,
                    div(class = "part-info-panel",
                        h4("Part Information"),
                        
                        div(class = "info-row",
                            div(class = "info-label", "Part Description:"),
                            textInput("partDescription", NULL, placeholder = "Description", width = "100%")
                        ),
                        
                        div(class = "info-row",
                            div(class = "info-label", "Supplier:"),
                            textInput("supplier", NULL, placeholder = "Supplier", width = "100%")
                        ),
                        
                        div(class = "info-row",
                            div(class = "info-label", "Lead Time:"),
                            textInput("leadTime", NULL, placeholder = "Lead Time", width = "100%")
                        ),
                        
                        div(class = "info-row",
                            div(class = "info-label", "Unit Price:"),
                            textInput("unitPrice", NULL, placeholder = "Price", width = "100%")
                        ),
                        
                        div(class = "info-row",
                            div(class = "info-label", "Part Yield:"),
                            textInput("partYield", NULL, placeholder = "Yield", width = "100%")
                        )
                    )
             ),
             
             # Right side - Monthly data table
             column(width = 9,
                    div(class = "data-panel",
                        h4("Supply Chain Data"),
                        DTOutput("monthlyDataTable")
                    )
             )
           ),
           
           # Shortage warning section
           fluidRow(
             column(width = 12,
                    div(class = "shortage-panel",
                        h4("Supply Shortage Warning (Next 6 Months)"),
                        DTOutput("shortageWarningTable")
                    )
             )
           )
    )
  ),
  
  # Footer with instructions
  tags$footer(
    div(class = "footer-content",
        p("Instructions:"),
        tags$ul(
          tags$li("Select a part number and click 'Retrieve Data' to load information."),
          tags$li("Green cells can be edited. Click to modify values."),
          tags$li("Click 'Save/Update Data' to write changes back to the source data."),
          tags$li("The shortage warning shows projected supply vs. demand for the next 6 months.")
        )
    )
  )
)