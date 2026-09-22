# app-step1-simple.R
#
# Clinical dashboard: step 1- simple dashboard

# Load required packages
library(shiny)
library(ggplot2)
library(DT)
library(haven)

# Source reusable plotting functions
source("functions.R")


# Load dashboard data
dashboard_data <- haven::read_xpt("data/adsl.xpt")

ui <- fluidPage(
  
  titlePanel(
    "Simple Clinical Analytics Dashboard Template"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      h4("Dashboard Information"),
      p("This template uses sample data.")
    ),

    mainPanel(
      
      plotOutput(
        "box_plot",
        height = "450px"
      ),
      
      plotOutput(
        "meanplot",
        height = "700px"
      ),
      
      DTOutput("data_table")
    )
  )
)

server <- function(input, output, session) {
  
  output$box_plot <- renderPlot({
    create_box_plot()
  })
  
  output$meanplot <- renderPlot({
    create_meanplot()
  })
  
  output$data_table <- renderDT({
    
    datatable(
      dashboard_data,
      rownames = TRUE,
      filter = "top",
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
}

shinyApp(
  ui = ui,
  server = server
)