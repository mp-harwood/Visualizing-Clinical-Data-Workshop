# app-step2-layout.R
#
# Clinical data dashboard: step 2 - add a layout

# Load required packages
library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(haven)

# Source reusable plotting functions
source("functions.R")

# Load dashboard data
dashboard_data <- haven::read_xpt("data/adsl.xpt")

# Figure choices shown in the sidebar dropdown.
# Add an entry here for each new plot you create in functions.R
figure_choices <- c(
  "Lab Value by Treatment (Box Plot)"        = "box_plot",
  "Mean Change from Baseline (Line Plot)"    = "meanplot"
)

ui <- page_sidebar(
  title = "Clinical Analytics Dashboard",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    title = "Controls",
    width = 280,
    selectInput(
      "figure_choice",
      "Select a figure to display",
      choices = figure_choices
    ),
    hr(),
    p(
      "This template uses sample ADaM data. Swap in your own dataset and",
      "plotting functions in", code("functions.R"), "to adapt it.",
      class = "text-muted small"
    )
  ),
  card(
    full_screen = TRUE,
    card_header("Figure"),
    plotOutput("selected_plot", height = "600px")
  ),
  card(
    full_screen = TRUE,
    card_header("Subject-Level Data"),
    DTOutput("data_table")
  )
)

server <- function(input, output, session) {

  output$selected_plot <- renderPlot({
    switch(
      input$figure_choice,
      box_plot = create_box_plot(),
      meanplot = create_meanplot()
    )
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

shinyApp(ui = ui, server = server)
