# app-step3-interactive.R
#
# Clinical data dashboard: step 3- add interactive components

# Load required packages
library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(DT)
library(haven)
library(dplyr)
library(crosstalk)

# Source reusable plotting functions
source("functions.R")

# Load dashboard data. Wrapped in a crosstalk SharedData object, keyed by
# USUBJID, so the table can be linked to the box plot's per-subject points
crosstalk_group_id <- "clinical_dashboard"
dashboard_data <- haven::read_xpt("data/adsl.xpt")
dashboard_shared <- crosstalk::SharedData$new(
  dashboard_data,
  key   = ~USUBJID,
  group = crosstalk_group_id
)

# Figure choices shown in the sidebar dropdown.
# Add an entry here for each new plot you create in functions.R
figure_choices <- c(
  "Lab Value by Treatment (Box Plot)"     = "box_plot",
  "Mean Change from Baseline (Line Plot)" = "meanplot"
)

# Build lab-parameter dropdown choices from each dataset's PARAMCD/PARAM
get_param_choices <- function(path) {
  df <- haven::read_xpt(path) |>
    distinct(PARAMCD, PARAM) |>
    filter(!startsWith(PARAMCD, "_"))
  setNames(df$PARAMCD, df$PARAM)
}

param_choices <- list(
  box_plot = get_param_choices("data/adlbc.xpt"),
  meanplot = get_param_choices("data/adlbh.xpt")
)
param_defaults <- c(box_plot = "ALB", meanplot = "EOS")

ui <- page_sidebar(
  title = "Clinical Analytics Dashboard",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    title = "Controls",
    width = 300,

    selectInput(
      "figure_choice",
      "Select a figure to display",
      choices = figure_choices
    ),

    # Lab parameter choices depend on which figure is selected, so this
    # dropdown is built dynamically on the server (see `param_ui` below).
    uiOutput("param_ui"),

    checkboxGroupInput(
      "treatments",
      "Treatment groups",
      choices = all_treatments,
      selected = all_treatments
    ),

    actionButton(
      "clear_selection",
      "Clear Selection",
      icon = icon("eraser"),
      class = "w-100 mt-2"
    ),

    hr(),
    p(
      "This template uses sample ADaM data. Swap in your own dataset and",
      "plotting functions in", code("functions.R"), "to adapt it.",
      class = "text-muted small"
    ),
    p(
      "Tip: on the box plot, click a point to highlight that subject's",
      "row(s) in the table below. Double-click the plot to reset it, and use the",
      strong("Clear Selection"), "above to reset the table.",
      class = "text-muted small"
    )
  ),
  card(
    full_screen = TRUE,
    card_header("Figure"),
    plotlyOutput("selected_plot", height = "600px")
  ),
  card(
    full_screen = TRUE,
    card_header("Subject-Level Data"),
    DTOutput("data_table")
  )
)

server <- function(input, output, session) {

  # "Clear Selection" button
  observeEvent(input$clear_selection, {
    session$sendCustomMessage("update-client-value", list(
      group = crosstalk_group_id,
      name  = "selection",
      value = character(0)
    ))
  })

  # Lab-parameter dropdown, rebuilt whenever the selected figure changes
  # so its choices always match the dataset that figure plots.
  output$param_ui <- renderUI({
    selectInput(
      "param_choice",
      "Lab parameter",
      choices  = param_choices[[input$figure_choice]],
      selected = param_defaults[[input$figure_choice]]
    )
  })

  output$selected_plot <- renderPlotly({
    req(input$param_choice)
    validate(
      need(length(input$treatments) > 0, "Select at least one treatment group.")
    )

    if (input$figure_choice == "box_plot") {
      # Per-subject points here share USUBJID with the table's key, so
      # clicking one highlights the matching row(s) below.
      p <- create_box_plot(
        param           = input$param_choice,
        treatments      = input$treatments,
        crosstalk_group = crosstalk_group_id
      )
      ggplotly(p) |>
        highlight(on = "plotly_click", off = "plotly_doubleclick", color = "#e74c3c")
    } else {
      # The mean-change plot shows group-level averages, not individual
      # subjects, so it isn't linked to the table.
      p <- create_meanplot(param = input$param_choice, treatments = input$treatments)
      ggplotly(p)
    }
  })

  # Crosstalk-linked tables require DT's client-side mode (server = FALSE) —
  output$data_table <- renderDT({
    datatable(
      dashboard_shared,
      rownames = TRUE,
      filter = "top",
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  }, server = FALSE)
}

shinyApp(ui = ui, server = server)
