# Visualizing Clinical Data: From Publication-Quality Figures to Interactive Shiny Dashboards

2026-10-28

- [Overview](#overview)
- [R Packages](#r-packages)
- [Example Data](#example-data)
- [Part 1: Producing Visualizations](#part-1-producing-visualizations)
  - [Step 1: Prepare Lab Data for
    Plotting](#step-1-prepare-lab-data-for-plotting)
  - [Step 2: Build a Box Plot of Lab Values by Treatment and
    Visit](#step-2-build-a-box-plot-of-lab-values-by-treatment-and-visit)
    - [Customize: Show Every Subject’s
      Value](#customize-show-every-subjects-value)
  - [Step 3: Build a Mean Change from Baseline
    Plot](#step-3-build-a-mean-change-from-baseline-plot)
    - [Customize: Add reference line](#customize-add-reference-line)
    - [Customize: Distinguish Treatments with Line
      Type](#customize-distinguish-treatments-with-line-type)
    - [Customize: Nudge Points So They Don’t
      Overlap](#customize-nudge-points-so-they-dont-overlap)
    - [Customize: Anchor the Baseline Point on the
      Axis](#customize-anchor-the-baseline-point-on-the-axis)
  - [Step 4: Wrap Into Reusable
    Functions](#step-4-wrap-into-reusable-functions)
- [Part 2: Creating R Shiny
  Dashboards](#part-2-creating-r-shiny-dashboards)
  - [Shared Plotting Functions
    (`functions.R`)](#shared-plotting-functions-functionsr)
  - [Step 1: A Simple Dashboard](#step-1-a-simple-dashboard)
  - [Step 2: Add a Polished Layout](#step-2-add-a-polished-layout)
  - [Step 3: Add Interactivity](#step-3-add-interactivity)
    - [Step 3a: Make Figures Interactive with
      plotly](#step-3a-make-figures-interactive-with-plotly)
    - [Step 3b: Add Filtering Controls](#step-3b-add-filtering-controls)
    - [Step 3c: Link the Plot and Table with
      crosstalk](#step-3c-link-the-plot-and-table-with-crosstalk)

Michelle Harwood\
Alexion, AstraZeneca Rare Disease\
michelle.harwood@alexion.com

## Overview

This workshop takes a two-part approach to clinical data visualization,
moving from high-quality static figures to interactive, searchable
dashboards. 🚀

- 🎨 **Part 1 — Static figures:** Use {ggplot2} to build
  publication-quality visualizations for commonly used clinical plots.
  Develop reusable plotting code for consistent, reproducible outputs
  across a study.

  1.  [Prepare Lab Data for
      Plotting](#step-1-prepare-lab-data-for-plotting)
  2.  [Build a Box Plot of Lab Values by Treatment and
      Visit](#step-2-build-a-box-plot-of-lab-values-by-treatment-and-visit)
  3.  [Build a Mean Change from Baseline
      Plot](#step-3-build-a-mean-change-from-baseline-plot)
  4.  [Wrap Into Reusable
      Functions](#step-4-wrap-into-reusable-functions)

- 🖥️ **Part 2 — Interactive dashboards:** Build the same clinical
  dashboard three times over, in three separate app files, each one
  adding a layer of capability on top of the last.

  1.  [A Simple Dashboard](#step-1-a-simple-dashboard)
  2.  [Add a Polished Layout](#step-2-add-a-polished-layout)
  3.  [Add Interactivity](#step-3-add-interactivity)
      - [Make Figures Interactive with
        plotly](#step-3a-make-figures-interactive-with-plotly)
      - [Add Filtering Controls](#step-3b-add-filtering-controls)
      - [Link the Plot and Table with
        crosstalk](#step-3c-link-the-plot-and-table-with-crosstalk)

------------------------------------------------------------------------

## R Packages

The following R packages are used throughout this workshop:

``` r
library(dplyr)
library(haven)
library(scales)

# Part 1: static visualizations
library(ggplot2)

# Part 2: interactive dashboards
library(shiny)
library(bslib)
library(plotly)
library(DT)
library(crosstalk)
```

## Example Data

The example [CDISC Pilot
data](https://github.com/cdisc-org/sdtm-adam-pilot-project/tree/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets)
can be used to follow along. This workshop starts from the pre-built
ADaM datasets (`adsl`, `adlbc`, `adlbh`) so we can focus on
visualization. Load them as follows:

``` r
library(haven)

adsl  <- read_xpt("data/adsl.xpt")  
adlbc <- read_xpt("data/adlbc.xpt")
adlbh <- read_xpt("data/adlbh.xpt")
```

------------------------------------------------------------------------

# Part 1: Producing Visualizations

Part 1 builds two figures: a box plot of a lab value by treatment and
visit, and a mean-change-from-baseline line plot.

## Step 1: Prepare Lab Data for Plotting

Before plotting, filter down to a single parameter, keep only real
analysis records, and merge in treatment group from `adsl`.

A palette and treatment list, shared by both figures, defined once up
front:

``` r
library(dplyr)

trt_colors <- c(
  "Placebo"              = "#7F7F7F",  
  "Xanomeline Low Dose"  = "#0072B2", 
  "Xanomeline High Dose" = "#E69F00" 
)

all_treatments <- names(trt_colors)
```

Data for the box plot — Albumin (`ALB`) by treatment and visit:

``` r
keep_visits <- c("Baseline", "Week 2", "Week 4", "Week 8", "Week 12")
param <- "ALB"

lab_by_visit <- adlbc |>
  mutate(AVISIT = sub("^\\s+", "", AVISIT)) |>  # the pilot data pads AVISIT with leading spaces
  filter(PARAMCD == param, is.na(ANL01FL) | ANL01FL == "Y" | ABLFL == "Y") |>
  filter(!is.na(AVAL)) |>
  left_join(adsl, by = "USUBJID", suffix = c(".adlb", ".adsl")) |>
  filter(!is.na(TRTA), TRTA %in% all_treatments) |>
  filter(AVISIT %in% keep_visits) |>
  mutate(
    TRTA   = factor(TRTA, levels = all_treatments), # maintain consistent order
    AVISIT = factor(AVISIT, levels = keep_visits)
  )
```

> **Tip:** `ANL01FL`/`ABLFL` are ADaM “analysis flags” marking which
> records are the intended one-per-subject-per-visit analysis value —
> without this filter, a subject with a repeated or unscheduled
> measurement at a visit could contribute more than one point to that
> visit.

------------------------------------------------------------------------

## Step 2: Build a Box Plot of Lab Values by Treatment and Visit

Start with the box plot itself: one box per treatment arm, faceted by
visit — faceting by visit, rather than mapping it to a color or shape,
keeps each visit’s treatment comparison self-contained and easy to scan
left-to-right over time.

``` r
library(ggplot2)

param_label <- adlbc |> distinct(PARAMCD, PARAM) |> filter(PARAMCD == param) |> pull(PARAM)

p_box <- ggplot(lab_by_visit, aes(x = TRTA, y = AVAL)) +
  geom_boxplot(aes(color = TRTA), show.legend = FALSE, outlier.shape = NA) +
  facet_wrap(~ AVISIT, nrow = 1) +
  labs(x = NULL, y = param_label[1]) +
  scale_color_manual(values = trt_colors) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    axis.title       = element_text(face = "bold"),
    strip.background = element_blank(),
    legend.position  = "top",
    legend.title     = element_blank()
  ) +
  guides(linetype = guide_legend(override.aes = list(color = "black")))

p_box
```

### Customize: Show Every Subject’s Value

A box-and-whisker summary hides how many subjects are behind it and
whether the distribution is lumpy, skewed, or driven by a couple of
outliers. Overlaying every subject as a jittered point makes that
visible, at the cost of a busier plot. Since `p_box` already exists,
only the new layer needs to be added:

``` r
p_box <- p_box +
  geom_point(
    aes(color = TRTA),
    position = position_jitter(width = 0.2), alpha = 0.4, size = 1, show.legend = FALSE
  )

p_box
```

------------------------------------------------------------------------

## Step 3: Build a Mean Change from Baseline Plot

``` r
keep_visits <- c("Baseline", "Week 2", "Week 4", "Week 8", "Week 12")
xticks       <- c(0, 2, 4, 8, 12)
param       <- "EOS"

chg_by_visit <- adlbh |>
  mutate(AVISIT = sub("^\\s+", "", AVISIT)) |>
  filter(PARAMCD == param) |>
  mutate(CHG = AVAL - BASE) |>
  filter(!is.na(TRTA), TRTA %in% all_treatments) |>
  filter(AVISIT %in% keep_visits) |>
  mutate(
    TRTA = factor(TRTA, levels = all_treatments),
    # Turn "Baseline"/"Week N" labels into a numeric study week for the x-axis
    week_n = case_when(
      grepl("^\\s*baseline\\s*$", AVISIT, ignore.case = TRUE) ~ 0,
      grepl("^\\s*week\\b", AVISIT, ignore.case = TRUE) ~
        suppressWarnings(as.numeric(sub("^\\s*Week\\s*([0-9]+).*$", "\\1", AVISIT, ignore.case = TRUE))),
      TRUE ~ NA_real_
    )
  )

param_label <- adlbh |> distinct(PARAMCD, PARAM) |> filter(PARAMCD == param) |> pull(PARAM)

visit_summary <- chg_by_visit |>
  group_by(TRTA, week_n) |>
  summarise(n = n(), mean_chg = mean(CHG, na.rm = TRUE), sd_chg = sd(CHG, na.rm = TRUE), .groups = "drop") |>
  mutate(
    se_chg   = ifelse(is.na(sd_chg / sqrt(n)), 0, sd_chg / sqrt(n)),
    mean_chg = ifelse(week_n == 0, 0, mean_chg),  # baseline is 0 by definition
    se_chg   = ifelse(week_n == 0, 0, se_chg)
  ) |>
  arrange(week_n)
```

With the summary table ready, a first pass just plots mean change over
time by treatment, colored by `TRTA`:

``` r
p_mean <- ggplot(visit_summary, aes(x = week_n, y = mean_chg, color = TRTA, group = TRTA)) +
  geom_line(linewidth = 1, alpha = 0.7) +
  geom_point(size = 3, alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_chg - se_chg, ymax = mean_chg + se_chg), width = 0.5, alpha = 0.7) +
  labs(
    x = "Weeks Since Baseline Measurement",
    y = paste0(param_label, "\nChange from Baseline (Mean \u00b1 SE)"),
    color = "Treatment"
  ) +
  scale_color_manual(values = trt_colors) +
  scale_x_continuous(breaks = xticks) +
  theme_classic(base_size = 12) +
  theme(axis.title = element_text(face = "bold"), legend.position = "top")

p_mean
```

There are a number of customization options we can add. Not limited to
these, we will show 1) how to add a dotted reference line, 2) change
line type, 3) nudge points, and 4) anchor baseline on y-axis

### Customize: Add reference line

``` r
p_mean <- p_mean +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", linewidth = 0.6)
p_mean
```

### Customize: Distinguish Treatments with Line Type

Mapping `TRTA` to `linetype` as well as `color` means the lines are
still distinguishable even if colors are hard to tell apart (printed in
grayscale, or for colorblind readers).

``` r
p_mean <- p_mean +
  aes(linetype = TRTA) +
  labs(linetype = "Treatment")

p_mean
```

### Customize: Nudge Points So They Don’t Overlap

Line type alone doesn’t fix the visual overlap of the points and error
bars themselves. Adding a small, treatment-specific horizontal offset
(`nudge`) spreads them apart at each visit.

``` r
nudge_vals <- c("Placebo" = -0.3, "Xanomeline Low Dose" = 0.0, "Xanomeline High Dose" = 0.3)

visit_summary <- visit_summary |>
  mutate(nudge = nudge_vals[as.character(TRTA)])

p_mean <- p_mean +
  visit_summary +          # swap in the data now that it has a `nudge` column
  aes(x = week_n + nudge)  # shift each treatment's x position by its nudge

p_mean
```

Better at Weeks 2–12 — but now baseline is nudged apart too, even though
all three arms are exactly 0 there by definition. Spreading out a point
that’s supposed to be identical across arms is misleading.

### Customize: Anchor the Baseline Point on the Axis

The fix is to only apply the nudge when `week_n != 0`, so baseline stays
a single point sitting directly on the y-axis, and only the follow-up
visits spread apart. This is also a natural point to swap the legend for
direct end-of-line labels, since there are only three groups.

``` r
p_mean <- p_mean +
  aes(x = ifelse(week_n == 0, week_n, week_n + nudge))+  # only non-baseline visits get nudged
  scale_x_continuous(breaks = xticks, expand = c(0, 0)) +
  coord_cartesian(xlim = c(0, max(xticks) + 0.5), clip = "off")
  
p_mean
```

------------------------------------------------------------------------

## Step 4: Wrap Into Reusable Functions

Both figures currently hardcode which lab parameter and treatment arms
they show. Turning `param`/`treatments` into function arguments —
instead of fixed values in the data-prep code — is what will let Part
2’s dashboard controls drive these plots directly.

``` r
create_box_plot <- function(param = "ALB", treatments = all_treatments) {
  keep_visits <- c("Baseline", "Week 2", "Week 4", "Week 8", "Week 12")

  lab_by_visit  <- adlbc |>
    mutate(AVISIT = sub("^\\s+", "", AVISIT)) |>
    filter(PARAMCD == param, is.na(ANL01FL) | ANL01FL == "Y" | ABLFL == "Y") |>
    filter(!is.na(AVAL)) |>
    left_join(adsl, by = "USUBJID", suffix = c(".adlb", ".adsl")) |>
    filter(!is.na(TRTA), TRTA %in% treatments) |> # This should not be hard coded as all_treatments
    filter(AVISIT %in% keep_visits) |>
    mutate(
      TRTA   = factor(TRTA, levels = treatments), # This should not be hard coded as all_treatments
      AVISIT = factor(AVISIT, levels = keep_visits)
    )

  param_label <- adlbc |> distinct(PARAMCD, PARAM) |> filter(PARAMCD == param) |> pull(PARAM)

  ggplot(lab_by_visit, aes(x = TRTA, y = AVAL)) +
    geom_boxplot(aes(color = TRTA), show.legend = FALSE, outlier.shape = NA) +
    geom_point(
      aes(color = TRTA),
      position = position_jitter(width = 0.2), alpha = 0.4, size = 1, show.legend = FALSE
    ) +
    facet_wrap(~ AVISIT, nrow = 1) +
    labs(x = NULL, y = param_label[1]) +
    scale_color_manual(values = trt_colors[treatments]) +
    theme_classic(base_size = 12) +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1),
      axis.title       = element_text(face = "bold"),
      strip.background = element_blank(),
      legend.position  = "top",
      legend.title     = element_blank()
    ) +
    guides(linetype = guide_legend(override.aes = list(color = "black")))
}

create_box_plot(param = "GLUC", treatments = c("Placebo", "Xanomeline High Dose"))
```

``` r
create_meanplot <- function(param = "EOS", treatments = all_treatments) {
  keep_visits <- c("Baseline", "Week 2", "Week 4", "Week 8", "Week 12")
  nudge_vals  <- c("Placebo" = -0.3, "Xanomeline Low Dose" = 0.0, "Xanomeline High Dose" = 0.3)
  xticks      <- c(0, 2, 4, 8, 12)

  chg_by_visit  <- adlbh |>
    mutate(AVISIT = sub("^\\s+", "", AVISIT)) |>
    filter(PARAMCD == param) |>
    mutate(CHG = AVAL - BASE) |>
    filter(!is.na(TRTA), TRTA %in% treatments) |> # This should not be hard coded as all_treatments
    filter(AVISIT %in% keep_visits) |>
    mutate(
      TRTA = factor(TRTA, levels = treatments), # This should not be hard coded as all_treatments
      week_n = case_when(
        grepl("^\\s*baseline\\s*$", AVISIT, ignore.case = TRUE) ~ 0,
        grepl("^\\s*week\\b", AVISIT, ignore.case = TRUE) ~
          suppressWarnings(as.numeric(sub("^\\s*Week\\s*([0-9]+).*$", "\\1", AVISIT, ignore.case = TRUE))),
        TRUE ~ NA_real_
      )
    )

  param_label <- adlbh |> distinct(PARAMCD, PARAM) |> filter(PARAMCD == param) |> pull(PARAM)

  visit_summary  <- chg_by_visit  |>
    group_by(TRTA, week_n) |>
    summarise(n = n(), mean_chg = mean(CHG, na.rm = TRUE), sd_chg = sd(CHG, na.rm = TRUE), .groups = "drop") |>
    mutate(
      se_chg   = ifelse(is.na(sd_chg / sqrt(n)), 0, sd_chg / sqrt(n)),
      mean_chg = ifelse(week_n == 0, 0, mean_chg),
      se_chg   = ifelse(week_n == 0, 0, se_chg),
      nudge = nudge_vals[as.character(TRTA)]
    ) |>
    arrange(week_n)

  ggplot(visit_summary, aes(x = ifelse(week_n == 0, week_n, week_n + nudge), y = mean_chg, color = TRTA, linetype = TRTA, group = TRTA)
  ) +
    geom_line(linewidth = 1, alpha = 0.7) +
    geom_point(size = 3, alpha = 0.7) +
    geom_errorbar(aes(ymin = mean_chg - se_chg, ymax = mean_chg + se_chg), width = 0.5, alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", linewidth = 0.6) +
    labs(
      x = "Weeks Since Baseline Measurement",
      y = paste0(param_label, "\nChange from Baseline (Mean \u00b1 SE)")
    ) +
    scale_color_manual(values = trt_colors[treatments]) +
    scale_x_continuous(breaks = xticks, expand = c(0, 0)) +
    coord_cartesian(xlim = c(0, max(xticks) + 0.5), clip = "off") +
    theme_classic(base_size = 12) +
    theme(axis.title = element_text(face = "bold"), legend.position = "none")
}

create_meanplot(param = "HGB")
```

> **Note:** These two functions are exactly
> `create_box_plot()`/`create_meanplot()` from `functions.R`, used
> throughout Part 2 — minus one addition (`crosstalk_group`) introduced
> in [Step 3c](#step-3c-link-the-plot-and-table-with-crosstalk).

------------------------------------------------------------------------

# Part 2: Creating R Shiny Dashboards

Part 1 built individual figures; Part 2 turns those static figures into
a dashboard - with three versions of increasing complexity. Each version
is its own standalone app file, and each one adds a layer of capability
on top of the last:

| File | Adds |
|:---|:---|
| `app-step1-simple.R` | A simple working Shiny app: static plots + a data table |
| `app-step2-layout.R` | Imrpoving the layout with a figure-choice dropdown |
| `app-step3-interactive.R` | Adding interactive components |

All three source the same `functions.R`, so the dashboard logic never
has to be rewritten — only the UI/server code around it grows.

## Shared Plotting Functions (`functions.R`)

`create_box_plot()` and `create_meanplot()` — built up from scratch in
[Part 1](#part-1-producing-visualizations) — live in `functions.R`,
which every app in Part 2 sources rather than redefining them.

``` r
source("functions.R")
```

Because the plotting logic already lives in one place, every app below
is free to focus entirely on UI and interactivity — the figures
themselves never need to be rewritten.

------------------------------------------------------------------------

## Step 1: A Simple Dashboard

The smallest useful dashboard: a classic `fluidPage()` layout, both
plots always visible via `plotOutput()`/`renderPlot()`, and a searchable
`DT` table underneath. With default arguments, `create_box_plot()` and
`create_meanplot()` — built from scratch in [Part 1, Step
4](#step-4-wrap-into-reusable-functions) — render exactly the two
figures shown there; `app-step1-simple.R` just wires them into
`renderPlot()`.

``` r
# app-step1-simple.R

# Load required packages
library(shiny)
library(ggplot2)
library(DT)
library(haven)

# Source reusable functions from the R folder
source("functions.R")

#load Dashboard data
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
```

> **Note:** Both plots use their default `param`/`treatments` arguments
> here — there’s no UI yet for changing them. That’s what Step 3 adds.

------------------------------------------------------------------------

## Step 2: Add a Polished Layout

Two changes make this feel like a real dashboard rather than a stack of
outputs: swapping `fluidPage()` for `bslib::page_sidebar()` (with a
Bootswatch theme), and adding a dropdown so the user picks *one* figure
to view at a time instead of scrolling past both.

``` r
# app-step2-layout.R
#
# Clinical dashboard: step 2 - add a layout

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
# TEMPLATE: add an entry here for each new plot you create in functions.R
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
    if (input$figure_choice == "box_plot") {
      create_box_plot()
    } else if (input$figure_choice == "meanplot") {
      create_meanplot()
    }
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
```

Design choices worth calling out:

- **`page_sidebar()` + `full_screen = TRUE` cards** give each output
  room to breathe, and let a reviewer expand a figure or table to fill
  the screen.
- **One figure at a time** (via the dropdown) keeps the layout simple as
  more plot types get added — no need to keep stacking outputs
  vertically.

------------------------------------------------------------------------

## Step 3: Add Interactivity

Step 2 has a working layout but is otherwise static. Step 3 gets there
in three small, independently-understandable additions, ending at
`app-step3-interactive.R`.

### Step 3a: Make Figures Interactive with plotly

`ggplotly()` converts the existing `ggplot2` output into an interactive
widget with hover tooltips, zoom, and pan. The change from Step 2 is
mechanical: `plotOutput()` → `plotlyOutput()`, `renderPlot()` →
`renderPlotly()`.

``` r
library(plotly)

# ui: plotOutput() becomes...
plotlyOutput("selected_plot", height = "600px")

# server: renderPlot() becomes...
output$selected_plot <- renderPlotly({
  if (input$figure_choice == "box_plot") {
    p <- create_box_plot()
  } else if (input$figure_choice == "meanplot") {
    p <- create_meanplot()
  }
  ggplotly(p)
})
```

------------------------------------------------------------------------

### Step 3b: Add Filtering Controls

Two new sidebar controls make the plots configurable rather than fixed:
a treatment-group checkbox filter, and a lab-parameter dropdown. Because
the box plot and mean-change plot draw from different lab datasets
(chemistry vs. hematology), the parameter dropdown’s choices depend on
which figure is selected — built dynamically with `renderUI()` rather
than hardcoded in the UI.

``` r
# Sidebar additions
#selectInput()...
uiOutput("param_ui"),
checkboxGroupInput(
  "treatments", "Treatment groups",
  choices = all_treatments, selected = all_treatments
)
#hr(),
#p(...
```

``` r
# Build lab-parameter choices from each dataset's PARAMCD/PARAM pairs,
# read once at startup rather than on every plot render.

#figure_choices <- c(...
get_param_choices <- function(path) {
  df <- haven::read_xpt(path) |>
    dplyr::distinct(PARAMCD, PARAM) |>
    dplyr::filter(!startsWith(PARAMCD, "_"))
  setNames(df$PARAMCD, df$PARAM)
}

param_choices <- list(
  box_plot = get_param_choices("data/adlbc.xpt"),
  meanplot = get_param_choices("data/adlbh.xpt")
)
param_defaults <- c(box_plot = "ALB", meanplot = "EOS")
#ui...

# Server additions
#server <- ...
output$param_ui <- renderUI({
  selectInput(
    "param_choice", "Lab parameter",
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
    p <- create_box_plot(param = input$param_choice, treatments = input$treatments)
  } else if (input$figure_choice == "meanplot") {
    p <- create_meanplot(param = input$param_choice, treatments = input$treatments)
  }
  ggplotly(p)
})

#output$data_table...
```

This is why `create_box_plot()`/`create_meanplot()` in `functions.R`
take `param`/`treatments` arguments instead of hardcoding a single lab
value — the UI now drives them directly.

------------------------------------------------------------------------

### Step 3c: Link the Plot and Table with crosstalk

The last addition links the box plot to the table: clicking a subject’s
point highlights that subject’s row(s) below. This uses
`crosstalk::SharedData` — both the plot’s data and the table’s data are
wrapped with the same key column (`USUBJID`) and the same group name,
and `plotly::highlight()` turns a plain click into a selection event
that DT listens for.

``` r
library(crosstalk)
# source("../functions.R")...
crosstalk_group_id <- "clinical_dashboard"
dashboard_shared <- crosstalk::SharedData$new(
  dashboard_data,
  key   = ~USUBJID,
  group = crosstalk_group_id
)
```

``` r
# functions.R: create_box_plot() gains a crosstalk_group argument.

# add crosstalk_group = NULL to boxplot function

# lab_by_visit <- adlbc |> ...
plot_data <- if (!is.null(crosstalk_group)) {
  crosstalk::SharedData$new(lab_by_visit, key = ~USUBJID, group = crosstalk_group)
} else {
  lab_by_visit
}
#param_label <- ...

p <- ggplot(plot_data, aes(x = TRTA, y = AVAL)) + ...
```

``` r
#app file
#server
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

# Crosstalk-linked tables require DT's client-side mode (server = FALSE)
output$data_table <- renderDT({
  datatable(
    dashboard_shared,
    rownames = TRUE,
    filter = "top",
    options = list(pageLength = 10, scrollX = TRUE)
  )
}, server = FALSE)
```

> **Note:** Only the box plot is linked. The mean-change plot shows
> treatment-arm averages rather than individual subjects, so there’s no
> single row for a click on that plot to highlight.

The complete result of Steps 3a–3c is `app-step3-interactive.R`.
