library(shiny)
library(bslib)
library(shinyWidgets)
library(dplyr)
library(DT)
library(leaflet)
library(htmltools)
library(shinyjs)
library(plotly)
library(ggplot2)

load("./dataOK.RData")

# ============================================================
# Preprocessing: Obtain the choices of each input
# ============================================================
prepare_choices <- function(data) {
  list(
    year_range = c(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE)),
    constructor_nat = c("ALL", data |> 
                          count(constructor.nationality, sort = TRUE) |> 
                          pull(constructor.nationality)),
    constructor_name = c("ALL", data |> 
                           count(constructor.name, sort = TRUE) |> 
                           pull(constructor.name)),
    driver_name = c("ALL", data |> 
                      count(driver.name, sort = TRUE) |> 
                      pull(driver.name)),
    driver_fatal = unique(data$driver.fatal)
  )
}


# Generate choices once at startup
choices <- prepare_choices(dataOK)

# ============================================================
# Helper function to create labeled inputs with tooltips on icons only
# ============================================================
create_labeled_input <- function(input_func, input_id, label_text, tooltip_text, ...) {
  tagList(
    tags$label(
      class = "control-label",
      label_text,
      tooltip(
        icon("circle-info", class = "text-muted ms-1"),
        tooltip_text,
        placement = "right"
      )
    ),
    input_func(input_id, label = NULL, ...)
  )
}


# ============================================================
# UI - Changed to page_navbar with a SHARED sidebar
# ============================================================
ui <- page_navbar(
  title = tags$div(
    tags$img(src = "https://www.formula1.com/etc/designs/fom-website/images/f1_logo.svg", height = "30px", style = "margin-right: 10px;"),
    "Formula 1 Data Explorer (1950-2024)",
    style = "display: flex; align-items: center;"
  ),
  
  # Create a SINGLE shared sidebar for all tabs
  sidebar = sidebar(
    width = 300,
    bg = "#f8f9fa",
    open ='always',
    
    # Year Range Slider with tooltip only on icon
    create_labeled_input(
      sliderInput,
      "year_range",
      "Year Range",
      "Filter F1 races by year. Drag the slider to select a range from 1950 to 2024.",
      min = choices$year_range[1],
      max = choices$year_range[2],
      value = c(choices$year_range[1], choices$year_range[2]),
      step = 1,
      sep = "",
      ticks = FALSE
    ),
    
    # Small divider
    tags$div(style = "height: 8px;"),
    
    # Constructor Nationality
    create_labeled_input(
      pickerInput,
      "constructor_nationality",
      "Constructor Nationality",
      "Select the nationality of constructor teams. Choose 'ALL' to include all.",
      choices = choices$constructor_nat,
      selected = "ALL",
      options = pickerOptions(
        liveSearch = TRUE,
        liveSearchPlaceholder = "Search nationality...",
        style = "btn-outline-secondary"
      )
    ),
    
    # Constructor Name
    create_labeled_input(
      pickerInput,
      "constructor_name",
      "Constructor Name",
      "Select the constructor/team name. Choose 'ALL' to include all.",
      choices = choices$constructor_name,
      selected = "ALL",
      options = pickerOptions(
        liveSearch = TRUE,
        liveSearchPlaceholder = "Search constructor...",
        style = "btn-outline-secondary"
      )
    ),
    
    # Driver Name
    create_labeled_input(
      pickerInput,
      "driver_name",
      "Driver Name",
      "Select a specific driver name. Choose 'ALL' to include all drivers.",
      choices = choices$driver_name,
      selected = "ALL",
      options = pickerOptions(
        liveSearch = TRUE,
        liveSearchPlaceholder = "Search driver...",
        style = "btn-outline-secondary"
      )
    ),
    
    # Driver Fatal Status
    create_labeled_input(
      checkboxGroupInput,
      "driver_fatal",
      "Driver Fatal Status",
      "'NULL' = No fatal accident; 'Death' = Driver had a fatal accident.",
      choices = choices$driver_fatal,
      selected = choices$driver_fatal,
      inline = TRUE
    ),
    
    # Small divider
    tags$div(style = "height: 8px;"),
    
    # Reset button
    tags$div(class = "d-grid gap-2 mt-2",
             actionButton(
               inputId = "reset_filters",
               label = "Reset All Filters",
               icon = icon("rotate"),
               class = "btn-outline-secondary"
             )
    ),
    
    # Data summary
    tags$div(
      class = "mt-3 pt-2 border-top",
      textOutput("data_summary"),
      style = "font-size: 0.9rem; color: #666;"
    )
  ),
  
  theme = bs_theme(
    version = 5, 
    bootswatch = "flatly",
    primary = "#E10600",  # F1 red
    "navbar-bg" = "#15151E"  # F1 dark blue
  ),
  
  # Add shinyjs
  useShinyjs(),
  
  # Custom CSS for better styling
  tags$head(tags$style(HTML("
    .control-label { font-weight: 600; color: #333; }
    .card { box-shadow: 0 4px 6px rgba(0,0,0,0.1); }
    /* Remove card margins */
    .tab-content .card { margin-bottom: 0px !important; }
    .action-button { margin-top: 10px; }
    .dataTables_wrapper { font-size: 14px; }
    .fatal-event { color: #E10600 !important; font-weight: bold; }
    .map-container { border-radius: 5px; overflow: hidden; }
    /* Added padding for the datatable */
    .datatable-container { 
      height: calc(100vh - 200px) !important; 
      min-height: 700px;
      padding: 15px !important; 
    }
    /* More compact sidebar styles */
    .sidebar .form-group { margin-bottom: 10px; }
    .sidebar .control-label { font-size: 0.9rem; }
    .sidebar .shiny-input-container { margin-bottom: 5px !important; }
    /* Add spacing between tab titles and content */
    .tab-content { padding-top: 15px; }
    .nav-underline { margin-bottom: 10px; }
    /* Chart container styling */
    .chart-container { height: 400px; width: 100%; }
    .plotly-graph { height: 100%; width: 100%; }
    /* Card headers */
    .card-header { background-color: #f8f9fa; }
    /* No side padding for the main container */
    .tab-content .container-fluid { padding-left: 0 !important; padding-right: 0 !important; }
    /* Fix for sidebar labels */
    .sidebar .control-label { display: flex; align-items: center; }
    /* Bottom margin for layout columns */
    .layout-columns { margin-bottom: 0 !important; }
  "))),)

# ============================================================
# Server
# ============================================================
server <- function(input, output, session) {}
# Run the application 
shinyApp(ui = ui, server = server)
