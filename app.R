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
  "))),
  # ----------------------------------------------------------
  # Primary tab: Circuit Map
  # ----------------------------------------------------------
  nav_panel(
    title = "Circuit Map",
    icon = icon("map-location-dot"),
    
    # Main content - no need for layout_sidebar since we have a shared sidebar
    tags$div(style = "height: 15px;"),
    
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        tags$span("F1 Circuit Locations", class = "h5 m-0"),
        tags$div(
          class = "btn-group",
          actionButton(
            inputId = "zoom_all",
            label = "View All",
            icon = icon("globe"),
            class = "btn-sm btn-outline-primary"
          ),
          actionButton(
            inputId = "zoom_europe",
            label = "Europe",
            icon = icon("location-dot"),
            class = "btn-sm btn-outline-primary"
          )
        )
      ),
      card_body(
        class = "p-0 map-container",
        leafletOutput("map", height = "700px")
      )
    )
  ),
  
  # ----------------------------------------------------------
  # Secondary tab: Analytics Dashboard
  # ----------------------------------------------------------
  nav_panel(
    title = "Analytics Dashboard",
    icon = icon("chart-line"),
    
    # Main content - no need for layout_sidebar
    tags$div(style = "height: 15px;"),
    
    # First row: Annual Races and Fatal Event Occurrence
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        tags$span("Annual Races and Fatal Event Occurrence", class = "h5 m-0")
      ),
      card_body(
        class = "chart-container",
        plotlyOutput("annual_race_plot", height = "100%")
      )
    ),
    
    # Second row: Two visualizations side by side
    layout_columns(
      col_widths = c(6, 6),
      
      # Left column: Distribution of Fatal Events by Constructor
      card(
        height = "100%",
        card_header(
          "Distribution of Fatal Events by Constructor"
        ),
        card_body(
          class = "chart-container",
          plotlyOutput("constructor_pie_chart", height = "100%")
        )
      ),
      
      # Right column: Driver Nationalities and Fatality Rates
      card(
        height = "100%",
        card_header(
          "Driver Nationalities and Fatality Rates"
        ),
        card_body(
          class = "chart-container",
          plotlyOutput("nationality_sunburst", height = "100%")
        )
      )
    )
  ),
  
  # ----------------------------------------------------------
  # Third tab: Data Table
  # ----------------------------------------------------------
  nav_panel(
    title = "Data Table",
    icon = icon("table"),
    
    # Main content - no need for layout_sidebar
    tags$div(style = "height: 15px;"),
    
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        tags$span("Race Results", class = "h5 m-0"),
        downloadButton("download_data", "Download", class = "btn-sm btn-outline-primary")
      ),
      card_body(
        class = "datatable-container", # Added padding in CSS
        DTOutput("datatable", height = "100%")
      )
    )
  )
)

# ============================================================
# Server
# ============================================================
server <- function(input, output, session) {  # ----------------------------------------------------------
  # Reset Filters Button
  # ----------------------------------------------------------
  observeEvent(input$reset_filters, {
    updateSliderInput(session, "year_range", value = c(choices$year_range[1], choices$year_range[2]))
    updatePickerInput(session, "constructor_nationality", selected = "ALL")
    updatePickerInput(session, "constructor_name", selected = "ALL")
    updatePickerInput(session, "driver_name", selected = "ALL")
    updateCheckboxGroupInput(session, "driver_fatal", selected = choices$driver_fatal)
  })
  
  # ----------------------------------------------------------
  # Reactive: Filtered Data
  # ----------------------------------------------------------
  filtered_data <- reactive({
    # Apply filters
    data <- dataOK |>
      filter(
        year >= input$year_range[1],
        year <= input$year_range[2]
      )
    
    # Filter by constructor nationality
    if (!is.null(input$constructor_nationality) && 
        input$constructor_nationality != "ALL") {
      data <- data |>
        filter(constructor.nationality == input$constructor_nationality)
    }
    
    # Filter by constructor name
    if (!is.null(input$constructor_name) && 
        input$constructor_name != "ALL") {
      data <- data |>
        filter(constructor.name == input$constructor_name)
    }
    
    # Filter by driver name
    if (!is.null(input$driver_name) && 
        input$driver_name != "ALL") {
      data <- data |>
        filter(driver.name == input$driver_name)
    }
    
    # Filter by driver fatal status
    if (!is.null(input$driver_fatal) && length(input$driver_fatal) > 0) {
      data <- data |>
        filter(driver.fatal %in% input$driver_fatal)
    }
    
    data
  })
  # ----------------------------------------------------------
  # Reactive: Filtered Data
  # ----------------------------------------------------------
  filtered_data <- reactive({
    # Apply filters
    data <- dataOK |>
      filter(
        year >= input$year_range[1],
        year <= input$year_range[2]
      )
    
    # Filter by constructor nationality
    if (!is.null(input$constructor_nationality) && 
        input$constructor_nationality != "ALL") {
      data <- data |>
        filter(constructor.nationality == input$constructor_nationality)
    }
    
    # Filter by constructor name
    if (!is.null(input$constructor_name) && 
        input$constructor_name != "ALL") {
      data <- data |>
        filter(constructor.name == input$constructor_name)
    }
    
    # Filter by driver name
    if (!is.null(input$driver_name) && 
        input$driver_name != "ALL") {
      data <- data |>
        filter(driver.name == input$driver_name)
    }
    
    # Filter by driver fatal status
    if (!is.null(input$driver_fatal) && length(input$driver_fatal) > 0) {
      data <- data |>
        filter(driver.fatal %in% input$driver_fatal)
    }
    
    data
  })
  
  # ----------------------------------------------------------
  # Output: Data Summary
  # ----------------------------------------------------------
  output$data_summary <- renderText({
    data <- filtered_data()
    races <- length(unique(data$Race))
    drivers <- length(unique(data$driver.name))
    fatalities <- sum(data$driver.fatal == "Death", na.rm = TRUE)
    
    paste0(
      "Showing ", nrow(data), " entries\n",
      "Races: ", races, " | ",
      "Drivers: ", drivers, " | ",
      "Fatal events: ", fatalities
    )
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
