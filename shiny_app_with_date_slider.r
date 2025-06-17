# Ultimate EDA Shiny App - Foundation with Date Range Slider
# This is the starting framework focusing on file upload, filtering, and DT table

library(shiny)
library(DT)
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(purrr)
library(lubridate)

# UI
ui <- fluidPage(
  # Remove default bootstrap container constraints
  tags$head(
    tags$style(HTML("
      .container-fluid {
        max-width: none !important;
        padding: 0px;
      }
      .row {
        margin-left: 0px;
        margin-right: 0px;
      }
      .col-sm-12 {
        padding-left: 5px;
        padding-right: 5px;
      }
      /* Ensure dropdown arrows are visible */
      .selectize-control.single .selectize-input:after {
        content: ' ';
        display: block;
        position: absolute;
        top: 50%;
        right: 15px;
        margin-top: -3px;
        width: 0;
        height: 0;
        border-style: solid;
        border-width: 5px 5px 0 5px;
        border-color: #333333 transparent transparent transparent;
      }
      .selectize-control.multi .selectize-input.has-items {
        padding-right: 30px;
      }
      /* Style for date range input */
      .shiny-date-range-input {
        font-size: 14px;
      }
      .shiny-date-range-input .input-daterange {
        width: 100%;
      }
    "))
  ),
  
  titlePanel("Ultimate EDA Dashboard"),
  
  # Row 1: File Upload and Basic Controls
  fluidRow(
    column(3,
      fileInput("file", "Upload CSV or Excel File",
                accept = c(".csv", ".xlsx", ".xls"))
    ),
    column(2,
      checkboxInput("header", "Header in first row", value = TRUE)
    ),
    column(2,
      selectInput("rows_display", "Rows to display:",
                  choices = c(30, 40, 50),
                  selected = 30)
    ),
    column(5,
      # Placeholder for additional controls
      div(id = "additional_controls")
    )
  ),
  
  # Row 2: Filtering Controls (will be populated dynamically)
  fluidRow(
    column(12,
      div(id = "filter_controls",
          style = "background-color: #f8f9fa; padding: 10px; margin: 10px 0px; border-radius: 5px;")
    )
  ),
  
  # Row 3: Tab Panel for Data Table and Charts
  fluidRow(
    column(12,
      tabsetPanel(id = "main_tabs",
        tabPanel("Data Table", 
          br(),
          DT::dataTableOutput("data_table", width = "100%")
        ),
        tabPanel("Run Chart", 
          br(),
          div("Run Chart will be implemented here")
        ),
        tabPanel("Bar Chart", 
          br(),
          div("Bar Chart will be implemented here")
        ),
        tabPanel("Line Chart", 
          br(),
          div("Line Chart will be implemented here")
        ),
        tabPanel("Untrended Control Chart", 
          br(),
          div("Untrended Control Chart will be implemented here")
        ),
        tabPanel("Trended Control Chart", 
          br(),
          div("Trended Control Chart will be implemented here")
        ),
        tabPanel("Cohort Chart", 
          br(),
          div("Cohort Chart will be implemented here")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Function to detect and convert numeric dates
  detect_and_convert_dates <- function(data) {
    for(col in names(data)) {
      if(is.numeric(data[[col]])) {
        values <- data[[col]][!is.na(data[[col]])]
        
        if(length(values) > 0) {
          converted <- FALSE
          
          # Check for years (like 1995, 1996, 2023)
          if(all(values >= 1900 & values <= 2100) & all(values == floor(values))) {
            try({
              # Convert years to January 1st of each year
              data[[col]] <- as.Date(paste0(data[[col]], "-01-01"))
              converted <- TRUE
            }, silent = FALSE)
          }
          # Check for Excel serial dates (typically 15000-60000 range)
          else if(all(values >= 15000 & values <= 60000)) {
            try({
              # Convert Excel serial dates (origin = "1899-12-30")
              data[[col]] <- as.Date(data[[col]], origin = "1899-12-30")
              converted <- TRUE
            }, silent = FALSE)
          }
          # Check for YYYYMMDD format (like 20230101)
          else if(all(values >= 19000101 & values <= 21001231) & 
                  all(nchar(as.character(values)) == 8)) {
            try({
              data[[col]] <- as.Date(as.character(data[[col]]), format = "%Y%m%d")
              converted <- TRUE
            }, silent = FALSE)
          }
          # Check for Unix timestamps (very large numbers, seconds since 1970)
          else if(all(values >= 946684800 & values <= 2147483647)) {
            try({
              data[[col]] <- as.Date(as.POSIXct(data[[col]], origin = "1970-01-01"))
              converted <- TRUE
            }, silent = FALSE)
          }
          # Check if it might be days since epoch (1970-01-01)
          else if(all(values >= 1 & values <= 25000)) {
            try({
              data[[col]] <- as.Date(data[[col]], origin = "1970-01-01")
              converted <- TRUE
            }, silent = FALSE)
          }
        }
      }
    }
    return(data)
  }
  
  # Reactive value to store the uploaded data
  raw_data <- reactive({
    req(input$file)
    
    ext <- tools::file_ext(input$file$datapath)
    
    data <- if(ext == "csv") {
      read_csv(input$file$datapath, col_names = input$header, show_col_types = FALSE)
    } else if(ext %in% c("xlsx", "xls")) {
      read_excel(input$file$datapath, col_names = input$header)
    } else {
      return(NULL)
    }
    
    # Try to detect and convert numeric date columns
    if(!is.null(data)) {
      data <- detect_and_convert_dates(data)
    }
    
    return(data)
  })
  
  # Observe the uploaded data and create filtering controls
  observeEvent(raw_data(), {
    data <- raw_data()
    req(data)
    
    # Clear existing filter controls first
    removeUI(selector = "#filter_controls > *", multiple = TRUE)
    
    # Get column names excluding common 'Value' columns (case insensitive)
    value_patterns <- c("^value$", "^pct$", "^percent$", "^amount$", "^count$", "^measure$")
    value_pattern <- paste(value_patterns, collapse = "|")
    filter_columns <- names(data)[!grepl(value_pattern, names(data), ignore.case = TRUE)]
    
    # Create dynamic filter controls
    filter_controls <- map(filter_columns, function(col) {
      
      # Check if column is a date
      if(inherits(data[[col]], "Date") || lubridate::is.Date(data[[col]])) {
        # Date columns get actual date sliders
        date_values <- data[[col]][!is.na(data[[col]])]
        min_date <- min(date_values, na.rm = TRUE)
        max_date <- max(date_values, na.rm = TRUE)
        
        column(4,  # Wider for actual slider
          sliderInput(
            inputId = paste0("filter_", make.names(col)),
            label = col,
            min = min_date,
            max = max_date,
            value = c(min_date, max_date),
            timeFormat = "%Y-%m-%d",
            step = 1
          )
        )
        
      } else if(is.numeric(data[[col]])) {
        # Numeric columns get range sliders
        column(2,
          sliderInput(
            inputId = paste0("filter_", make.names(col)),
            label = col,
            min = min(data[[col]], na.rm = TRUE),
            max = max(data[[col]], na.rm = TRUE),
            value = c(min(data[[col]], na.rm = TRUE), max(data[[col]], na.rm = TRUE)),
            step = (max(data[[col]], na.rm = TRUE) - min(data[[col]], na.rm = TRUE)) / 100
          )
        )
        
      } else {
        # Categorical columns get multi-select with All/None options
        unique_values <- unique(data[[col]][!is.na(data[[col]])])
        unique_values <- sort(as.character(unique_values))
        
        choices_list <- c("All" = "ALL_VALUES", "None" = "NO_VALUES", setNames(unique_values, unique_values))
        
        column(2,
          selectInput(
            inputId = paste0("filter_", make.names(col)),
            label = col,
            choices = choices_list,
            selected = "ALL_VALUES",
            multiple = TRUE,
            selectize = TRUE
          )
        )
      }
    })
    
    # Insert the filter controls into the UI
    insertUI(
      selector = "#filter_controls",
      where = "beforeEnd",
      ui = fluidRow(filter_controls)
    )
  })
  
  # Observer to handle "All" and "None" selection logic for categorical variables
  observe({
    data <- raw_data()
    req(data)
    
    value_patterns <- c("^value$", "^pct$", "^percent$", "^amount$", "^count$", "^measure$")
    value_pattern <- paste(value_patterns, collapse = "|")
    filter_columns <- names(data)[!grepl(value_pattern, names(data), ignore.case = TRUE)]
    
    for(col in filter_columns) {
      # Only apply All/None logic to categorical columns (not dates or numeric)
      if(!is.numeric(data[[col]]) && !inherits(data[[col]], "Date") && !lubridate::is.Date(data[[col]])) {
        filter_input_name <- paste0("filter_", make.names(col))
        
        observeEvent(input[[filter_input_name]], {
          current_selection <- input[[filter_input_name]]
          
          if(!is.null(current_selection)) {
            # If "All" is selected with other values, keep only "All"
            if("ALL_VALUES" %in% current_selection && length(current_selection) > 1) {
              updateSelectInput(session, filter_input_name, selected = "ALL_VALUES")
            }
            # If "None" is selected with other values, keep only "None"
            else if("NO_VALUES" %in% current_selection && length(current_selection) > 1) {
              updateSelectInput(session, filter_input_name, selected = "NO_VALUES")
            }
          }
        }, ignoreInit = TRUE)
      }
    }
  })
  
  # Reactive filtered data
  filtered_data <- reactive({
    data <- raw_data()
    req(data)
    
    # Get all filter inputs (excluding value columns)
    value_patterns <- c("^value$", "^pct$", "^percent$", "^amount$", "^count$", "^measure$")
    value_pattern <- paste(value_patterns, collapse = "|")
    filter_columns <- names(data)[!grepl(value_pattern, names(data), ignore.case = TRUE)]
    
    filtered <- data
    
    for(col in filter_columns) {
      filter_input_name <- paste0("filter_", make.names(col))
      filter_value <- input[[filter_input_name]]
      
      if(!is.null(filter_value)) {
        
        # Date filtering
        if(inherits(data[[col]], "Date") || lubridate::is.Date(data[[col]])) {
          start_date <- filter_value[1]
          end_date <- filter_value[2]
          filtered <- filtered %>%
            filter(!!sym(col) >= start_date & !!sym(col) <= end_date)
            
        } else if(is.numeric(data[[col]])) {
          # Numeric filtering
          filtered <- filtered %>%
            filter(!!sym(col) >= filter_value[1] & !!sym(col) <= filter_value[2])
            
        } else {
          # Categorical filtering
          # Handle special "All" and "None" options
          if("ALL_VALUES" %in% filter_value) {
            # If "All" is selected, include all values (no filtering needed)
            next
          } else if("NO_VALUES" %in% filter_value && length(filter_value) == 1) {
            # If only "None" is selected, filter to no rows
            filtered <- filtered %>% filter(FALSE)
          } else {
            # Remove special values and filter normally
            actual_values <- filter_value[!filter_value %in% c("ALL_VALUES", "NO_VALUES")]
            if(length(actual_values) > 0) {
              filtered <- filtered %>%
                filter(as.character(!!sym(col)) %in% actual_values)
            } else {
              # If no actual values selected (only "None"), filter to no rows
              filtered <- filtered %>% filter(FALSE)
            }
          }
        }
      }
    }
    
    return(filtered)
  })
  
  # Render the data table
  output$data_table <- DT::renderDataTable({
    data <- filtered_data()
    req(data)
    
    DT::datatable(
      data,
      options = list(
        pageLength = as.numeric(input$rows_display),
        scrollX = TRUE,
        scrollY = "400px",
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      extensions = 'Buttons',
      rownames = FALSE
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)