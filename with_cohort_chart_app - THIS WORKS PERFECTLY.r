# Ultimate EDA Shiny App - Complete Version with Fixed NA Handling
# Clean, working version with Run Chart, Line Chart, Bar Chart, and EXPECTATION CHARTS
# FIXED: NA handling for missing data years and renamed Control Charts to Expectation Charts

library(shiny)
library(DT)
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(ggtext)
library(purrr)
library(lubridate)
library(scales)

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
    "))
  ),

  titlePanel("Systems Analysis Dashboard"),

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
    column(2,
      checkboxInput("format_as_percentage", "Y-axis: Show as Percentage (85.2%)", value = FALSE),
      tags$small("Check if values are rates/percentages, uncheck for scores/counts", style = "color: #6c757d;")
    ),
    column(3,
      conditionalPanel(
        condition = "output.data_uploaded",
        selectInput("grouping_var", "Line/Bar Chart Grouping:",
                    choices = NULL,
                    selected = NULL),
        tags$small("Column to group/label lines and bars", style = "color: #6c757d;")
      )
    )
  ),

  # Row 2: Filtering Controls
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
          fluidRow(
            column(12,
              uiOutput("date_info")
            )
          ),
          br(),
          fluidRow(
            column(4,
              textInput("run_title", "Chart Title:", value = "Run Chart")
            ),
            column(4,
              textInput("run_subtitle", "Subtitle:", value = "")
            ),
            column(4,
              textInput("run_caption", "Caption:", value = "")
            )
          ),
          br(),
          fluidRow(
            column(12,
              div(style = "text-align: center;",
                downloadButton("download_run_png", "Download PNG", class = "btn-primary"),
                tags$span(style = "margin: 0 10px;"),
                downloadButton("download_run_svg", "Download SVG", class = "btn-success"),
                tags$span(style = "margin: 0 10px;"),
                downloadButton("download_run_pdf", "Download PDF", class = "btn-info")
              )
            )
          ),
          br(),
          plotOutput("run_chart", height = "600px")
        ),
        tabPanel("Line Chart",
          br(),
          fluidRow(
            column(12,
              uiOutput("grouping_info")
            )
          ),
          br(),
          fluidRow(
            column(4,
              textInput("line_title", "Chart Title:", value = "Line Chart")
            ),
            column(4,
              textInput("line_subtitle", "Subtitle:", value = "")
            ),
            column(4,
              textInput("line_caption", "Caption:", value = "")
            )
          ),
          br(),
          fluidRow(
            column(12,
              div(style = "text-align: center;",
                downloadButton("download_line_png", "Download PNG", class = "btn-primary"),
                tags$span(style = "margin: 0 10px;"),
                downloadButton("download_line_svg", "Download SVG", class = "btn-success"),
                tags$span(style = "margin: 0 10px;"),
                downloadButton("download_line_pdf", "Download PDF", class = "btn-info")
              )
            )
          ),
          br(),
          plotOutput("line_chart", height = "600px")
        ),
        tabPanel("Bar Chart",
          br(),
          fluidRow(
            column(12,
              uiOutput("bar_info")
            )
          ),
          br(),
          fluidRow(
            column(4,
              textInput("bar_title", "Chart Title:", value = "Bar Chart")
            ),
            column(4,
              textInput("bar_subtitle", "Subtitle:", value = "")
            ),
            column(4,
              textInput("bar_caption", "Caption:", value = "")
            )
          ),
          br(),
          fluidRow(
            column(12,
              div(style = "text-align: center;",
                downloadButton("download_bar_png", "Download PNG", class = "btn-primary"),
                tags$span(style = "margin: 0 10px;"),
                downloadButton("download_bar_svg", "Download SVG", class = "btn-success"),
                tags$span(style = "margin: 0 10px;"),
                downloadButton("download_bar_pdf", "Download PDF", class = "btn-info")
              )
            )
          ),
          br(),
          plotOutput("bar_chart", height = "600px")
        ),
        tabPanel("Untrended Expectation Chart",
          br(),
          fluidRow(
            column(12,
              uiOutput("control_info")
            )
          ),
          br(),
          fluidRow(
            column(12,
              conditionalPanel(
                condition = "output.data_uploaded",
                div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border: 1px solid #dee2e6;",
                  fluidRow(
                    column(3,
                      checkboxInput("enable_recalc", "Enable Recalculation", value = FALSE)
                    ),
                    column(4,
                      conditionalPanel(
                        condition = "input.enable_recalc",
                        dateInput("recalc_date", "Recalculate limits starting from:",
                                 value = NULL, min = NULL, max = NULL)
                      )
                    ),
                    column(5,
                      conditionalPanel(
                        condition = "input.enable_recalc",
                        tags$small("Select date to split process and recalculate expectation limits",
                                  style = "color: #6c757d; margin-top: 5px; display: block;")
                      )
                    )
                  )
                )
              )
            )
          ),
          br(),
          fluidRow(
            column(4,
              textInput("control_title", "Chart Title:", value = "Untrended Expectation Chart")
            ),
            column(4,
              textInput("control_subtitle", "Subtitle:", value = "")
            ),
            column(4,
              textInput("control_caption", "Caption:", value = "")
            )
          ),
          br(),
          fluidRow(
            column(12,
              div(style = "text-align: center;",
                downloadButton("download_control_png", "Download PNG", class = "btn-primary"),
                tags$span(style = "margin: 0 10px;"),
                downloadButton("download_control_svg", "Download SVG", class = "btn-success"),
                tags$span(style = "margin: 0 10px;"),
                downloadButton("download_control_pdf", "Download PDF", class = "btn-info")
              )
            )
          ),
          br(),
          plotOutput("control_chart", height = "600px")
        ),
        tabPanel("Trended Expectation Chart",
          br(),
          fluidRow(
            column(12,
              uiOutput("trended_info")
            )
          ),
          br(),
          fluidRow(
            column(4,
              textInput("trended_title", "Chart Title:", value = "Trended Expectation Chart")
            ),
            column(4,
              textInput("trended_subtitle", "Subtitle:", value = "")
            ),
            column(4,
              textInput("trended_caption", "Caption:", value = "")
            )
          ),
          br(),
          fluidRow(
            column(12,
              div(style = "text-align: center;",
                downloadButton("download_trended_png", "Download PNG", class = "btn-primary"),
                tags$span(style = "margin: 0 10px;"),
                downloadButton("download_trended_svg", "Download SVG", class = "btn-success"),
                tags$span(style = "margin: 0 10px;"),
                downloadButton("download_trended_pdf", "Download PDF", class = "btn-info")
              )
            )
          ),
          br(),
          plotOutput("trended_chart", height = "600px")
        ),
        tabPanel("Cohort Chart",
          br(),
          fluidRow(
            column(12,
              uiOutput("cohort_info")
            )
          ),
          br(),
          fluidRow(
            column(12,
              conditionalPanel(
                condition = "output.data_uploaded",
                div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border: 1px solid #dee2e6;",
                  fluidRow(
                    column(2,
                      selectInput("cohort_grade_var", "Grade Column:",
                                  choices = NULL,
                                  selected = NULL)
                    ),
                    column(2,
                      numericInput("cohort_start_grade", "Starting Grade:",
                                   value = 3, min = 1, max = 12, step = 1)
                    ),
                    column(2,
                      numericInput("cohort_end_grade", "Ending Grade:",
                                   value = 8, min = 1, max = 12, step = 1)
                    ),
                    column(3,
                      numericInput("cohort_start_year", "Starting Year:",
                                   value = 2018, min = 1900, max = 2030, step = 1)
                    ),
                    column(3,
                      numericInput("cohort_end_year", "Ending Year:",
                                   value = 2023, min = 1900, max = 2030, step = 1)
                    )
                  ),
                  br(),
                  fluidRow(
                    column(12,
                      tags$small("Track a demographic cohort's progression through grades over consecutive years. Use filtering controls above to select your cohort (e.g., Asian, Reading).",
                                style = "color: #6c757d;")
                    )
                  )
                )
              )
            )
          ),
          br(),
          fluidRow(
            column(4,
              textInput("cohort_title", "Chart Title:", value = "Cohort Analysis")
            ),
            column(4,
              textInput("cohort_subtitle", "Subtitle:", value = "")
            ),
            column(4,
              textInput("cohort_caption", "Caption:", value = "")
            )
          ),
          br(),
          fluidRow(
            column(12,
              div(style = "text-align: center;",
                downloadButton("download_cohort_png", "Download PNG", class = "btn-primary"),
                tags$span(style = "margin: 0 10px;"),
                downloadButton("download_cohort_svg", "Download SVG", class = "btn-success"),
                tags$span(style = "margin: 0 10px;"),
                downloadButton("download_cohort_pdf", "Download PDF", class = "btn-info")
              )
            )
          ),
          br(),
          plotOutput("cohort_chart", height = "600px")
        ),
        tabPanel("Debug",
          br(),
          fluidRow(
            column(12,
              h4("Cohort Debug Information"),
              verbatimTextOutput("cohort_debug_info"),
              br(),
              h4("Cohort Data Structure"),
              verbatimTextOutput("cohort_data_structure"),
              br(),
              h4("Sample Cohort Data"),
              DT::dataTableOutput("cohort_data_sample")
            )
          )
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
              data[[col]] <- as.Date(paste0(data[[col]], "-01-01"))
              converted <- TRUE
            }, silent = FALSE)
          }
          # Check for Excel serial dates
          else if(all(values >= 15000 & values <= 60000)) {
            try({
              data[[col]] <- as.Date(data[[col]], origin = "1899-12-30")
            }, silent = FALSE)
          }
          # Check for YYYYMMDD format
          else if(all(values >= 19000101 & values <= 21001231) &
                  all(nchar(as.character(values)) == 8)) {
            try({
              data[[col]] <- as.Date(as.character(data[[col]]), format = "%Y%m%d")
              converted <- TRUE
            }, silent = FALSE)
          }
        }
      }
    }
    return(data)
  }

  # Reactive value to store uploaded data
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

    # Convert numeric date columns
    if(!is.null(data)) {
      data <- detect_and_convert_dates(data)
    }

    return(data)
  })

  # Output to control conditional panel visibility
  output$data_uploaded <- reactive({
    return(!is.null(raw_data()))
  })
  outputOptions(output, 'data_uploaded', suspendWhenHidden = FALSE)

  # Reactive for available grouping columns
  grouping_choices <- reactive({
    data <- raw_data()
    if(is.null(data)) return(NULL)

    # Exclude date and value columns
    value_patterns <- c("^value$", "^pct$", "^percent$", "^amount$", "^count$", "^measure$")
    value_pattern <- paste(value_patterns, collapse = "|")
    potential_groups <- names(data)[!names(data) %in% c("date") &
                                   !grepl(value_pattern, names(data), ignore.case = TRUE)]

    return(potential_groups)
  })

  # Reactive for cohort grade column choices
  cohort_grade_choices <- reactive({
    data <- raw_data()
    if(is.null(data)) return(NULL)

    # Look for grade-like columns
    grade_patterns <- c("grade", "level", "year")
    potential_grades <- names(data)

    # Prioritize columns with grade-like names
    grade_like <- potential_grades[grepl(paste(grade_patterns, collapse = "|"), potential_grades, ignore.case = TRUE)]

    # If no grade-like columns found, show all non-date/value columns
    if(length(grade_like) == 0) {
      value_patterns <- c("^value$", "^pct$", "^percent$", "^amount$", "^count$", "^measure$")
      value_pattern <- paste(value_patterns, collapse = "|")
      grade_like <- names(data)[!names(data) %in% c("date") &
                               !grepl(value_pattern, names(data), ignore.case = TRUE)]
    }

    return(grade_like)
  })

  # Update recalculation date input range when data changes
  observeEvent(raw_data(), {
    data <- raw_data()
    if(!is.null(data) && "date" %in% names(data)) {
      date_range <- range(data$date, na.rm = TRUE)
      # Set default to middle of date range, with available range
      default_date <- date_range[1] + as.numeric(diff(date_range)) * 0.6
      updateDateInput(session, "recalc_date",
                     value = default_date,
                     min = date_range[1],
                     max = date_range[2])
    }
  })

  # Update grouping variable choices when data changes
  observeEvent(grouping_choices(), {
    choices <- grouping_choices()
    if(!is.null(choices) && length(choices) > 0) {
      updateSelectInput(session, "grouping_var",
                       choices = setNames(choices, choices),
                       selected = choices[1])
    } else {
      updateSelectInput(session, "grouping_var",
                       choices = c("No grouping columns available" = ""),
                       selected = "")
    }
  })

  # Update cohort grade variable choices when data changes
  observeEvent(cohort_grade_choices(), {
    choices <- cohort_grade_choices()
    if(!is.null(choices) && length(choices) > 0) {
      # Try to find grade_level_code first, then grade_level_name, then first available
      preferred_selection <- if("grade_level_code" %in% choices) {
        "grade_level_code"
      } else if("grade_level_name" %in% choices) {
        "grade_level_name"
      } else {
        choices[1]
      }

      updateSelectInput(session, "cohort_grade_var",
                       choices = setNames(choices, choices),
                       selected = preferred_selection)
    } else {
      updateSelectInput(session, "cohort_grade_var",
                       choices = c("No grade columns available" = ""),
                       selected = "")
    }
  })

  # Display date range information (Run Chart tab)
  output$date_info <- renderUI({
    data <- raw_data()
    if (is.null(data) || !"date" %in% names(data)) return(NULL)

    date_range <- range(data$date, na.rm = TRUE)
    all_jan_first <- all(format(data$date, "%m-%d") == "01-01", na.rm = TRUE)
    format_used <- if (all_jan_first) "Years only" else "Full dates (YYYY-MM-DD)"

    div(
      style = "background-color: #f0f8ff; padding: 10px; border-radius: 5px; border: 1px solid #4682b4;",
      HTML(paste0(
        "<strong>Date Range:</strong> ",
        format(date_range[1], "%B %d, %Y"), " to ", format(date_range[2], "%B %d, %Y"),
        " <em>(", nrow(data), " rows, ", format_used, " on x-axis)</em>"
      ))
    )
  })

  # Display grouping variable information (Line Chart tab)
  output$grouping_info <- renderUI({
    data <- raw_data()
    if (is.null(data)) return(NULL)

    if(is.null(input$grouping_var) || input$grouping_var == "" || !input$grouping_var %in% names(data)) {
      div(
        style = "background-color: #fff3cd; padding: 10px; border-radius: 5px; border: 1px solid #ffeaa7;",
        HTML("<strong>Note:</strong> No valid grouping variable selected. Please select a column from the 'Line/Bar Chart Grouping' dropdown above to create multiple lines.")
      )
    } else {
      group_var <- input$grouping_var
      unique_groups <- unique(data[[group_var]])

      div(
        style = "background-color: #d1ecf1; padding: 10px; border-radius: 5px; border: 1px solid #81c784;",
        HTML(paste0(
          "<strong>Grouping Variable:</strong> '", group_var, "' with ", length(unique_groups), " categories: ",
          paste(head(unique_groups, 5), collapse = ", "),
          if(length(unique_groups) > 5) "..." else "",
          " <em>(Labels will appear at end of each line)</em>"
        ))
      )
    }
  })

  # Display bar chart information (Bar Chart tab)
  output$bar_info <- renderUI({
    data <- raw_data()
    if (is.null(data)) return(NULL)

    if(is.null(input$grouping_var) || input$grouping_var == "" || !input$grouping_var %in% names(data)) {
      div(
        style = "background-color: #fff3cd; padding: 10px; border-radius: 5px; border: 1px solid #ffeaa7;",
        HTML("<strong>Note:</strong> No valid grouping variable selected. Please select a column from the 'Line/Bar Chart Grouping' dropdown above to create bars.")
      )
    } else {
      group_var <- input$grouping_var
      unique_groups <- unique(data[[group_var]])
      date_range <- range(data$date, na.rm = TRUE)

      div(
        style = "background-color: #e7f3ff; padding: 10px; border-radius: 5px; border: 1px solid #7fb3d3;",
        HTML(paste0(
          "<strong>Bar Chart Data:</strong> Showing averages across all filtered dates (",
          format(date_range[1], "%B %d, %Y"), " to ", format(date_range[2], "%B %d, %Y"),
          ") with ", length(unique_groups), " categories: ",
          paste(head(unique_groups, 5), collapse = ", "),
          if(length(unique_groups) > 5) "..." else ""
        ))
      )
    }
  })

  # Display trended expectation chart information (Trended Expectation Chart tab)
  output$trended_info <- renderUI({
    data <- raw_data()
    if (is.null(data)) return(NULL)

    date_range <- range(data$date, na.rm = TRUE)

    div(
      style = "background-color: #e8f5e8; padding: 10px; border-radius: 5px; border: 1px solid #4caf50;",
      HTML(paste0(
        "<strong>Trended Expectation Chart:</strong> Linear trend-adjusted expectation chart showing process capability from ",
        format(date_range[1], "%B %d, %Y"), " to ", format(date_range[2], "%B %d, %Y"),
        " <em>(Points colored by sigma signals, centerline shows trend, includes runs analysis)</em>"
      ))
    )
  })

  # Display expectation chart information (Expectation Chart tab)
  output$control_info <- renderUI({
    data <- raw_data()
    if (is.null(data)) return(NULL)

    date_range <- range(data$date, na.rm = TRUE)

    div(
      style = "background-color: #fff8e1; padding: 10px; border-radius: 5px; border: 1px solid #ffa726;",
      HTML(paste0(
        "<strong>Expectation Chart:</strong> Untrended expectation chart showing process capability from ",
        format(date_range[1], "%B %d, %Y"), " to ", format(date_range[2], "%B %d, %Y"),
        " <em>(Points colored by sigma signals, center line shows runs signals)</em>"
      ))
    )
  })

  output$cohort_debug_info <- renderText({
    tryCatch({
      filtered <- filtered_data()

      # Get unique values from key columns
      unique_years <- if(!is.null(filtered) && "year" %in% names(filtered)) {
        if(inherits(filtered$year, c("Date", "POSIXct", "POSIXlt"))) {
          paste(sort(unique(year(filtered$year))), collapse = ", ")
        } else {
          paste(sort(unique(filtered$year)), collapse = ", ")
        }
      } else {
        "No year column or no data"
      }

      unique_grades <- if(!is.null(filtered) && !is.null(input$cohort_grade_var) && input$cohort_grade_var %in% names(filtered)) {
        paste(sort(unique(filtered[[input$cohort_grade_var]])), collapse = ", ")
      } else {
        "No grade column selected or no data"
      }

      paste(
        "=== COHORT DEBUG INFO ===",
        paste("Raw data rows:", if(is.null(raw_data())) "NULL" else nrow(raw_data())),
        paste("Filtered data rows:", if(is.null(filtered_data())) "NULL" else nrow(filtered_data())),
        paste("Grade column selected:", input$cohort_grade_var %||% "NULL"),
        paste("Start grade:", input$cohort_start_grade %||% "NULL"),
        paste("End grade:", input$cohort_end_grade %||% "NULL"),
        paste("Start year:", input$cohort_start_year %||% "NULL"),
        paste("End year:", input$cohort_end_year %||% "NULL"),
        "",
        "=== COLUMN NAMES IN FILTERED DATA ===",
        if(is.null(filtered_data())) "No filtered data" else paste(names(filtered_data()), collapse = ", "),
        "",
        "=== ACTUAL VALUES IN DATA ===",
        paste("Unique years in data:", unique_years),
        paste("Unique grades in data:", unique_grades),
        "",
        "=== YEAR-GRADE MATCHING TEST ===",
        if(!is.null(filtered) && nrow(filtered) > 0) {
          grade_col <- input$cohort_grade_var %||% "grade_level_code"
          if("year" %in% names(filtered) && grade_col %in% names(filtered)) {
            year_match <- if(inherits(filtered$year, c("Date", "POSIXct", "POSIXlt"))) {
              year(filtered$year) == 2018
            } else {
              filtered$year == 2018
            }
            grade_match <- filtered[[grade_col]] == 3
            paste("Sample rows for 2018 + Grade 3:", sum(year_match & grade_match, na.rm = TRUE))
          } else {
            "Missing year or grade column"
          }
        } else {
          "No filtered data to test"
        },
        "",
        "=== COHORT_DATA() FUNCTION RESULT ===",
        paste("Cohort data rows:", if(is.null(try(cohort_data(), silent = TRUE))) "ERROR/NULL" else
              if(is.data.frame(try(cohort_data(), silent = TRUE))) nrow(cohort_data()) else "NOT DATA FRAME"),
        sep = "\n"
      )
    }, error = function(e) {
      paste("ERROR in debug:", e$message)
    })
  })

  output$cohort_data_structure <- renderText({
    tryCatch({
      cohort_result <- cohort_data()
      if(is.null(cohort_result) || nrow(cohort_result) == 0) {
        return("No cohort data available")
      }

      paste(
        "=== COHORT DATA STRUCTURE ===",
        paste("Dimensions:", paste(dim(cohort_result), collapse = " x ")),
        paste("Column names:", paste(names(cohort_result), collapse = ", ")),
        "",
        "=== COLUMN CLASSES ===",
        paste(names(cohort_result), ":", sapply(cohort_result, class), collapse = "\n"),
        "",
        "=== SAMPLE VALUES ===",
        if("progression" %in% names(cohort_result)) {
          paste("Progression values:", paste(head(cohort_result$progression, 3), collapse = ", "))
        } else {
          "No 'progression' column found"
        },
        if("value" %in% names(cohort_result)) {
          paste("Value column values:", paste(head(cohort_result$value, 3), collapse = ", "))
        } else {
          "No 'value' column found"
        },
        sep = "\n"
      )
    }, error = function(e) {
      paste("ERROR in cohort_data():", e$message)
    })
  })

  output$cohort_data_sample <- DT::renderDataTable({
    tryCatch({
      cohort_result <- cohort_data()
      if(is.null(cohort_result) || nrow(cohort_result) == 0) {
        return(data.frame(Message = "No cohort data available"))
      }
      return(cohort_result)
    }, error = function(e) {
      return(data.frame(Error = paste("Error getting cohort data:", e$message)))
    })
  }, options = list(pageLength = 10, scrollX = TRUE))

  output$cohort_info <- renderUI({
    data <- raw_data()
    if (is.null(data)) return(NULL)

    if(is.null(input$cohort_grade_var) || input$cohort_grade_var == "" ||
       !input$cohort_grade_var %in% names(data) ||
       is.null(input$cohort_start_grade) || is.null(input$cohort_end_grade) ||
       is.null(input$cohort_start_year) || is.null(input$cohort_end_year)) {
      div(
        style = "background-color: #fff3cd; padding: 10px; border-radius: 5px; border: 1px solid #ffeaa7;",
        HTML("<strong>Note:</strong> Please select a grade column and specify grade range and year range for cohort tracking.")
      )
    } else {
      grade_var <- input$cohort_grade_var
      start_grade <- input$cohort_start_grade
      end_grade <- input$cohort_end_grade
      start_year <- input$cohort_start_year
      end_year <- input$cohort_end_year

      years_span <- end_year - start_year + 1
      grades_span <- end_grade - start_grade + 1

      div(
        style = "background-color: #f3e5f5; padding: 10px; border-radius: 5px; border: 1px solid #ba68c8;",
        HTML(paste0(
          "<strong>Educational Cohort Tracking:</strong> Using '", grade_var, "' column, following cohort progression from Grade ", start_grade, " to Grade ", end_grade,
          " over ", years_span, " years (", start_year, "-", end_year, "). ",
          "<em>Use filters above to select your demographic cohort (ethnicity, subject, etc.)</em>"
        ))
      )
    }
  })

  # Create reactive plot for run chart
  run_plot <- reactive({
    data <- filtered_data()
    req(data)

    # FIXED: Remove rows with NA values to prevent TRUE/FALSE errors
    data <- data[!is.na(data$date) & !is.na(data$value), ]

    if(nrow(data) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid data after removing NA values", size = 6) + theme_void())
    }

    title_text <- if(is.null(input$run_title) || input$run_title == "") "Run Chart" else input$run_title
    subtitle_text <- if(is.null(input$run_subtitle)) "" else input$run_subtitle
    caption_text <- if(is.null(input$run_caption)) "" else input$run_caption

    median_value <- median(data$value, na.rm = TRUE)

    # Calculate date range for proper text positioning
    date_range <- max(data$date) - min(data$date)
    start_text_pos <- min(data$date) - date_range * 0.05  # 5% before start
    end_text_pos <- max(data$date) + date_range * 0.05    # 5% after end

    # Smart date formatting
    all_jan_first <- all(format(data$date, "%m-%d") == "01-01", na.rm = TRUE)

    if (all_jan_first) {
      date_format <- "%Y"
      date_breaks_interval <- "1 year"
    } else {
      date_format <- "%Y-%m-%d"
      date_range_days <- as.numeric(max(data$date, na.rm = TRUE) - min(data$date, na.rm = TRUE))
      if (date_range_days > 365*5) {
        date_breaks_interval <- "1 year"
      } else if (date_range_days > 365) {
        date_breaks_interval <- "6 months"
      } else if (date_range_days > 90) {
        date_breaks_interval <- "1 month"
      } else {
        date_breaks_interval <- "1 week"
      }
    }

    # Create run chart with fixed formatting standards
    ggplot(data, aes(x = date, y = value)) +

      # Lines: darkgray, linewidth 1.2
      geom_line(color = "darkgray", linewidth = 1.2) +

      # Points: blue, size 2.5
      geom_point(color = "blue", size = 2.5) +

      # Median reference line
      geom_hline(yintercept = median_value, color = "red", linewidth = 1, linetype = "solid", alpha = 0.8) +

      # Median label above the line
      geom_text(aes(x = min(date), y = median_value, label = "Median"),
                color = "red", vjust = -0.5, hjust = 0, size = 8, fontface = "bold") +

      # Y-axis formatting: conditional based on user selection
      scale_y_continuous(
        labels = if(input$format_as_percentage) {
          scales::percent_format(accuracy = 0.1)
        } else {
          function(y) format(y, scientific = FALSE, big.mark = ",")
        },
        expand = expansion(mult = c(0.10, 0.10))
      ) +

      # X-axis formatting
      scale_x_date(
        date_labels = date_format,
        date_breaks = date_breaks_interval,
        expand = expansion(add = 30)
      ) +

      labs(title = title_text, subtitle = subtitle_text, caption = caption_text, x = "Date", y = "Value") +

      theme_minimal(base_size = 26) +

      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1.00),
        axis.title.x = element_text(margin = margin(t = 20)),
        text = element_text(color = "royalblue"),
        plot.title.position = "panel",
        plot.title = element_markdown(color = "darkgreen", size = 26, face = "bold", lineheight = 1.1, margin = margin(2, 0, 0, 0, "lines")),
        plot.subtitle = element_markdown(color = "darkgreen", size = 24, face = "bold", lineheight = 1.0, margin = margin(0, 0, 0, 0, "lines")),
        plot.caption = element_text(size = 22, hjust = 0, vjust = 2, face = "italic", color = "darkblue"),
        axis.text = element_text(color = "black"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
      )
  })

  # Render run chart
  output$run_chart <- renderPlot({
    run_plot()
  })

  # Create reactive plot for line chart
  line_plot <- reactive({
    data <- filtered_data()
    req(data)

    # FIXED: Remove rows with NA values to prevent TRUE/FALSE errors
    data <- data[!is.na(data$date) & !is.na(data$value), ]

    if(nrow(data) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid data after removing NA values", size = 6) + theme_void())
    }

    # Use selected grouping variable
    if(is.null(input$grouping_var) || input$grouping_var == "" || !input$grouping_var %in% names(data)) {
      return(NULL)
    }

    group_var <- input$grouping_var

    title_text <- if(is.null(input$line_title) || input$line_title == "") "Line Chart" else input$line_title
    subtitle_text <- if(is.null(input$line_subtitle)) "" else input$line_subtitle
    caption_text <- if(is.null(input$line_caption)) "" else input$line_caption

    # Smart date formatting
    all_jan_first <- all(format(data$date, "%m-%d") == "01-01", na.rm = TRUE)

    if (all_jan_first) {
      date_format <- "%Y"
      date_breaks_interval <- "1 year"
    } else {
      date_format <- "%Y-%m-%d"
      date_range_days <- as.numeric(max(data$date, na.rm = TRUE) - min(data$date, na.rm = TRUE))
      if (date_range_days > 365*5) {
        date_breaks_interval <- "1 year"
      } else if (date_range_days > 365) {
        date_breaks_interval <- "6 months"
      } else if (date_range_days > 90) {
        date_breaks_interval <- "1 month"
      } else {
        date_breaks_interval <- "1 week"
      }
    }

    # Create data for end-of-line labels
    label_data <- data %>%
      group_by(!!sym(group_var)) %>%
      filter(date == max(date)) %>%
      ungroup()

    colors <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#8E5572", "#007F5F", "#7209B7", "#AA6C39")

    # Create line chart
    ggplot(data, aes(x = date, y = value, color = !!sym(group_var))) +

      geom_line(linewidth = 1.2) +

      scale_color_manual(values = colors) +

      # End-of-line labels
      geom_text(data = label_data,
                aes(x = date, y = value, label = !!sym(group_var), color = !!sym(group_var)),
                hjust = -0.1, vjust = 0.5, size = 6, fontface = "bold") +

      scale_y_continuous(
        labels = if(input$format_as_percentage) {
          scales::percent_format(accuracy = 0.1)
        } else {
          function(y) format(y, scientific = FALSE, big.mark = ",")
        },
        expand = expansion(mult = c(0.10, 0.10))
      ) +

      scale_x_date(
        date_labels = date_format,
        date_breaks = date_breaks_interval,
        expand = expansion(mult = c(0.05, 0.15))
      ) +

      labs(title = title_text, subtitle = subtitle_text, caption = caption_text, x = "Date", y = "Value") +

      theme_minimal(base_size = 26) +

      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1.00),
        axis.title.x = element_text(margin = margin(t = 20)),
        text = element_text(color = "royalblue"),
        plot.title.position = "panel",
        plot.title = element_markdown(color = "darkgreen", size = 26, face = "bold", lineheight = 1.1, margin = margin(2, 0, 0, 0, "lines")),
        plot.subtitle = element_markdown(color = "darkgreen", size = 24, face = "bold", lineheight = 1.0, margin = margin(0, 0, 0, 0, "lines")),
        plot.caption = element_text(size = 22, hjust = 0, vjust = 2, face = "italic", color = "darkblue"),
        axis.text = element_text(color = "black"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
      )
  })

  # Render line chart
  output$line_chart <- renderPlot({
    line_plot()
  })

  # FIXED: Create reactive plot for expectation chart with improved NA handling and recalculation
  control_plot <- reactive({
    data <- filtered_data()
    req(data)

    # FIXED: Remove rows with NA values to prevent TRUE/FALSE errors
    data <- data[!is.na(data$date) & !is.na(data$value), ]

    if(nrow(data) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid data after removing NA values", size = 6) + theme_void())
    }

    title_text <- if(is.null(input$control_title) || input$control_title == "") "Untrended Expectation Chart" else input$control_title
    subtitle_text <- if(is.null(input$control_subtitle)) "" else input$control_subtitle
    caption_text <- if(is.null(input$control_caption)) "" else input$control_caption

    # IMPROVED: Better recalculation logic with date range checking
    recalc_enabled <- !is.null(input$enable_recalc) && input$enable_recalc &&
                     !is.null(input$recalc_date) &&
                     input$recalc_date >= min(data$date, na.rm = TRUE) &&
                     input$recalc_date <= max(data$date, na.rm = TRUE)

    if(recalc_enabled) {
      # RECALCULATION MODE: Split data and calculate separate expectation limits
      recalc_date <- input$recalc_date

      # Split data into two segments
      data_before <- data[data$date < recalc_date, ]
      data_after <- data[data$date >= recalc_date, ]

      # Calculate original expectation chart statistics (from before segment or full data)
      # Using full data for original calculation to maintain historical context
      emp_cl_orig <- mean(data$value, na.rm = TRUE)
      sigma_orig <- sd(data$value, na.rm = TRUE)
      emp_ucl_orig <- emp_cl_orig + 3 * sigma_orig
      emp_lcl_orig <- emp_cl_orig - 3 * sigma_orig

      # Calculate recalculated expectation chart statistics (from after segment)
      if(nrow(data_after) >= 3) {  # Need minimum points for statistics
        emp_cl_recalc <- mean(data_after$value, na.rm = TRUE)
        sigma_recalc <- sd(data_after$value, na.rm = TRUE)
        emp_ucl_recalc <- emp_cl_recalc + 3 * sigma_recalc
        emp_lcl_recalc <- emp_cl_recalc - 3 * sigma_recalc
      } else {
        # Fall back to original if insufficient data
        emp_cl_recalc <- emp_cl_orig
        sigma_recalc <- sigma_orig
        emp_ucl_recalc <- emp_ucl_orig
        emp_lcl_recalc <- emp_lcl_orig
      }

      # Add expectation chart columns to data
      data$emp_cl_orig <- emp_cl_orig
      data$emp_ucl_orig <- emp_ucl_orig
      data$emp_lcl_orig <- emp_lcl_orig
      data$emp_cl_recalc <- emp_cl_recalc
      data$emp_ucl_recalc <- emp_ucl_recalc
      data$emp_lcl_recalc <- emp_lcl_recalc
      data$recalc_date <- recalc_date

      # FIXED: Calculate sigma signals using appropriate limits for each segment with NA handling
      data$sigma_signals <- ifelse(!is.na(data$date) & data$date < recalc_date,
                                  (!is.na(data$value) & (data$value > emp_ucl_orig | data$value < emp_lcl_orig)),
                                  (!is.na(data$value) & (data$value > emp_ucl_recalc | data$value < emp_lcl_recalc)))

      # FIXED: Calculate runs signals across both segments with NA handling
      data$above_cl <- ifelse(!is.na(data$date) & data$date < recalc_date,
                             (!is.na(data$value) & data$value > emp_cl_orig),
                             (!is.na(data$value) & data$value > emp_cl_recalc))
      data$runs_signal <- FALSE

      # Simple runs test implementation across segments with NA safety
      for(i in 8:nrow(data)) {
        if(all(!is.na(data$above_cl[(i-7):i]))) {
          if(all(data$above_cl[(i-7):i]) || all(!data$above_cl[(i-7):i])) {
            data$runs_signal[(i-7):i] <- TRUE
          }
        }
      }

      # Create annotation data for original limits
      annotation_data_orig <- data.frame(
        x_pos = min(data$date) + as.numeric(diff(range(data$date))) * 0.02,
        y_pos = emp_ucl_orig + (emp_ucl_orig - emp_lcl_orig) * 0.02,
        label = paste0("Original: UCL = ", round(emp_ucl_orig, 2),
                      ", CL = ", round(emp_cl_orig, 2),
                      ", LCL = ", round(emp_lcl_orig, 2))
      )

      # FIXED: Move recalculated annotation box to the right
      annotation_data_recalc <- data.frame(
        x_pos = min(data$date) + as.numeric(diff(range(data$date))) * 0.55,  # Moved from 0.02 to 0.55
        y_pos = emp_lcl_orig - (emp_ucl_orig - emp_lcl_orig) * 0.08,
        label = paste0("Recalculated (from ", format(recalc_date, "%Y-%m-%d"), "): UCL = ", round(emp_ucl_recalc, 2),
                      ", CL = ", round(emp_cl_recalc, 2),
                      ", LCL = ", round(emp_lcl_recalc, 2))
      )

      # Create annotation data for runs analysis
      annotation_data_runs <- data.frame(
        x_pos = min(data$date) + as.numeric(diff(range(data$date))) * 0.02,
        y_pos = emp_lcl_orig - (emp_ucl_orig - emp_lcl_orig) * 0.15,
        label = ifelse(any(data$runs_signal, na.rm = TRUE), "Runs Signal Detected", "No Runs Signal")
      )

    } else {
      # STANDARD MODE: Original untrended expectation chart
      emp_cl <- mean(data$value, na.rm = TRUE)
      sigma <- sd(data$value, na.rm = TRUE)
      emp_ucl <- emp_cl + 3 * sigma
      emp_lcl <- emp_cl - 3 * sigma

      # Add expectation chart columns to data
      data$emp_cl <- emp_cl
      data$emp_ucl <- emp_ucl
      data$emp_lcl <- emp_lcl

      # FIXED: Calculate sigma signals with NA handling
      data$sigma_signals <- !is.na(data$value) & (data$value > emp_ucl | data$value < emp_lcl)

      # FIXED: Calculate runs signals with NA handling
      data$above_cl <- !is.na(data$value) & data$value > emp_cl
      data$runs_signal <- FALSE

      # Simple runs test implementation with NA safety
      for(i in 8:nrow(data)) {
        if(all(!is.na(data$above_cl[(i-7):i]))) {
          if(all(data$above_cl[(i-7):i]) || all(!data$above_cl[(i-7):i])) {
            data$runs_signal[(i-7):i] <- TRUE
          }
        }
      }

      # Create annotation data for limits
      annotation_data_limits <- data.frame(
        x_pos = min(data$date) + as.numeric(diff(range(data$date))) * 0.02,
        y_pos = emp_ucl + (emp_ucl - emp_lcl) * 0.02,
        label = paste0("UCL = ", round(emp_ucl, 2), "<br>CL = ", round(emp_cl, 2), "<br>LCL = ", round(emp_lcl, 2))
      )

      # Create annotation data for runs analysis
      annotation_data_runs <- data.frame(
        x_pos = min(data$date) + as.numeric(diff(range(data$date))) * 0.02,
        y_pos = emp_lcl - (emp_ucl - emp_lcl) * 0.05,
        label = ifelse(any(data$runs_signal, na.rm = TRUE), "Runs Signal Detected", "No Runs Signal")
      )
    }

    # Smart date formatting
    all_jan_first <- all(format(data$date, "%m-%d") == "01-01", na.rm = TRUE)

    if (all_jan_first) {
      date_format <- "%Y"
      date_breaks_interval <- "1 year"
    } else {
      date_format <- "%Y-%m-%d"
      date_range_days <- as.numeric(max(data$date, na.rm = TRUE) - min(data$date, na.rm = TRUE))
      if (date_range_days > 365*5) {
        date_breaks_interval <- "1 year"
      } else if (date_range_days > 365) {
        date_breaks_interval <- "6 months"
      } else if (date_range_days > 90) {
        date_breaks_interval <- "1 month"
      } else {
        date_breaks_interval <- "1 week"
      }
    }

    # Create expectation chart plot
    p <- ggplot(data, aes(x = date, y = value)) +

      # Connect the dots with lines
      geom_line(color = "darkgray", linewidth = 1.2) +

      # Points colored by sigma signals
      geom_point(aes(color = sigma_signals), size = 2.5) +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue"))

    if(recalc_enabled) {
      # RECALCULATION MODE: Show both sets of expectation limits

      # Original centerline (before recalc date)
      p <- p + geom_line(aes(y = ifelse(!is.na(date) & date < recalc_date, emp_cl_orig, NA),
                            linetype = factor(runs_signal)), color = "blue", linewidth = 1) +

      # Recalculated centerline (after recalc date)
      geom_line(aes(y = ifelse(!is.na(date) & date >= recalc_date, emp_cl_recalc, NA),
                   linetype = factor(runs_signal)), color = "green", linewidth = 1) +

      # Original expectation limits (before recalc date)
      geom_line(aes(y = ifelse(!is.na(date) & date < recalc_date, emp_ucl_orig, NA)), color = "red", linetype = "solid", linewidth = 1) +
      geom_line(aes(y = ifelse(!is.na(date) & date < recalc_date, emp_lcl_orig, NA)), color = "red", linetype = "solid", linewidth = 1) +

      # Recalculated expectation limits (after recalc date)
      geom_line(aes(y = ifelse(!is.na(date) & date >= recalc_date, emp_ucl_recalc, NA)), color = "red", linetype = "dashed", linewidth = 1) +
      geom_line(aes(y = ifelse(!is.na(date) & date >= recalc_date, emp_lcl_recalc, NA)), color = "red", linetype = "dashed", linewidth = 1) +

      # Vertical line at recalculation point
      geom_vline(xintercept = recalc_date, color = "purple", linetype = "dotted", linewidth = 1.5, alpha = 0.7) +

      # Rich text annotations for both sets of limits
      ggtext::geom_richtext(
        data = annotation_data_orig,
        aes(x = x_pos, y = y_pos, label = label),
        size = 5, color = "black", hjust = 0, vjust = 1,
        fill = "lightblue", label.color = "black"
      ) +

      ggtext::geom_richtext(
        data = annotation_data_recalc,
        aes(x = x_pos, y = y_pos, label = label),
        size = 5, color = "black", hjust = 0, vjust = 1,
        fill = "lightgreen", label.color = "black"
      ) +

      ggtext::geom_richtext(
        data = annotation_data_runs,
        aes(x = x_pos, y = y_pos, label = label),
        size = 5, color = "black", hjust = 0, vjust = 0,
        fill = "lightyellow", label.color = "black"
      ) +

      # Labels for original limits
      geom_text(aes(x = dplyr::first(date), y = dplyr::first(emp_cl_orig), label = "Original Expectation"),
                color = "blue", vjust = -1, hjust = 1.1, size = 5) +

      # Labels for recalculated limits
      geom_text(aes(x = recalc_date, y = emp_cl_recalc, label = "Recalc Expectation"),
                color = "green", vjust = -1, hjust = -0.1, size = 5) +

      # Recalculation point label
      geom_text(aes(x = recalc_date, y = max(c(emp_ucl_orig, emp_ucl_recalc), na.rm = TRUE), label = "Recalc Point"),
                color = "purple", vjust = -0.5, hjust = 0.5, size = 5, fontface = "bold")

    } else {
      # STANDARD MODE: Original expectation chart display
      p <- p +
      # Center line with linetype based on runs signals
      geom_line(aes(y = emp_cl, linetype = factor(runs_signal)), color = "blue", linewidth = 1) +
      scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "dashed")) +

      # Upper and lower expectation limits
      geom_line(aes(y = emp_ucl), color = "red", linetype = "solid", linewidth = 1) +
      geom_line(aes(y = emp_lcl), color = "red", linetype = "solid", linewidth = 1) +

      # Rich text annotations
      ggtext::geom_richtext(
        data = annotation_data_limits,
        aes(x = x_pos, y = y_pos, label = label),
        size = 5, color = "black", hjust = 0, vjust = 1,
        fill = "lightblue", label.color = "black"
      ) +

      ggtext::geom_richtext(
        data = annotation_data_runs,
        aes(x = x_pos, y = y_pos, label = label),
        size = 5, color = "black", hjust = 0, vjust = 0,
        fill = "lightyellow", label.color = "black"
      ) +

      # Standard labels
      geom_text(aes(x = dplyr::first(date), y = dplyr::first(emp_cl), label = "Avg Expectation"),
                color = "blue", vjust = -1, hjust = 1.1, size = 5) +

      geom_text(aes(x = dplyr::last(date), y = dplyr::last(emp_cl),
                    label = format(round(dplyr::last(emp_cl), 2), nsmall = 2)),
                color = "blue", vjust = 0, hjust = -0.5, size = 5) +

      geom_text(aes(x = dplyr::first(date) + 0.5, y = dplyr::first(emp_ucl), label = "Upper Expectation"),
                color = "red", vjust = -1, hjust = 1.1, size = 5) +

      geom_text(aes(x = dplyr::last(date), y = dplyr::last(emp_ucl),
                    label = format(round(dplyr::last(emp_ucl), 2), nsmall = 2)),
                color = "red", vjust = -1, hjust = -0.5, size = 5) +

      geom_text(aes(x = dplyr::first(date) + 0.5, y = dplyr::first(emp_lcl), label = "Lower Expectation"),
                color = "red", vjust = 1.75, hjust = 1.1, size = 5) +

      geom_text(aes(x = dplyr::last(date), y = dplyr::last(emp_lcl),
                    label = format(round(dplyr::last(emp_lcl), 2), nsmall = 2)),
                color = "red", vjust = 1.5, hjust = -0.5, size = 5)
    }

    # Complete the plot with formatting
    p <- p +
      # Y-axis formatting with generous space for labels
      scale_y_continuous(
        labels = if(input$format_as_percentage) {
          scales::percent_format(accuracy = 0.1)
        } else {
          function(y) format(y, scientific = FALSE, big.mark = ",")
        },
        expand = expansion(mult = c(0.20, 0.20))
      ) +

      # X-axis formatting
      scale_x_date(
        date_labels = date_format,
        date_breaks = date_breaks_interval,
        expand = expansion(mult = c(0.1, 0.1))
      ) +

      labs(title = title_text, subtitle = subtitle_text, caption = caption_text, x = "Date", y = "Value") +

      theme_minimal(base_size = 26) +

      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1.00),
        axis.title.x = element_text(margin = margin(t = 20)),
        text = element_text(color = "royalblue"),
        plot.title.position = "panel",
        plot.title = element_markdown(color = "darkgreen", size = 26, face = "bold", lineheight = 1.1, margin = margin(2, 0, 0, 0, "lines")),
        plot.subtitle = element_markdown(color = "darkgreen", size = 24, face = "bold", lineheight = 1.0, margin = margin(0, 0, 0, 0, "lines")),
        plot.caption = element_text(size = 22, hjust = 0, vjust = 2, face = "italic", color = "darkblue"),
        axis.text = element_text(color = "black"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
      )

    return(p)
  })

  # Create reactive plot for bar chart
  bar_plot <- reactive({
    data <- filtered_data()
    req(data)

    # FIXED: Remove rows with NA values to prevent TRUE/FALSE errors
    data <- data[!is.na(data$date) & !is.na(data$value), ]

    if(nrow(data) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid data after removing NA values", size = 6) + theme_void())
    }

    # Use selected grouping variable
    if(is.null(input$grouping_var) || input$grouping_var == "" || !input$grouping_var %in% names(data)) {
      return(NULL)
    }

    group_var <- input$grouping_var

    # Calculate averages across all user-selected dates for each group
    bar_data <- data %>%
      group_by(!!sym(group_var)) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = 'drop')

    title_text <- if(is.null(input$bar_title) || input$bar_title == "") "Bar Chart" else input$bar_title
    subtitle_text <- if(is.null(input$bar_subtitle)) "" else input$bar_subtitle
    caption_text <- if(is.null(input$bar_caption)) "" else input$bar_caption

    colors <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#8E5572", "#007F5F", "#7209B7", "#AA6C39")

    # Create bar chart
    ggplot(bar_data, aes(x = !!sym(group_var), y = value, fill = !!sym(group_var))) +

      geom_col(width = 0.7, alpha = 0.8) +

      scale_fill_manual(values = colors) +

      scale_y_continuous(
        labels = if(input$format_as_percentage) {
          scales::percent_format(accuracy = 0.1)
        } else {
          function(y) format(y, scientific = FALSE, big.mark = ",")
        },
        expand = expansion(mult = c(0.10, 0.10))
      ) +

      scale_x_discrete(expand = expansion(add = 0.6)) +

      # Flip coordinates so values are horizontal
      coord_flip() +

      labs(title = title_text, subtitle = subtitle_text, caption = caption_text,
           x = tools::toTitleCase(group_var), y = "Average Value") +

      theme_minimal(base_size = 26) +

      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5),  # No angle needed for horizontal values
        axis.text.y = element_text(angle = 0, hjust = 1.0),  # Groups on y-axis
        axis.title.x = element_text(margin = margin(t = 20)),
        text = element_text(color = "royalblue"),
        plot.title.position = "panel",
        plot.title = element_markdown(color = "darkgreen", size = 26, face = "bold", lineheight = 1.1, margin = margin(2, 0, 0, 0, "lines")),
        plot.subtitle = element_markdown(color = "darkgreen", size = 24, face = "bold", lineheight = 1.0, margin = margin(0, 0, 0, 0, "lines")),
        plot.caption = element_text(size = 22, hjust = 0, vjust = 2, face = "italic", color = "darkblue"),
        axis.text = element_text(color = "black"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
      )
  })

  # Render bar chart
  output$bar_chart <- renderPlot({
    bar_plot()
  })

  # Create reactive plot for trended expectation chart
  trended_plot <- reactive({
    data <- filtered_data()
    req(data)

    # FIXED: Remove rows with NA values to prevent TRUE/FALSE errors
    data <- data[!is.na(data$date) & !is.na(data$value), ]

    if(nrow(data) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid data after removing NA values", size = 6) + theme_void())
    }

    title_text <- if(is.null(input$trended_title) || input$trended_title == "") "Trended Expectation Chart" else input$trended_title
    subtitle_text <- if(is.null(input$trended_subtitle)) "" else input$trended_subtitle
    caption_text <- if(is.null(input$trended_caption)) "" else input$trended_caption

    # Convert dates to numeric for linear modeling (days since first date)
    data$date_numeric <- as.numeric(data$date - min(data$date))

    # Fit linear model
    trend_model <- lm(value ~ date_numeric, data = data)

    # Calculate residuals standard deviation with bias correction
    residuals_sd <- sd(residuals(trend_model))
    bias_corrected_sd <- residuals_sd / 1.128

    # Calculate trended centerline and expectation limits
    data$trended_cl <- predict(trend_model, newdata = data)
    data$trended_ucl <- data$trended_cl + 3 * bias_corrected_sd
    data$trended_lcl <- data$trended_cl - 3 * bias_corrected_sd

    # FIXED: Calculate sigma signals (points outside expectation limits) with NA handling
    data$sigma_signals <- !is.na(data$value) & (data$value > data$trended_ucl | data$value < data$trended_lcl)

    # Runs analysis (full statistical test as provided)
    runs <- sign(data$value - data$trended_cl)
    runs <- runs[runs != 0 & !is.na(runs)]  # FIXED: Remove NA values
    runs <- rle(runs)$lengths
    n.obs <- sum(runs)
    longest.run <- max(runs, na.rm = TRUE)
    n.runs <- length(runs)
    n.crossings <- n.runs - 1
    longest.run.max <- round(log2(n.obs) + 3)
    n.crossings.min <- qbinom(.05, n.obs - 1, 0.5)
    runs.signal <- longest.run > longest.run.max | n.crossings < n.crossings.min

    # Add runs signal to data (all points get same value)
    data$runs_signal <- runs.signal

    # Create annotation data for limits
    annotation_data_limits <- data.frame(
      x_pos = min(data$date) + as.numeric(diff(range(data$date))) * 0.02,
      y_pos = max(data$trended_ucl) + (max(data$trended_ucl) - min(data$trended_lcl)) * 0.02,
      label = paste0("Trend Model: y = ", round(coef(trend_model)[1], 2), " + ",
                     round(coef(trend_model)[2], 4), " × days<br>",
                     "Bias-corrected σ = ", round(bias_corrected_sd, 2))
    )

    # Create annotation data for runs analysis
    annotation_data_runs <- data.frame(
      x_pos = min(data$date) + as.numeric(diff(range(data$date))) * 0.02,
      y_pos = min(data$trended_lcl) - (max(data$trended_ucl) - min(data$trended_lcl)) * 0.05,
      label = paste0("Longest run: ", longest.run, " (max: ", longest.run.max, ")<br>",
                     "Crossings: ", n.crossings, " (min: ", n.crossings.min, ")<br>",
                     ifelse(runs.signal, "Runs Signal Detected", "No Runs Signal"))
    )

    # Smart date formatting
    all_jan_first <- all(format(data$date, "%m-%d") == "01-01", na.rm = TRUE)

    if (all_jan_first) {
      date_format <- "%Y"
      date_breaks_interval <- "1 year"
    } else {
      date_format <- "%Y-%m-%d"
      date_range_days <- as.numeric(max(data$date, na.rm = TRUE) - min(data$date, na.rm = TRUE))
      if (date_range_days > 365*5) {
        date_breaks_interval <- "1 year"
      } else if (date_range_days > 365) {
        date_breaks_interval <- "6 months"
      } else if (date_range_days > 90) {
        date_breaks_interval <- "1 month"
      } else {
        date_breaks_interval <- "1 week"
      }
    }

    # Create trended expectation chart
    ggplot(data, aes(x = date, y = value)) +

      # Connect the dots with lines
      geom_line(color = "darkgray", linewidth = 1.2) +

      # Points colored by sigma signals (red = outside limits, blue = within)
      geom_point(aes(color = sigma_signals), size = 2.5) +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +

      # Trended centerline with linetype based on runs signals
      geom_line(aes(y = trended_cl, linetype = factor(runs_signal)), color = "blue", linewidth = 1) +
      scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "dashed")) +

      # Upper and lower expectation limits (red dotted lines for trended)
      geom_line(aes(y = trended_ucl), color = "red", linetype = "dotted", linewidth = 1) +
      geom_line(aes(y = trended_lcl), color = "red", linetype = "dotted", linewidth = 1) +

      # Rich text annotation for trend model info
      ggtext::geom_richtext(
        data = annotation_data_limits,
        aes(x = x_pos, y = y_pos, label = label),
        size = 5,
        color = "black",
        hjust = 0,
        vjust = 1,
        fill = "lightblue",
        label.color = "black"
      ) +

      # Rich text annotation for runs analysis
      ggtext::geom_richtext(
        data = annotation_data_runs,
        aes(x = x_pos, y = y_pos, label = label),
        size = 5,
        color = "black",
        hjust = 0,
        vjust = 0,
        fill = "lightyellow",
        label.color = "black"
      ) +

      # Label for trended centerline at start
      geom_text(aes(x = dplyr::first(date), y = dplyr::first(trended_cl), label = "Trended Expectation"),
                color = "blue", vjust = -1, hjust = 1.1, size = 5) +

      # Value for trended centerline at end
      geom_text(aes(x = dplyr::last(date), y = dplyr::last(trended_cl),
                    label = format(round(dplyr::last(trended_cl), 2), nsmall = 2)),
                color = "blue", vjust = 0, hjust = -0.5, size = 5) +

      # Label for upper limit at start
      geom_text(aes(x = dplyr::first(date) + 0.5, y = dplyr::first(trended_ucl), label = "Upper Trend Limit"),
                color = "red", vjust = -1, hjust = 1.1, size = 5) +

      # Value for upper limit at end
      geom_text(aes(x = dplyr::last(date), y = dplyr::last(trended_ucl),
                    label = format(round(dplyr::last(trended_ucl), 2), nsmall = 2)),
                color = "red", vjust = -1, hjust = -0.5, size = 5) +

      # Label for lower limit at start
      geom_text(aes(x = dplyr::first(date) + 0.5, y = dplyr::first(trended_lcl), label = "Lower Trend Limit"),
                color = "red", vjust = 1.75, hjust = 1.1, size = 5) +

      # Value for lower limit at end
      geom_text(aes(x = dplyr::last(date), y = dplyr::last(trended_lcl),
                    label = format(round(dplyr::last(trended_lcl), 2), nsmall = 2)),
                color = "red", vjust = 1.5, hjust = -0.5, size = 5) +

      # Y-axis formatting with generous space for labels
      scale_y_continuous(
        labels = if(input$format_as_percentage) {
          scales::percent_format(accuracy = 0.1)
        } else {
          function(y) format(y, scientific = FALSE, big.mark = ",")
        },
        expand = expansion(mult = c(0.20, 0.20))
      ) +

      # X-axis formatting
      scale_x_date(
        date_labels = date_format,
        date_breaks = date_breaks_interval,
        expand = expansion(mult = c(0.1, 0.1))
      ) +

      labs(title = title_text, subtitle = subtitle_text, caption = caption_text, x = "Date", y = "Value") +

      theme_minimal(base_size = 26) +

      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1.00),
        axis.title.x = element_text(margin = margin(t = 20)),
        text = element_text(color = "royalblue"),
        plot.title.position = "panel",
        plot.title = element_markdown(color = "darkgreen", size = 26, face = "bold", lineheight = 1.1, margin = margin(2, 0, 0, 0, "lines")),
        plot.subtitle = element_markdown(color = "darkgreen", size = 24, face = "bold", lineheight = 1.0, margin = margin(0, 0, 0, 0, "lines")),
        plot.caption = element_text(size = 22, hjust = 0, vjust = 2, face = "italic", color = "darkblue"),
        axis.text = element_text(color = "black"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
      )
  })

  # NEW: Create reactive plot for educational cohort tracking
  cohort_plot <- reactive({
    data <- filtered_data()
    req(data)

    # FIXED: Remove rows with NA values to prevent TRUE/FALSE errors
    data <- data[!is.na(data$date) & !is.na(data$value), ]

    if(nrow(data) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid data after removing NA values", size = 6) + theme_void())
    }

    # Check if required inputs are available
    if(is.null(input$cohort_grade_var) || input$cohort_grade_var == "" ||
       !input$cohort_grade_var %in% names(data) ||
       is.null(input$cohort_start_grade) || is.null(input$cohort_end_grade) ||
       is.null(input$cohort_start_year) || is.null(input$cohort_end_year)) {
      return(NULL)
    }

    grade_var <- input$cohort_grade_var
    start_grade <- input$cohort_start_grade
    end_grade <- input$cohort_end_grade
    start_year <- input$cohort_start_year
    end_year <- input$cohort_end_year

    title_text <- if(is.null(input$cohort_title) || input$cohort_title == "") "Educational Cohort Analysis" else input$cohort_title
    subtitle_text <- if(is.null(input$cohort_subtitle)) "" else input$cohort_subtitle
    caption_text <- if(is.null(input$cohort_caption)) "" else input$cohort_caption

    # Validate inputs
    if(end_grade < start_grade || end_year < start_year) {
      return(NULL)
    }

    # Create cohort progression data
    # Year 1: Grade start_grade, Year 2: Grade start_grade+1, etc.
    cohort_data <- data.frame()

    current_year <- start_year
    current_grade <- start_grade

    while(current_year <= end_year && current_grade <= end_grade) {
      # Check if data has 'year' column, otherwise extract from 'date'
      year_matches <- if("year" %in% names(data)) {
        # Extract year from date objects if needed
        if(inherits(data$year, c("Date", "POSIXct", "POSIXlt"))) {
          year(data$year) == current_year
        } else {
          data$year == current_year
        }
      } else if("date" %in% names(data)) {
        year(data$date) == current_year
      } else {
        rep(TRUE, nrow(data))  # If no year info, include all
      }

      # Use user-selected grade column
      grade_matches <- if(is.numeric(data[[grade_var]])) {
        # If numeric grade column (like grade_level_code: 3,4,5,6,7,8)
        data[[grade_var]] == current_grade
      } else {
        # If text grade column (like grade_level_name: "Grade 3", "Grade 4", etc.)
        # Try to extract number from text or match text patterns
        grade_text_patterns <- paste0("\\b", current_grade, "\\b")  # Word boundary match
        grepl(grade_text_patterns, data[[grade_var]], ignore.case = TRUE)
      }

      # Get data for this year-grade combination
      year_grade_data <- data[year_matches & grade_matches, ]

      if(nrow(year_grade_data) > 0) {
        # Extract the single value directly (no averaging needed)
        cohort_value <- year_grade_data$value[1]  # Take first/only value

        # Add to cohort progression
        cohort_data <- rbind(cohort_data, data.frame(
          year = current_year,
          grade = current_grade,
          cohort_year = current_year - start_year + 1,
          value = cohort_value
        ))
      }

      current_year <- current_year + 1
      current_grade <- current_grade + 1
    }

    # If no valid data found, return NULL
    if(nrow(cohort_data) == 0) {
      return(NULL)
    }

    # Create the cohort progression line chart
    ggplot(cohort_data, aes(x = grade, y = value)) +

      # Connect dots with line
      geom_line(color = "#2E86AB", linewidth = 1.5) +

      # Points for each grade
      geom_point(color = "#A23B72", size = 3.5) +

      # Add value labels above points (with better precision)
      geom_text(aes(label = format(round(value, 3), nsmall = 3)),
                color = "black", vjust = -0.8, size = 5, fontface = "bold") +

      # Add year labels below points (larger and more distinct)
      geom_text(aes(label = paste0("(", year, ")")),
                color = "darkgreen", vjust = 2.5, size = 7, fontface = "bold") +

      scale_x_continuous(
        breaks = cohort_data$grade,
        labels = paste("Grade", cohort_data$grade),
        expand = expansion(mult = c(0.1, 0.1))
      ) +

      scale_y_continuous(
        labels = if(input$format_as_percentage) {
          scales::percent_format(accuracy = 0.1)
        } else {
          function(y) format(y, scientific = FALSE, big.mark = ",")
        },
        expand = expansion(mult = c(0.15, 0.15))
      ) +

      labs(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        x = "Grade Level",
        y = "Performance Value"
      ) +

      theme_minimal(base_size = 26) +

      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1.0),
        axis.text.y = element_text(angle = 0, hjust = 1.0),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        text = element_text(color = "royalblue"),
        plot.title.position = "panel",
        plot.title = element_markdown(color = "darkgreen", size = 26, face = "bold", lineheight = 1.1, margin = margin(2, 0, 0, 0, "lines")),
        plot.subtitle = element_markdown(color = "darkgreen", size = 24, face = "bold", lineheight = 1.0, margin = margin(0, 0, 0, 0, "lines")),
        plot.caption = element_text(size = 22, hjust = 0, vjust = 2, face = "italic", color = "darkblue"),
        axis.text = element_text(color = "black"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
      )
  })

  # Cohort data reactive function (for compatibility with existing chart render code)
  cohort_data <- reactive({
    # Check if required inputs are available
    if(is.null(input$cohort_grade_var) || input$cohort_grade_var == "" ||
       is.null(input$cohort_start_grade) || is.null(input$cohort_end_grade) ||
       is.null(input$cohort_start_year) || is.null(input$cohort_end_year)) {
      return(data.frame())
    }

    data <- filtered_data()
    if(is.null(data) || nrow(data) == 0) {
      return(data.frame())
    }

    # FIXED: Remove rows with NA values
    data <- data[!is.na(data$date) & !is.na(data$value), ]

    if(nrow(data) == 0) {
      return(data.frame())
    }

    grade_var <- input$cohort_grade_var
    start_grade <- as.numeric(input$cohort_start_grade)
    end_grade <- as.numeric(input$cohort_end_grade)
    start_year <- as.numeric(input$cohort_start_year)
    end_year <- as.numeric(input$cohort_end_year)

    # Calculate years to track
    usable_years <- seq(from = start_year, to = end_year)
    years_to_track <- length(usable_years)
    grades <- seq(from = start_grade, by = 1, length.out = years_to_track)

    # Build cohort progression data
    final_result <- data.frame()

    for (i in 1:length(usable_years)) {
      current_year <- usable_years[i]
      current_grade <- grades[i]

      # Handle year matching - extract year from date objects
      year_matches <- if("year" %in% names(data)) {
        # Extract year from date objects if needed
        if(inherits(data$year, c("Date", "POSIXct", "POSIXlt"))) {
          year(data$year) == current_year
        } else {
          data$year == current_year
        }
      } else if("date" %in% names(data)) {
        year(data$date) == current_year
      } else {
        rep(TRUE, nrow(data))
      }

      # Handle grade matching using selected column
      grade_matches <- if(is.numeric(data[[grade_var]])) {
        data[[grade_var]] == current_grade
      } else {
        grade_text_patterns <- paste0("\\b", current_grade, "\\b")
        grepl(grade_text_patterns, data[[grade_var]], ignore.case = TRUE)
      }

      # Filter for this year-grade combination
      year_data <- data[year_matches & grade_matches, ]

      if(nrow(year_data) > 0) {
        final_result <- rbind(final_result, year_data)
      }
    }

    if(nrow(final_result) == 0) {
      return(data.frame())
    }

    # Add progression column for x-axis
    final_result$progression <- paste0("Year ", final_result$year, " (Grade ", final_result[[grade_var]], ")")

    # Sort by year to ensure correct order
    final_result <- final_result %>% arrange(if("year" %in% names(final_result)) year else date)

    return(final_result)
  })

  # Render expectation chart
  output$control_chart <- renderPlot({
    control_plot()
  })

  # Render trended expectation chart
  output$trended_chart <- renderPlot({
    trended_plot()
  })

  # NEW: Render cohort chart
  output$cohort_chart <- renderPlot({
    cohort_plot()
  })

  # Download handlers for run chart
  output$download_run_png <- downloadHandler(
    filename = function() paste0("run_chart_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = run_plot(), device = "png", width = 16, height = 8, dpi = 300, bg = "white")
    }
  )

  output$download_run_svg <- downloadHandler(
    filename = function() paste0("run_chart_", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(file, plot = run_plot(), device = "svg", width = 16, height = 8, bg = "white")
    }
  )

  output$download_run_pdf <- downloadHandler(
    filename = function() paste0("run_chart_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch({
        ggsave(file, plot = run_plot(), device = cairo_pdf, width = 16, height = 8, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = run_plot(), device = "pdf", width = 16, height = 8, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 16, height = 8)
          print(run_plot())
          dev.off()
        })
      })
    }
  )

  # Download handlers for line chart
  output$download_line_png <- downloadHandler(
    filename = function() paste0("line_chart_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = line_plot(), device = "png", width = 16, height = 8, dpi = 300, bg = "white")
    }
  )

  output$download_line_svg <- downloadHandler(
    filename = function() paste0("line_chart_", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(file, plot = line_plot(), device = "svg", width = 16, height = 8, bg = "white")
    }
  )

  output$download_line_pdf <- downloadHandler(
    filename = function() paste0("line_chart_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch({
        ggsave(file, plot = line_plot(), device = cairo_pdf, width = 16, height = 8, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = line_plot(), device = "pdf", width = 16, height = 8, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 16, height = 8)
          print(line_plot())
          dev.off()
        })
      })
    }
  )

  # Download handlers for expectation chart
  output$download_control_png <- downloadHandler(
    filename = function() paste0("expectation_chart_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = control_plot(), device = "png", width = 16, height = 8, dpi = 300, bg = "white")
    }
  )

  output$download_control_svg <- downloadHandler(
    filename = function() paste0("expectation_chart_", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(file, plot = control_plot(), device = "svg", width = 16, height = 8, bg = "white")
    }
  )

  output$download_control_pdf <- downloadHandler(
    filename = function() paste0("expectation_chart_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch({
        ggsave(file, plot = control_plot(), device = cairo_pdf, width = 16, height = 8, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = control_plot(), device = "pdf", width = 16, height = 8, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 16, height = 8)
          print(control_plot())
          dev.off()
        })
      })
    }
  )

  # Download handlers for bar chart
  output$download_bar_png <- downloadHandler(
    filename = function() paste0("bar_chart_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = bar_plot(), device = "png", width = 16, height = 8, dpi = 300, bg = "white")
    }
  )

  output$download_bar_svg <- downloadHandler(
    filename = function() paste0("bar_chart_", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(file, plot = bar_plot(), device = "svg", width = 16, height = 8, bg = "white")
    }
  )

  output$download_bar_pdf <- downloadHandler(
    filename = function() paste0("bar_chart_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch({
        ggsave(file, plot = bar_plot(), device = cairo_pdf, width = 16, height = 8, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = bar_plot(), device = "pdf", width = 16, height = 8, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 16, height = 8)
          print(bar_plot())
          dev.off()
        })
      })
    }
  )

  # Download handlers for trended expectation chart
  output$download_trended_png <- downloadHandler(
    filename = function() paste0("trended_expectation_chart_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = trended_plot(), device = "png", width = 16, height = 8, dpi = 300, bg = "white")
    }
  )

  output$download_trended_svg <- downloadHandler(
    filename = function() paste0("trended_expectation_chart_", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(file, plot = trended_plot(), device = "svg", width = 16, height = 8, bg = "white")
    }
  )

  output$download_trended_pdf <- downloadHandler(
    filename = function() paste0("trended_expectation_chart_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch({
        ggsave(file, plot = trended_plot(), device = cairo_pdf, width = 16, height = 8, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = trended_plot(), device = "pdf", width = 16, height = 8, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 16, height = 8)
          print(trended_plot())
          dev.off()
        })
      })
    }
  )

  # Download handlers for cohort chart
  output$download_cohort_png <- downloadHandler(
    filename = function() paste0("cohort_chart_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = cohort_plot(), device = "png", width = 16, height = 8, dpi = 300, bg = "white")
    }
  )

  output$download_cohort_svg <- downloadHandler(
    filename = function() paste0("cohort_chart_", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(file, plot = cohort_plot(), device = "svg", width = 16, height = 8, bg = "white")
    }
  )

  output$download_cohort_pdf <- downloadHandler(
    filename = function() paste0("cohort_chart_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch({
        ggsave(file, plot = cohort_plot(), device = cairo_pdf, width = 16, height = 8, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = cohort_plot(), device = "pdf", width = 16, height = 8, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 16, height = 8)
          print(cohort_plot())
          dev.off()
        })
      })
    }
  )

  # Observe uploaded data and create filtering controls
  observeEvent(raw_data(), {
    data <- raw_data()
    req(data)

    # Clear existing filter controls
    removeUI(selector = "#filter_controls > *", multiple = TRUE)

    # Get column names excluding value columns
    value_patterns <- c("^value$", "^pct$", "^percent$", "^amount$", "^count$", "^measure$")
    value_pattern <- paste(value_patterns, collapse = "|")
    filter_columns <- names(data)[!grepl(value_pattern, names(data), ignore.case = TRUE)]

    # Create dynamic filter controls
    filter_controls <- map(filter_columns, function(col) {

      if(inherits(data[[col]], "Date") || lubridate::is.Date(data[[col]])) {
        # Date columns get date sliders
        date_values <- data[[col]][!is.na(data[[col]])]
        min_date <- min(date_values, na.rm = TRUE)
        max_date <- max(date_values, na.rm = TRUE)

        column(4,
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
            step = (max(data[[col]], na.rm = TRUE) - min(data[[col]], na.rm = TRUE)) / 100,
            sep = ""
          )
        )

      } else {
        # Categorical columns get multi-select
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

    # Insert filter controls
    insertUI(
      selector = "#filter_controls",
      where = "beforeEnd",
      ui = fluidRow(filter_controls)
    )
  })

  # Observer for "All" and "None" selection logic
  observe({
    data <- raw_data()
    req(data)

    value_patterns <- c("^value$", "^pct$", "^percent$", "^amount$", "^count$", "^measure$")
    value_pattern <- paste(value_patterns, collapse = "|")
    filter_columns <- names(data)[!grepl(value_pattern, names(data), ignore.case = TRUE)]

    for(col in filter_columns) {
      if(!is.numeric(data[[col]]) && !inherits(data[[col]], "Date") && !lubridate::is.Date(data[[col]])) {
        filter_input_name <- paste0("filter_", make.names(col))

        observeEvent(input[[filter_input_name]], {
          current_selection <- input[[filter_input_name]]

          if(!is.null(current_selection)) {
            if("ALL_VALUES" %in% current_selection && length(current_selection) > 1) {
              updateSelectInput(session, filter_input_name, selected = "ALL_VALUES")
            }
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
          if("ALL_VALUES" %in% filter_value) {
            next
          } else if("NO_VALUES" %in% filter_value && length(filter_value) == 1) {
            filtered <- filtered %>% filter(FALSE)
          } else {
            actual_values <- filter_value[!filter_value %in% c("ALL_VALUES", "NO_VALUES")]
            if(length(actual_values) > 0) {
              filtered <- filtered %>%
                filter(as.character(!!sym(col)) %in% actual_values)
            } else {
              filtered <- filtered %>% filter(FALSE)
            }
          }
        }
      }
    }

    return(filtered)
  })

  # Render data table
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
