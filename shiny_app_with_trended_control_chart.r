# Ultimate EDA Shiny App - Complete Version
# Clean, working version with Run Chart, Line Chart, and Bar Chart
# NEW: User-selectable grouping variable for Line/Bar charts

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
        tabPanel("Untrended Control Chart",
          br(),
          fluidRow(
            column(12,
              uiOutput("control_info")
            )
          ),
          br(),
          fluidRow(
            column(4,
              textInput("control_title", "Chart Title:", value = "Untrended Control Chart")
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
        tabPanel("Trended Control Chart",
          br(),
          fluidRow(
            column(12,
              uiOutput("trended_info")
            )
          ),
          br(),
          fluidRow(
            column(4,
              textInput("trended_title", "Chart Title:", value = "Trended Control Chart")
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
              data[[col]] <- as.Date(paste0(data[[col]], "-01-01"))
              converted <- TRUE
            }, silent = FALSE)
          }
          # Check for Excel serial dates
          else if(all(values >= 15000 & values <= 60000)) {
            try({
              data[[col]] <- as.Date(data[[col]], origin = "1899-12-30")
              converted <- TRUE
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

  # Display trended control chart information (Trended Control Chart tab)
  output$trended_info <- renderUI({
    data <- raw_data()
    if (is.null(data)) return(NULL)

    date_range <- range(data$date, na.rm = TRUE)

    div(
      style = "background-color: #e8f5e8; padding: 10px; border-radius: 5px; border: 1px solid #4caf50;",
      HTML(paste0(
        "<strong>Trended Control Chart:</strong> Linear trend-adjusted expectation chart showing process capability from ",
        format(date_range[1], "%B %d, %Y"), " to ", format(date_range[2], "%B %d, %Y"),
        " <em>(Points colored by sigma signals, centerline shows trend, includes runs analysis)</em>"
      ))
    )
  })

  # Display control chart information (Control Chart tab)
  output$control_info <- renderUI({
    data <- raw_data()
    if (is.null(data)) return(NULL)

    date_range <- range(data$date, na.rm = TRUE)

    div(
      style = "background-color: #fff8e1; padding: 10px; border-radius: 5px; border: 1px solid #ffa726;",
      HTML(paste0(
        "<strong>Control Chart:</strong> Untrended expectation chart showing process capability from ",
        format(date_range[1], "%B %d, %Y"), " to ", format(date_range[2], "%B %d, %Y"),
        " <em>(Points colored by sigma signals, center line shows runs signals)</em>"
      ))
    )
  })

  # Create reactive plot for run chart
  run_plot <- reactive({
    data <- filtered_data()
    req(data)

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

  # Create reactive plot for control chart
  control_plot <- reactive({
    data <- filtered_data()
    req(data)

    title_text <- if(is.null(input$control_title) || input$control_title == "") "Untrended Control Chart" else input$control_title
    subtitle_text <- if(is.null(input$control_subtitle)) "" else input$control_subtitle
    caption_text <- if(is.null(input$control_caption)) "" else input$control_caption

    # Calculate control chart statistics
    emp_cl <- mean(data$value, na.rm = TRUE)  # Center line (average)
    sigma <- sd(data$value, na.rm = TRUE)     # Standard deviation
    emp_ucl <- emp_cl + 3 * sigma             # Upper control limit
    emp_lcl <- emp_cl - 3 * sigma             # Lower control limit

    # Add control chart columns to data
    data$emp_cl <- emp_cl
    data$emp_ucl <- emp_ucl
    data$emp_lcl <- emp_lcl

    # Calculate sigma signals (points outside control limits)
    data$sigma_signals <- data$value > emp_ucl | data$value < emp_lcl

    # Calculate runs signals (simplified runs test - 8 consecutive points on same side of center line)
    data$above_cl <- data$value > emp_cl
    data$runs_signal <- FALSE

    # Simple runs test implementation
    for(i in 8:nrow(data)) {
      if(all(data$above_cl[(i-7):i]) || all(!data$above_cl[(i-7):i])) {
        data$runs_signal[(i-7):i] <- TRUE
      }
    }

    # Create annotation data for limits (positioned better)
    annotation_data_limits <- data.frame(
      x_pos = min(data$date) + as.numeric(diff(range(data$date))) * 0.02,
      y_pos = emp_ucl + (emp_ucl - emp_lcl) * 0.02,  # Reduced offset
      label = paste0("UCL = ", round(emp_ucl, 2), "<br>CL = ", round(emp_cl, 2), "<br>LCL = ", round(emp_lcl, 2))
    )

    # Create annotation data for CL type (positioned better)
    annotation_data_cl_type <- data.frame(
      x_pos = min(data$date) + as.numeric(diff(range(data$date))) * 0.02,
      y_pos = emp_lcl - (emp_ucl - emp_lcl) * 0.05,  # Reduced offset
      label = ifelse(any(data$runs_signal), "Runs Signal Detected", "No Runs Signal")
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

    # Create control chart with your specific formatting
    ggplot(data, aes(x = date, y = value)) +

      # Connect the dots with lines (essential for control charts!)
      geom_line(color = "darkgray", linewidth = 1.2) +

      # Points colored by sigma signals (red = outside limits, blue = within)
      geom_point(aes(color = sigma_signals), size = 2.5) +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +

      # Center line with linetype based on runs signals
      geom_line(aes(y = emp_cl, linetype = factor(runs_signal)), color = "blue", linewidth = 1) +
      scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "dashed")) +

      # Upper and lower control limits (red solid lines)
      geom_line(aes(y = emp_ucl), color = "red", linetype = "solid", linewidth = 1) +
      geom_line(aes(y = emp_lcl), color = "red", linetype = "solid", linewidth = 1) +

      # Rich text annotation for CL calculation (smaller font, positioned lower)
      ggtext::geom_richtext(
        data = annotation_data_limits,
        aes(x = x_pos, y = y_pos, label = label),
        size = 5,  # Reduced from 8
        color = "black",
        hjust = 0,
        vjust = 1,  # Changed to position above instead of below
        fill = "lightblue",
        label.color = "black"
      ) +

      # Rich text annotation for CL type (smaller font)
      ggtext::geom_richtext(
        data = annotation_data_cl_type,
        aes(x = x_pos, y = y_pos, label = label),
        size = 5,  # Reduced from 8
        color = "black",
        hjust = 0,
        vjust = 0,
        fill = "lightyellow",
        label.color = "black"
      ) +

      # Label for center line at start (restored font size)
      geom_text(aes(x = dplyr::first(date), y = dplyr::first(emp_cl), label = "Avg Expectation"),
                color = "blue", vjust = -1, hjust = 1.1, size = 5) +  # Was 8

      # Value for center line at end (restored font size)
      geom_text(aes(x = dplyr::last(date), y = dplyr::last(emp_cl),
                    label = format(round(dplyr::last(emp_cl), 2), nsmall = 2)),
                color = "blue", vjust = 0, hjust = -0.5, size = 5) +  # Pulled in from -0.1 to -0.5

      # Label for upper limit at start (restored font size)
      geom_text(aes(x = dplyr::first(date) + 0.5, y = dplyr::first(emp_ucl), label = "Upper Expectation"),
                color = "red", vjust = -1, hjust = 1.1, size = 5) +  # Was 8

      # Value for upper limit at end (restored font size)
      geom_text(aes(x = dplyr::last(date), y = dplyr::last(emp_ucl),
                    label = format(round(dplyr::last(emp_ucl), 2), nsmall = 2)),
                color = "red", vjust = -1, hjust = -0.5, size = 5) +  # Pulled in from -0.1 to -0.5

      # Label for lower limit at start (restored font size)
      geom_text(aes(x = dplyr::first(date) + 0.5, y = dplyr::first(emp_lcl), label = "Lower Expectation"),
                color = "red", vjust = 1.75, hjust = 1.1, size = 5) +  # Was 8

      # Value for lower limit at end (restored font size)
      geom_text(aes(x = dplyr::last(date), y = dplyr::last(emp_lcl),
                    label = format(round(dplyr::last(emp_lcl), 2), nsmall = 2)),
                color = "red", vjust = 1.5, hjust = -0.5, size = 5) +  # Pulled in from -0.1 to -0.5

      # Y-axis formatting: conditional based on user selection with generous space for labels
      scale_y_continuous(
        labels = if(input$format_as_percentage) {
          scales::percent_format(accuracy = 0.1)
        } else {
          function(y) format(y, scientific = FALSE, big.mark = ",")
        },
        expand = expansion(mult = c(0.20, 0.20))  # Generous space for control chart labels
      ) +

      # X-axis formatting with space for text at calculated positions
      scale_x_date(
        date_labels = date_format,
        date_breaks = date_breaks_interval,
        expand = expansion(mult = c(0.1, 0.1))  # 10% expansion on each side
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

  # Create reactive plot for bar chart
  bar_plot <- reactive({
    data <- filtered_data()
    req(data)

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

  # Create reactive plot for trended control chart
  trended_plot <- reactive({
    data <- filtered_data()
    req(data)

    title_text <- if(is.null(input$trended_title) || input$trended_title == "") "Trended Control Chart" else input$trended_title
    subtitle_text <- if(is.null(input$trended_subtitle)) "" else input$trended_subtitle
    caption_text <- if(is.null(input$trended_caption)) "" else input$trended_caption

    # Convert dates to numeric for linear modeling (days since first date)
    data$date_numeric <- as.numeric(data$date - min(data$date))
    
    # Fit linear model
    trend_model <- lm(value ~ date_numeric, data = data)
    
    # Calculate residuals standard deviation with bias correction
    residuals_sd <- sd(residuals(trend_model))
    bias_corrected_sd <- residuals_sd / 1.128
    
    # Calculate trended centerline and control limits
    data$trended_cl <- predict(trend_model, newdata = data)
    data$trended_ucl <- data$trended_cl + 3 * bias_corrected_sd
    data$trended_lcl <- data$trended_cl - 3 * bias_corrected_sd

    # Calculate sigma signals (points outside control limits)
    data$sigma_signals <- data$value > data$trended_ucl | data$value < data$trended_lcl

    # Runs analysis (full statistical test as provided)
    runs <- sign(data$value - data$trended_cl)
    runs <- runs[runs != 0]
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

    # Create trended control chart
    ggplot(data, aes(x = date, y = value)) +

      # Connect the dots with lines
      geom_line(color = "darkgray", linewidth = 1.2) +

      # Points colored by sigma signals (red = outside limits, blue = within)
      geom_point(aes(color = sigma_signals), size = 2.5) +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +

      # Trended centerline with linetype based on runs signals
      geom_line(aes(y = trended_cl, linetype = factor(runs_signal)), color = "blue", linewidth = 1) +
      scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "dashed")) +

      # Upper and lower control limits (red dotted lines for trended)
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

  # Render control chart
  output$control_chart <- renderPlot({
    control_plot()
  })

  # Render trended control chart
  output$trended_chart <- renderPlot({
    trended_plot()
  })

  # Download handlers for run chart
  output$download_run_png <- downloadHandler(
    filename = function() paste0("run_chart_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = run_plot(), device = "png", width = 12, height = 8, dpi = 300, bg = "white")
    }
  )

  output$download_run_svg <- downloadHandler(
    filename = function() paste0("run_chart_", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(file, plot = run_plot(), device = "svg", width = 12, height = 8, bg = "white")
    }
  )

  output$download_run_pdf <- downloadHandler(
    filename = function() paste0("run_chart_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch({
        ggsave(file, plot = run_plot(), device = cairo_pdf, width = 12, height = 8, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = run_plot(), device = "pdf", width = 12, height = 8, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 12, height = 8)
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
      ggsave(file, plot = line_plot(), device = "png", width = 12, height = 8, dpi = 300, bg = "white")
    }
  )

  output$download_line_svg <- downloadHandler(
    filename = function() paste0("line_chart_", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(file, plot = line_plot(), device = "svg", width = 12, height = 8, bg = "white")
    }
  )

  output$download_line_pdf <- downloadHandler(
    filename = function() paste0("line_chart_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch({
        ggsave(file, plot = line_plot(), device = cairo_pdf, width = 12, height = 8, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = line_plot(), device = "pdf", width = 12, height = 8, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 12, height = 8)
          print(line_plot())
          dev.off()
        })
      })
    }
  )

  # Download handlers for control chart
  output$download_control_png <- downloadHandler(
    filename = function() paste0("control_chart_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = control_plot(), device = "png", width = 12, height = 8, dpi = 300, bg = "white")
    }
  )

  output$download_control_svg <- downloadHandler(
    filename = function() paste0("control_chart_", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(file, plot = control_plot(), device = "svg", width = 12, height = 8, bg = "white")
    }
  )

  output$download_control_pdf <- downloadHandler(
    filename = function() paste0("control_chart_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch({
        ggsave(file, plot = control_plot(), device = cairo_pdf, width = 12, height = 8, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = control_plot(), device = "pdf", width = 12, height = 8, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 12, height = 8)
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
      ggsave(file, plot = bar_plot(), device = "png", width = 12, height = 8, dpi = 300, bg = "white")
    }
  )

  output$download_bar_svg <- downloadHandler(
    filename = function() paste0("bar_chart_", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(file, plot = bar_plot(), device = "svg", width = 12, height = 8, bg = "white")
    }
  )

  output$download_bar_pdf <- downloadHandler(
    filename = function() paste0("bar_chart_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch({
        ggsave(file, plot = bar_plot(), device = cairo_pdf, width = 12, height = 8, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = bar_plot(), device = "pdf", width = 12, height = 8, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 12, height = 8)
          print(bar_plot())
          dev.off()
        })
      })
    }
  )

  # Download handlers for trended control chart
  output$download_trended_png <- downloadHandler(
    filename = function() paste0("trended_chart_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = trended_plot(), device = "png", width = 12, height = 8, dpi = 300, bg = "white")
    }
  )

  output$download_trended_svg <- downloadHandler(
    filename = function() paste0("trended_chart_", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(file, plot = trended_plot(), device = "svg", width = 12, height = 8, bg = "white")
    }
  )

  output$download_trended_pdf <- downloadHandler(
    filename = function() paste0("trended_chart_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch({
        ggsave(file, plot = trended_plot(), device = cairo_pdf, width = 12, height = 8, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = trended_plot(), device = "pdf", width = 12, height = 8, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 12, height = 8)
          print(trended_plot())
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