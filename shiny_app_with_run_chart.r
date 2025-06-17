# Ultimate EDA Shiny App - Foundation with Date Range Slider
# This is the starting framework focusing on file upload, filtering, and DT table

library(shiny)
library(DT)
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(ggtext)
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
          # Show date range info
          fluidRow(
            column(12,
              uiOutput("date_info")
            )
          ),
          br(),
          # Only essential text inputs - no formatting controls
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
          # Download buttons
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
  
  # Display date range information
  output$date_info <- renderUI({
    data <- raw_data()
    if (is.null(data) || !"date" %in% names(data)) return(NULL)
    
    date_range <- range(data$date, na.rm = TRUE)
    date_class <- class(data$date)[1]
    
    # Check if all dates are January 1st (yearly data)
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
  
  # Display grouping variable information for line chart
  output$grouping_info <- renderUI({
    data <- raw_data()
    if (is.null(data)) return(NULL)
    
    # Find potential grouping variables (not date or value)
    potential_groups <- names(data)[!names(data) %in% c("date", "value")]
    
    if (length(potential_groups) == 0) {
      div(
        style = "background-color: #fff3cd; padding: 10px; border-radius: 5px; border: 1px solid #ffeaa7;",
        HTML("<strong>Note:</strong> No grouping variable detected. Line chart needs a third column (category, series, group, etc.) to create multiple lines.")
      )
    } else {
      # Use first non-date/value column as grouping variable
      group_var <- potential_groups[1]
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
  
  # Create reactive plot for line chart using fixed formatting standards
  line_plot <- reactive({
    data <- filtered_data()
    req(data)
    
    # Find grouping variable
    potential_groups <- names(data)[!names(data) %in% c("date", "value")]
    if (length(potential_groups) == 0) return(NULL)
    
    group_var <- potential_groups[1]
    
    # Get user text inputs (with safe defaults)
    title_text <- if(is.null(input$line_title) || input$line_title == "") "Line Chart" else input$line_title
    subtitle_text <- if(is.null(input$line_subtitle)) "" else input$line_subtitle
    caption_text <- if(is.null(input$line_caption)) "" else input$line_caption
    
    # Smart date formatting: check if all dates are January 1st (yearly data)
    all_jan_first <- all(format(data$date, "%m-%d") == "01-01", na.rm = TRUE)
    
    # Set date format and breaks based on data pattern
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
    
    # Professional color palette for multiple lines
    colors <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#8E5572", "#007F5F", "#7209B7", "#AA6C39")
    
    # Create line chart with FIXED formatting standards
    ggplot(data, aes(x = date, y = value, color = !!sym(group_var))) +
      
      # Lines: multiple colors, linewidth 1.2
      geom_line(linewidth = 1.2) +
      
      # Professional color palette
      scale_color_manual(values = colors) +
      
      # End-of-line labels (instead of legend)
      geom_text(data = label_data,
                aes(x = date, y = value, label = !!sym(group_var), color = !!sym(group_var)),
                hjust = -0.1,  # Position labels to the right of line endpoints
                vjust = 0.5,
                size = 8,
                fontface = "bold") +
      
      # Y-axis formatting: commas, no scientific notation, 10% expansion
      scale_y_continuous(
        labels = function(y) format(y, scientific = FALSE, big.mark = ","),
        expand = expansion(mult = c(0.10, 0.10))  # 10% space below and above
      ) +
      
      # Smart X-axis formatting: adaptive based on date pattern
      scale_x_date(
        date_labels = date_format, 
        date_breaks = date_breaks_interval,
        expand = expansion(mult = c(0.05, 0.15))  # Extra space on right for labels
      ) +
      
      # Labels
      labs(title = title_text,
           subtitle = subtitle_text,
           caption = caption_text,
           x = "Date",
           y = "Value") +
      
      # Base theme with size 26
      theme_minimal(base_size = 26) +
      
      # Fixed theme formatting based on preferences
      theme(
        # Remove legend - using end-of-line labels instead
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1.00),
        axis.title.x = element_text(margin = margin(t = 20)),
        
        # General text color
        text = element_text(color = "royalblue"),
        
        # Title formatting - darkgreen, size 33, bold, element_markdown
        plot.title.position = "panel",
        plot.title = element_markdown(
          color = "darkgreen",
          size = 33,
          face = "bold",
          lineheight = 1.1,
          margin = margin(2, 0, 0, 0, "lines")
        ),
        
        # Subtitle formatting - darkgreen, size 28, bold, element_markdown  
        plot.subtitle = element_markdown(
          color = "darkgreen",
          size = 28,
          face = "bold",
          lineheight = 1.0,
          margin = margin(0, 0, 0, 0, "lines")
        ),
        
        # Caption formatting - size 25, hjust 0, vjust 2, italic, darkblue
        plot.caption = element_text(
          size = 25,
          hjust = 0,
          vjust = 2,
          face = "italic",
          color = "darkblue"
        ),
        
        # Axis text color
        axis.text = element_text(color = "black"),
        
        # Background colors
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        
        # Plot margins
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
      )
  })
  
  # Render line chart for display
  output$line_chart <- renderPlot({
    line_plot()
  })
  
  # Download handlers for line chart
  output$download_line_png <- downloadHandler(
    filename = function() {
      paste0("line_chart_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = line_plot(), device = "png", 
             width = 12, height = 8, dpi = 300, bg = "white")
    }
  )
  
  output$download_line_svg <- downloadHandler(
    filename = function() {
      paste0("line_chart_", Sys.Date(), ".svg")
    },
    content = function(file) {
      ggsave(file, plot = line_plot(), device = "svg", 
             width = 12, height = 8, bg = "white")
    }
  )
  
  output$download_line_pdf <- downloadHandler(
    filename = function() {
      paste0("line_chart_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      tryCatch({
        ggsave(file, plot = line_plot(), device = cairo_pdf, 
               width = 12, height = 8, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = line_plot(), device = "pdf", 
                 width = 12, height = 8, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 12, height = 8)
          print(line_plot())
          dev.off()
        })
      })
    }
  )
  output$date_info <- renderUI({
    data <- raw_data()
    if (is.null(data) || !"date" %in% names(data)) return(NULL)
    
    date_range <- range(data$date, na.rm = TRUE)
    date_class <- class(data$date)[1]
    
    # Check if all dates are January 1st (yearly data)
    all_jan_first <- all(format(data$date, "%m-%d") == "01-01", na.rm = TRUE)
    format_used <- if(all_jan_first) "Years only" else "Full dates (YYYY-MM-DD)"
    
    div(
      style = "background-color: #f0f8ff; padding: 10px; border-radius: 5px; border: 1px solid #4682b4;",
      HTML(paste0(
        "<strong>Date Range:</strong> ", 
        format(date_range[1], "%B %d, %Y"), " to ", format(date_range[2], "%B %d, %Y"),
        " <em>(", nrow(data), " rows, ", format_used, " on x-axis)</em>"
      ))
    )
  })
  
  # Create reactive plot for run chart using fixed formatting standards
  run_plot <- reactive({
    data <- filtered_data()
    req(data)
    
    # Get user text inputs (with safe defaults)
    title_text <- if(is.null(input$run_title) || input$run_title == "") "Run Chart" else input$run_title
    subtitle_text <- if(is.null(input$run_subtitle)) "" else input$run_subtitle
    caption_text <- if(is.null(input$run_caption)) "" else input$run_caption
    
    # Calculate median for reference line
    median_value <- median(data$value, na.rm = TRUE)
    
    # Smart date formatting: check if all dates are January 1st (yearly data)
    all_jan_first <- all(format(data$date, "%m-%d") == "01-01", na.rm = TRUE)
    
    # Set date format and breaks based on data pattern
    if (all_jan_first) {
      # Yearly data: show just years
      date_format <- "%Y"
      date_breaks_interval <- "1 year"
    } else {
      # Specific dates: show full date format
      date_format <- "%Y-%m-%d"
      # Adjust breaks based on data density
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
    
    # Create run chart with FIXED formatting standards
    ggplot(data, aes(x = date, y = value)) +
      
      # Lines: darkgray, linewidth 1.2
      geom_line(color = "darkgray", 
                linewidth = 1.2) +
      
      # Points: blue, size 2.5  
      geom_point(color = "blue",
                 size = 2.5) +
      
      # Median reference line (like centerline in control charts)
      geom_hline(yintercept = median_value,
                 color = "red",
                 linewidth = 1,
                 linetype = "solid",
                 alpha = 0.8) +
      
      # Median label above the line
      geom_text(aes(x = min(date), y = median_value, label = "Median"),
                color = "red",
                vjust = -0.5,
                hjust = 0,
                size = 8,
                fontface = "bold") +
      
      # Y-axis formatting: commas, no scientific notation, 10% expansion
      scale_y_continuous(
        labels = function(y) format(y, scientific = FALSE, big.mark = ","),
        expand = expansion(mult = c(0.10, 0.10))  # 10% space below and above
      ) +
      
      # Smart X-axis formatting: adaptive based on date pattern
      scale_x_date(
        date_labels = date_format, 
        date_breaks = date_breaks_interval,
        expand = expansion(add = 30)
      ) +
      
      # Labels
      labs(title = title_text,
           subtitle = subtitle_text,
           caption = caption_text,
           x = "Date",
           y = "Value") +
      
      # Base theme with size 26
      theme_minimal(base_size = 26) +
      
      # Fixed theme formatting based on preferences
      theme(
        # Remove legend and set x-axis to 45 degrees
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1.00),
        axis.title.x = element_text(margin = margin(t = 20)),  # Push x-axis title down
        
        # General text color
        text = element_text(color = "royalblue"),
        
        # Title formatting - darkgreen, size 33, bold, element_markdown
        plot.title.position = "panel",
        plot.title = element_markdown(
          color = "darkgreen",
          size = 33,
          face = "bold",
          lineheight = 1.1,
          margin = margin(2, 0, 0, 0, "lines")
        ),
        
        # Subtitle formatting - darkgreen, size 28, bold, element_markdown  
        plot.subtitle = element_markdown(
          color = "darkgreen",
          size = 28,
          face = "bold",
          lineheight = 1.0,
          margin = margin(0, 0, 0, 0, "lines")
        ),
        
        # Caption formatting - size 25, hjust 0, vjust 2, italic, darkblue
        plot.caption = element_text(
          size = 25,
          hjust = 0,
          vjust = 2,
          face = "italic",
          color = "darkblue"
        ),
        
        # Axis text color
        axis.text = element_text(color = "black"),
        
        # Background colors
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        
        # Plot margins
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
      )
  })
  
  # Render run chart for display
  output$run_chart <- renderPlot({
    run_plot()
  })
  
  # Download handlers for run chart
  output$download_run_png <- downloadHandler(
    filename = function() {
      paste0("run_chart_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = run_plot(), device = "png", 
             width = 12, height = 8, dpi = 300, bg = "white")
    }
  )
  
  output$download_run_svg <- downloadHandler(
    filename = function() {
      paste0("run_chart_", Sys.Date(), ".svg")
    },
    content = function(file) {
      ggsave(file, plot = run_plot(), device = "svg", 
             width = 12, height = 8, bg = "white")
    }
  )
  
  output$download_run_pdf <- downloadHandler(
    filename = function() {
      paste0("run_chart_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Try-catch for PDF generation with better error handling
      tryCatch({
        # Use cairo_pdf for better font handling
        ggsave(file, plot = run_plot(), device = cairo_pdf, 
               width = 12, height = 8, bg = "white")
      }, error = function(e) {
        # Fallback to regular pdf device
        tryCatch({
          ggsave(file, plot = run_plot(), device = "pdf", 
                 width = 12, height = 8, bg = "white")
        }, error = function(e2) {
          # If all else fails, create a simple PDF with base R
          pdf(file, width = 12, height = 8)
          print(run_plot())
          dev.off()
        })
      })
    }
  )
  
  # Download handlers for line chart
  output$download_line_png <- downloadHandler(
    filename = function() {
      paste0("line_chart_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = line_plot(), device = "png", 
             width = 12, height = 8, dpi = 300, bg = "white")
    }
  )
  
  output$download_line_svg <- downloadHandler(
    filename = function() {
      paste0("line_chart_", Sys.Date(), ".svg")
    },
    content = function(file) {
      ggsave(file, plot = line_plot(), device = "svg", 
             width = 12, height = 8, bg = "white")
    }
  )
  
  output$download_line_pdf <- downloadHandler(
    filename = function() {
      paste0("line_chart_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      tryCatch({
        ggsave(file, plot = line_plot(), device = cairo_pdf, 
               width = 12, height = 8, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = line_plot(), device = "pdf", 
                 width = 12, height = 8, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 12, height = 8)
          print(line_plot())
          dev.off()
        })
      })
    }
  )
  
  # Download handlers for bar chart
  output$download_bar_png <- downloadHandler(
    filename = function() {
      paste0("bar_chart_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = bar_plot(), device = "png", 
             width = 12, height = 8, dpi = 300, bg = "white")
    }
  )
  
  output$download_bar_svg <- downloadHandler(
    filename = function() {
      paste0("bar_chart_", Sys.Date(), ".svg")
    },
    content = function(file) {
      ggsave(file, plot = bar_plot(), device = "svg", 
             width = 12, height = 8, bg = "white")
    }
  )
  
  output$download_bar_pdf <- downloadHandler(
    filename = function() {
      paste0("bar_chart_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      tryCatch({
        ggsave(file, plot = bar_plot(), device = cairo_pdf, 
               width = 12, height = 8, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = bar_plot(), device = "pdf", 
                 width = 12, height = 8, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 12, height = 8)
          print(bar_plot())
          dev.off()
        })
      })
    }
  )
  
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