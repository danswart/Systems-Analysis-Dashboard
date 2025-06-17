
# Load Libraries
library(tidyverse)
library(qicharts2)
library(haven)
library(readxl)
library(skimr)
library(visdat)
library(gghighlight)
library(dplyr)
library(stringr)
library(ggtext)
library(gganimate)
library(ggplot2)
library(purrr)
library(scales)
library(ggrepel)
library(glue)             # concatenating strings
library(plotly)
library(htmlwidgets)
library(readr)
library(camcorder)
library(RColorBrewer)
library(rlang)
library(cowplot)
library(shiny)
library(flexdashboard)
library(DT)
library(htmltools)
library(shinyobjects)     # debugging shiny code
library(prettycode)
library(vroom)            # fast loading of data frames
library(tidylog)          # reports changes in console
library(janitor)
library(lubridate)
library(tsibble)          # working with time series data
library(tidytext)         # text analysis for chr columns
library(dlookr)           # diagnose and repair data frames; can compare distribs to normal
library(flextable)
library(patchwork)        # display multiple ggplot2 objects with various configs
library(grid)


# Options
options(scipen = 999)
options(qic.clshade = T)            # NO LONGER NEEDED; CHARTS ALL PREPARED WITH GGPLOT2 ONLY
options(qic.linecol = 'black')      # NO LONGER NEEDED; CHARTS ALL PREPARED WITH GGPLOT2 ONLY
options(qic.signalcol = "red")      # NO LONGER NEEDED; CHARTS ALL PREPARED WITH GGPLOT2 ONLY
options(qic.targetcol = "purple")   # NO LONGER NEEDED; CHARTS ALL PREPARED WITH GGPLOT2 ONLY
options(DT.options = list(dom = 'pBlfrti'))   # Add buttons, filtering, and top (t) pagination controls
options(shiny.maxRequestSize = 50 * 1024^2)   # Set upload maximum to 50 MB



# Load the data by path
df1 <- vroom::vroom(file = here::here("data",
                                      "2024-25_weekly_attendance-LONG.csv"
                                      )
)



# Filter the data frame based on the conditions
df1_filtered <- df1 %>%
  dplyr::filter(campus == "district_total")


#####  CALCULATE UNTRENDED CL, SD AND CONTROL LIMITS BASED ON CONTROL LIMITS DATA FRAME #####
df1_control_limits <- df1_filtered

emp_cl <<- mean(df1_control_limits$pct, na.rm = TRUE)  # Calculate empirical centerline
emp_sd <<- mean(abs(diff(df1_control_limits$pct)), na.rm = TRUE) / 1.128  # Calculate empirical standard deviation
emp_lcl <<- max(emp_cl - (3 * emp_sd), 0)  # If LCL is negative, set it to zero
emp_ucl <<- emp_cl + (3 * emp_sd)   # Calculate empirical upper control limit


#####  END OF CALCULATING CL, LCL AND UCL BASED ON CONTROL LIMITS DATA FRAME df1_control_limits #####



df1_modified <- df1_filtered     # agree name to agree with further code


#####  Add logical column to 'df1_modified' based on Shewhart Rule #1 ONLY.

# Shewhart Rule #1: violations if mr pct is outside UCL or LCL
sigma_signals <- df1_modified$pct < emp_lcl | df1_modified$pct > emp_ucl

# Rename the rule #1 violations and run rule violations
emp_pts_out_of_control <- sigma_signals

# Add the 'out_of_control' column to the dataframe
df1_modified$emp_pts_out_of_control <- emp_pts_out_of_control


# Runs analysis, for changing CL color and linetype
runs           <- sign(df1_modified$pct - emp_cl)
runs           <- runs[runs != 0]
runs_lengths   <- rle(runs)$lengths
n_obs          <- sum(runs_lengths)
longest_run    <- max(runs_lengths)
n_runs         <- length(runs_lengths)
n_crossings    <- n_runs - 1
longest_run_max <- round(log2(n_obs) + 3)
n_crossings_min <- qbinom(.05, n_obs - 1, 0.5)

runs_signal <- longest_run > longest_run_max | n_crossings < n_crossings_min


#####  ADD EMPERICAL CL, SD, UCL, LCL AND RUNS_SIGNAL TO df1_modified DATA FRAME  #####

df1_modified <- df1_modified %>%
  dplyr::mutate(
    emp_cl = emp_cl,            # add empirical centerline
    emp_sd = emp_sd,            # add empirical standard deviation
    emp_lcl = emp_lcl,          # add empirical lcl
    emp_ucl = emp_ucl,         # add empirical upper control limit
    runs_signal = as.logical(runs_signal)  # Ensure it's a column, not a constant # add runs_signal
  )

#####  END OF FILTERING DATA FRAME  ##########



# HARD CODE CHAR VECTORS OF UNIQUE pctS IN VARIOUS ROWS IN DATA FRAME

# Create char vector of unique entries in 'week_ended' column
desired_week_ended = unique(df1_modified$week_ended)

# Create num vector of unique entries in 'pct' column
desired_pcts <- unique(df1_modified$pct)

# Specify chart type as a char vector for creating filenames
chart_type = "Untrended I-Chart"

# Specify the caption for the plots
caption <- c("\n Source:  https://tea.texas.gov/reports-and-data")

# Replace zeros in the pct column with NA
df1_modified$pct <- ifelse(df1_modified$pct == 0,
                             NA,
                             df1_modified$pct
)



# Create a separate data frame with ALL the columns you need
annotation_data_limits <- data.frame(
  x_pos = as.Date(min(df1_modified$week_ended, na.rm = TRUE)),  # Convert to Date
  y_pos = max(df1_modified$pct, na.rm = TRUE) * 1.01,  # Position slightly above max
  label = paste0("<b><i>UPL, CL, LPL are based on the week_ended: ",
                 min(df1_control_limits$week_ended),
                 " - ",
                 max(df1_control_limits$week_ended),
                 "</i></b>")
)


annotation_data_cl_type <- data.frame(
  x_pos = mean(range(df1_modified$week_ended, na.rm = TRUE)),  # center of the x axis
  y_pos = max(df1_modified$pct, na.rm = TRUE) * 1.00,       # Ensure NA handling
  label = paste0("<b><i>If run rules violated, centerline will be dashed line",
               "</i></b>")
  )






#####  CREATE AN UNTRENDED I-CHART  IN GGPLOT2  #####

# Create dynamic title and subtitle
i_title <- paste0(
  "Expectation Chart: Avg Daily Attendance - District Totals")


i_subtitle <- paste0(
  "For the Weeks Ended ",
  paste0(min(desired_week_ended),
         " - ",
         max(desired_week_ended))
)


  # Create plot object I-chart USING GGPLOT2 ONLY
  i_chart <- ggplot2::ggplot(df1_modified,
                             aes(x = week_ended,
                                 y = pct
                             )
  ) +

  ## Add lines connecting the points
  ggplot2::geom_line(color = "darkgray", linewidth = 1.2) +


  ggplot2::geom_point(aes(x = week_ended,
                          y = pct,
                          color = factor(emp_pts_out_of_control)
                          ),
                      size = 2.5) +

  ggplot2::theme_minimal(base_size = 26) +

 # remove legend and set x-axis to 45 degrees and push title downward
 ggplot2::theme(
   legend.position = "none",
   axis.text.x = element_text(angle = 45,
                              hjust = 1.00),      # Keep this for your angled labels
   axis.title.x = element_text(margin = margin(t = 20))  # This moves the TITLE down
 ) +


  ggplot2::scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +    # Define colors for dots based on pct of sigma_signals

  # CL line type depends on runs_signals
  geom_line(aes(y = emp_cl,
                linetype = factor(runs_signal)
                )
            ) +

  scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "dashed")) +

  ggplot2::geom_line(aes(y = emp_ucl),
                     color = "red",
                     linetype = "solid",
                     linewidth = 1
  ) +

  ggplot2::geom_line(aes(y = emp_lcl),
                     color = "red",
                     linetype = "solid",
                     linewidth = 1
  ) +

  # Add labels
  ggplot2::labs(title = i_title,
                subtitle = i_subtitle,
                caption = caption,
                x = "Academic Week Ended",
                y = "Percent"
  ) +



  # Disclose CL calculation
  ggtext::geom_richtext(
    data = annotation_data_limits,  # Use the annotation data frame
    aes(x = x_pos, y = y_pos, label = label),  # Map to columns in annotation_data
    size = 8,
    color = "black",
    hjust = 0,
    vjust = 0,
    fill = "lightblue",
    label.color = "black"
  ) +


  # Disclose CL linetype
  ggtext::geom_richtext(
    data = annotation_data_cl_type ,  # Use the annotation data frame
    aes(x = x_pos, y = y_pos, label = label),  # Map to columns in annotation_data
    size = 8,
    color = "black",
    hjust = 0,
    vjust = 0,
    fill = "lightyellow",
    label.color = "black"
  ) +


  # Push x-axis downward and add space between y-axis and chart.  Makes room for labels
  ggplot2::scale_x_continuous(breaks = unique(df1_modified$week_ended),
                              expand = expansion(add = 30)   # Add 2 units of space on the left and right of the data
  ) +


  # Add padding on both sides of y-axis
  ggplot2::scale_y_continuous(
    labels = function(y)
      format(y,
             scientific = FALSE,
             big.mark = ","
      ),
    expand = expansion(mult = c(0.10, 0.10))  # 10% space below lowest value and 10% above highest
  )





# Theme and title the plot object i_chart
i_chart <- i_chart +

  ggplot2::theme(plot.title.position = "panel",
                 text = ggplot2::element_text(color = "royalblue"
                 ),
                 plot.title = ggtext::element_markdown(
                   color = "darkgreen",
                   size = ggplot2::rel(1.25),
                   face = "bold",
                   lineheight = 1.1,
                   margin = ggplot2::margin(2, 0, 0, 0, "lines"
                   )
                 ),

                 plot.subtitle = ggtext::element_markdown(
                   color = "darkgreen",
                   size = ggplot2::rel(1.1),
                   face = "bold",
                   lineheight = 1.0,
                   margin = ggplot2::margin(0, 0, 0, 0, "lines"
                   )
                 ),

                 # Customize i_chart caption's font size and position
                 plot.caption = ggplot2::element_text(
                   size = ggplot2::rel(.95),      # Set the font size of the caption
                   hjust = 0,                     # Center align the caption horizontally
                   vjust = 2,                     # Set vertical position of the caption
                   face = "italic",               # Set font style (e.g., italic)
                   color = "darkblue"             # Set color of the caption text
                 ),

                 strip.text = ggplot2::element_text(color = "orange",
                                                    size = ggplot2::rel(1.1),
                                                    face = "italic",
                                                    margin = ggplot2::margin(2, 0, 0.5, 0, "lines"
                                                    )
                 ),

                 axis.text = ggplot2::element_text(color = "black"),

                 panel.background = ggplot2::element_rect(fill = "white", color = NA),

                 plot.background = ggplot2::element_rect(fill = "white", color = NA)
  )



# Add labels for the first, middle, and last pcts of UCL, LCL, and centerline

i_chart <- i_chart +

  ggplot2::geom_text(aes(x = dplyr::first(week_ended),     # dplyr function to set the x position of text
                         y = dplyr::first(emp_cl),         # dplyr function to set the y position of the text
                         label = "Avg Expectation",
  ),
  color = "blue",
  vjust = -1,
  hjust = 1.1,
  size = 8
  ) +


  ggplot2::geom_text(aes(x = dplyr::last(week_ended),                  # dplyr function to set the x position of text
                         y = dplyr::last(emp_cl),                      # dplyr function to set the y position of the text
                         label = format(round(dplyr::last(emp_cl), 2   # dplyr function to round last pct in 'centerline' column
                         ),
                         )
  ),
  color = "blue",
  vjust = 0,
  hjust = -0.1,
  size = 8
  ) +

  ggplot2::geom_text(aes(x = dplyr::first(week_ended) + 0.5,   # dplyr function to set the x position of text
                         y = dplyr::first(emp_ucl),            # dplyr function to set the y position of the text
                         label = "Upper Expectation",
  ),
  color = "red",
  vjust = -1,
  hjust = 1.1,
  size = 8
  ) +


  ggplot2::geom_text(aes(x = dplyr::last(week_ended),                   # dplyr function to set the x position of text
                         y = dplyr::last(emp_ucl),                      # dplyr function to set the y position of text
                         label = format(round(dplyr::last(emp_ucl), 2   # dplyr function to round last pct in 'ucl' column
                         ),
                         )
                         ),
                     color = "red",
                     vjust = -1,
                     hjust = -0.1,
                     size = 8
  ) +

  ggplot2::geom_text(aes(x = dplyr::first(week_ended) + .5,      # dplyr function to set the x position of text
                         y = dplyr::first(emp_lcl),              # dplyr function to set the y position of text
                         label = "Lower Expectation"
                         ),
  color = "red",
  vjust = 1.75,
  hjust = 1.1,
  size = 8
  ) +

  ggplot2::geom_text(aes(x = dplyr::last(week_ended),                    # dplyr function to set the x position of text
                         y = dplyr::last(emp_lcl),                       # dplyr function to set the y position of text
                         label = format(round(dplyr::last(emp_lcl), 2    # dplyr function to round last pct in 'ucl' column
                         ),
                         )
                         ),
                     color = "red",
                     vjust = 1.5,
                     hjust = -0.1,
                     size = 8
                     )


# Add padding to individual plots (I-chart)
i_chart <- i_chart +

  ggplot2::theme(plot.margin = ggplot2::margin(t = 0,
                                               r = 0,
                                               b = 0,
                                               l = 0
  )
  ) # Adjust padding (top, right, bottom, left)



# Display the chart
print(i_chart)







