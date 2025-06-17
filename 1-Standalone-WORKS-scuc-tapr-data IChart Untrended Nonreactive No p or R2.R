# I-CHART UNTRENDED NON-REACTIVE NO P AND NO R^2 VALUES



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


########  CONVERTING AN UPDATED VERSION OF THE CSV FILE IN WIDE FORMAT TO LONG FORMAT ##########

# # Load required packages
# library(tidyr)
# library(dplyr)   # for data manipulation, if needed
# library(readr)   # for reading and writing CSV files
#
# # Read the wide-format CSV file
# data_wide <- read_csv("data/SCUC Snapshots 1995 to 2023-WIDE.csv")
#
# # Convert from wide to long format.
# # In this example, we assume that the first column (e.g., an identifier or date)
# # should remain as-is, and all other columns will be gathered.
# # Convert from wide to long format
# data_long <- data_wide %>%
#   pivot_longer(
#     cols = -(1:6),
#     names_to = "year",
#     values_to = "value"
#   )
#
# # Write the long-format data to a new CSV file - RENAME TO PREVIOUS FILENAME AFTER TESTING
# write_csv(data_long, "SCUC Snapshots 1995 to 2023-LONG-NEW.csv")



# # Load the data by path
df1 <- vroom::vroom(file = here::here("data",
                  "staar_performance_elementary_2018_2024_cleaned_long.csv"
                  )
)


# Load data locally, if needed
# df1 <- vroom::vroom(file = here::here("SCUC Snapshots 1995 to 2023-LONG.csv"))


##### Control chart calculations

# # Filter the data frame for control limits calc
# df1_control_limits <- df1 %>%
#   dplyr::filter(grouping == "Asian",
#          section == "Standardized_Scores",
#          level_achieved == "Approaches_or_Above",
#          dplyr::between(year, 2010, 2016)
#          )



# Filter the data frame based on the conditions
df1_control_limits <- df1 %>%
   dplyr::filter(grade_level_name == "Grade 3",
         group == "district",
         staar_grouping == "Approaches or Above",
         between(year, 2018, 2024)
         ) # %>%
    # rename(value1 = value) %>%
    # dplyr::mutate(value = 100 - value1)


#####  CALCULATE UNTRENDED CL, SD AND CONTROL LIMITS BASED ON CONTROL LIMITS DATA FRAME #####

  emp_cl <<- mean(df1_control_limits$value, na.rm = TRUE)  # Calculate empirical centerline
  emp_sd <<- mean(abs(diff(df1_control_limits$value)), na.rm = TRUE) / 1.128  # Calculate empirical standard deviation
  emp_lcl <<- max(emp_cl - (3 * emp_sd), 0)  # If LCL is negative, set it to zero
  emp_ucl <<- emp_cl + (3 * emp_sd)   # Calculate empirical upper control limit


#####  END OF CALCULATING CL, LCL AND UCL BASED ON CONTROL LIMITS DATA FRAME df1_control_limits #####



#####  FILTER TO INCLUDED ONLY DISIRED ROWS TO INCLUDE IN UNTRENDED I-CHART  ##########

# Filter the data frame based on the desired control chart rows
# df1_modified <- df1 %>%
#   dplyr::filter(grouping == "Asian",
#          section == "Standardized_Scores",
#          level_achieved == "Approaches_or_Above",
#          dplyr::between(year, 2012, 2023)
#          )  # Efficient range filtering)


# Filter the data frame based on the conditions
df1_modified <- df1 %>%
   dplyr::filter(grade_level_name == "Grade 3",
         group == "district",
         subject == "Mathematics",
         staar_grouping == "Approaches or Above",
         between(year, 2018, 2024)
         ) # %>%
    # rename(value1 = value) %>%
    # dplyr::mutate(value = 100 - value1)


#####  Add logical column to 'df1_modified' based on Shewhart Rule #1 ONLY.

# Shewhart Rule #1: violations if mr value is outside UCL or LCL
  sigma_signals <- df1_modified$value < emp_lcl | df1_modified$value > emp_ucl

# Rename the rule #1 violations and run rule violations
  emp_pts_out_of_control <- sigma_signals

# Add the 'out_of_control' column to the dataframe
  df1_modified$emp_pts_out_of_control <- emp_pts_out_of_control


# Runs analysis, for changing CL color and linetype
runs           <- sign(df1_modified$value - emp_cl)
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


# HARD CODE CHAR VECTORS OF UNIQUE VALUES IN VARIOUS ROWS IN DATA FRAME

# Create char vector of unique entries in 'year' column
desired_years = unique(df1_modified$year)

# Create char vector of unique entries in 'group' column
desired_groups = unique(df1_modified$group)

# Create char vector of unique entries in 'grade_level' column
desired_grade_levels <- unique(df1_modified$grade_level_name)

# Create char vector of unique entries in 'subject' column
desired_subjects <- unique(df1_modified$subject)

# Create char vector of unique entries in 'achieve_level' column
desired_achieve_levels <- unique(df1_modified$staar_grouping)

# Create num vector of unique entries in 'value' column
desired_values <- unique(df1_modified$value)

# Specify chart type as a char vector for creating filenames
chart_type = "Untrended I-Chart"

# Specify the caption for the plots
caption <- c("\n Source:  https://tea.texas.gov/reports-and-data")

# Replace zeros in the value column with NA
df1_modified$value <- ifelse(df1_modified$value == 0,
                         NA,
                         df1_modified$value
                         )


#####  'df1_modified' data frame now has 7 columns  #####

# Compute x and y positions for geom_richtext before the ggplot call
x_pos <- min(df1_modified$year, na.rm = TRUE)  # Ensure NA handling
y_pos <- max(df1_modified$value, na.rm = TRUE)  # Ensure NA handling




################    Function to generate    #####################
################    AN UNTRENDED I-Chart    #####################


# PLOT OBJECT 'combined_plots' CONTAINING ALL CHARTS IN ONE MAP FUNCTION -------------------------------------

#####  CREATE A TRENDED I-CHART  IN GGPLOT2  #####

# Create dynamic title and subtitle
  i_title <- paste0(
    "Individuals Chart: % of Students in ",
    desired_subjects,
    "<br>",
    "who achieved ",
    desired_achieve_levels
    # desired_sections
  )

  i_subtitle <- paste0(
    "In ",
    desired_grade_levels,
    "<br>",
    "For the Years Ended ",
    paste0(min(desired_years),
          " - ",
          max(desired_years))
  )

# Create plot object I-chart USING GGPLOT2 ONLY
  i_chart <- ggplot2::ggplot(df1_modified,
                      aes(x = df1_modified$year,
                          y = df1_modified$value
                          )
                      ) +

## Add lines connecting the points
   ggplot2::geom_line(color = "darkgray", linewidth = 1.2) +

# Use 'color' to map logical TRUE/FALSE to red/black points
  ggplot2::geom_point(aes(x = year,
                         y = value,
                         color = factor(emp_pts_out_of_control)  # Map to emp_pts_out_of_control to color the points
                        ),
                      size = 2.5) +

  ggplot2::scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +    # Define colors for dots based on value of sigma_signals

  # CL line type depends on value of runs_signals
  geom_line(aes(y = emp_cl,
                linetype = factor(runs_signal)
                )
            ) +

  scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "dashed")) +

  ggplot2::geom_line(aes(y = df1_modified$emp_ucl),
                     color = "red",
                     linetype = "solid",
                     linewidth = 1
                     ) +

  ggplot2::geom_line(aes(y = df1_modified$emp_lcl),
                     color = "red",
                     linetype = "solid",
                     linewidth = 1
                     ) +

# Add labels
  ggplot2::labs(title = i_title,
       subtitle = i_subtitle,
       caption = caption,
       x = "Academic Year",
       y = "Value"
       ) +


# Disclose CL calculation
ggtext::geom_richtext(aes(
  label = paste0("<b><i>UPL, CL, LPL are based on the years:  ",
  min(df1_control_limits$year),
  " - ",
  max(df1_control_limits$year),
  "</i></b>"),
  x = x_pos,
  y = y_pos
  ),
  size = 6,  # Font size (adjusted to a reasonable level)
  color = "black",
  hjust = -.40,  # Align near center
  vjust = -5,  # Align above the plot
  fill = "lightblue",
  label.color = "black"
  ) +


# Disclose CL linetype
ggtext::geom_richtext(aes(
  label = paste0("<b><i>If run rules violated, centerline will be dashed line",
                 "</i></b>"),
  x = max(df1_modified$year, na.rm = TRUE),  # Ensure NA handling
  y = min(df1_modified$value, na.rm = TRUE)  # Ensure NA handling
  ),
  size = 6,  # Font size (adjusted to a reasonable level)
  color = "black",
  hjust = 1,  # Align near center
  vjust = 3.0,  # Align above the plot
  fill = "pink",
  label.color = "black"
  ) +


# Add padding to both x and y axes in I-chart
  ggplot2::scale_x_continuous(breaks = unique(df1_modified$year),
                     expand = c(0.15, 0.00),
                     ) +


  ggplot2::scale_y_continuous(
    labels = function(y)
      format(y,
             scientific = FALSE,
             big.mark = ","
             ),
    name = unique(df1_modified$units
                  ),
    expand = c(0.25, 0.25
               )  # Add padding on both sides of y-axis on i_chart
                    ) +

# set base size for all future fonts in INTERNAL object i_chart
 ggplot2::theme_minimal(base_size = 26)


# Theme and title the plot object i_chart
i_chart <- i_chart +


ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                 hjust = 1
                                 )
      ) +

ggplot2::theme(legend.position = "none") +

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
        size = ggplot2::rel(.95),           # Set the font size of the caption
        hjust = 0,         # Center align the caption horizontally
        vjust = 2,           # Set vertical position of the caption
        face = "italic",     # Optional: Set font style (e.g., italic)
        color = "darkblue"   # Optional: Set color of the caption text
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


# Add labels for the first, middle, and last values of UCL, LCL, and centerline

i_chart <- i_chart +

  ggplot2::geom_text(aes(x = dplyr::first(year),                         # dplyr function to set the x position of text
                y = dplyr::first(emp_cl),                   # dplyr function to set the y position of the text
                label = "CenterLine",  # format(round(dplyr::first(emp_cl), 1   # dplyr function to round first value in 'centerline' column
                ),
            color = "blue",
            vjust = -1,
            hjust = 1.1,
            size = 5
            ) +


##### To add a measurement label at the mid-point of the center line

  # ggplot2::geom_text(aes(x = stats::median(year),                           # stats function to set the x position of text
  #               y = stats::median(emp_cl),                     # stats function to set the y position of text
  #               label = format(round(stats::median(emp_cl), 1),   # stats function to round median value in 'centerline' column
  #                              big.mark = ","
  #                              )
  #               ),
  #           color = "blue",
  #           vjust = 1,
  #           size = 5
  #           ) +


  ggplot2::geom_text(aes(x = dplyr::last(year),                         # dplyr function to set the x position of text
                y = dplyr::last(emp_cl),                   # dplyr function to set the y position of the text
                label = format(round(dplyr::last(emp_cl), 1   # dplyr function to round last value in 'centerline' column
                                     ),
                               )
                ),
            color = "blue",
            vjust = 0,
            hjust = -0.1,
            size = 5
            ) +

  ggplot2::geom_text(aes(x = dplyr::first(year) + 0.5,                  # dplyr function to set the x position of text
                y = dplyr::first(emp_ucl),                   # dplyr function to set the y position of the text
                label = "Upper Process Limit",  #format(round(dplyr::first(emp_ucl), 1   # dplyr function to round first value in 'ucl' column
                ),
            color = "red",
            vjust = -1,
            hjust = 1.1,
            size = 5
            ) +

  # ggplot2::geom_text(aes(x = stats::median(year),                   # stats function to set the x position of text
  #               y = stats::median(emp_ucl),                    # stats function to set the y position of text
  #               label = format(round(stats::median(emp_ucl), 1    # stats function to round median value in 'ucl' column
  #                                    ),
  #                              big.mark = ","
  #                              )
  #               ),
  #           color = "red",
  #           vjust = -1,
  #           size = 5
  #           ) +


  ggplot2::geom_text(aes(x = dplyr::last(year),                   # dplyr function to set the x position of text
                y = dplyr::last(emp_ucl),                    # dplyr function to set the y position of text
                label = format(round(dplyr::last(emp_ucl), 1    # dplyr function to round last value in 'ucl' column
                                     ),
                               )
                ),
            color = "red",
            vjust = -1,
            hjust = -0.1,
            size = 5
            ) +

  ggplot2::geom_text(aes(x = dplyr::first(year) + .5,                    # dplyr function to set the x position of text
                y = dplyr::first(emp_lcl),                     # dplyr function to set the y position of text
                label = "Lower Process Limit"   # format(round(dplyr::first(emp_lcl), 1     # dplyr function to round first value in 'ucl' column
                ),
            color = "red",
            vjust = 1.75,
            hjust = 1.1,
            size = 5
            ) +

  # ggplot2::geom_text(aes(x = stats::median(year),                   # stats function to set the x position of text
  #               y = stats::median(emp_lcl),                    # stats function to set the y position of text
  #               label = format(round(stats::median(emp_lcl), 1    # stats function to round median value in 'lcl' column
  #                                    ),
  #                              big.mark = ","
  #                              )
  #               ),
  #           color = "red",
  #           vjust = 1.5,
  #           size = 5
  #           ) +
  #
  ggplot2::geom_text(aes(x = dplyr::last(year),                      # dplyr function to set the x position of text
                y = dplyr::last(emp_lcl),                       # dplyr function to set the y position of text
                label = format(round(dplyr::last(emp_lcl), 1       # dplyr function to round last value in 'ucl' column
                                     ),
                ),
            color = "red",
            vjust = 1.5,
            hjust = -0.1,
            size = 5
            )
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



#####################

# print(section_unique)
# [1] "District_Perform"    "Expenses"            "Fund_Balances"       "SAT_ACT"
# [5] "Staff"               "Standardized_Scores" "Students"            "Taxes_and_Revenues"
# [9] "Teachers"


#####################

#  print(grouping_unique)
#  [1] "Total Students in District"
#  [2] "Total Number of Schools"
#  [3] "Attendance Rate"
#  [4] "Completion Rate"
#  [5] "Annual Graduate Count"
#  [6] "Annual RHSP-DAP-FHSP-E-FHSP-DLA Graduate Count"
#  [7] "4-Year Longitudinal Graduation Rate"
#  [8] "5-Year Longitudinal Graduation Rate"
#  [9] "6-Year Longitudinal Graduation Rate"
# [10] "Annual Dropout Rate"
# [11] "Annual Dropout Rate Gr 9-12"
# [12] "Four-year Dropout Rate"
# [13] "Exit-Level Cumulative Pass Rate"
# [14] "Instructional"
# [15] "Central Administrative"
# [16] "Campus Administrative-School Leadership"
# [17] "Plant Services"
# [18] "Other Operating"
# [19] "Non-Operating"
# [20] "Total Expenditures"
# [21] "Total Instructional Expenditures"
# [22] "Total Instructional ExpendPer Pupil"
# [23] "Total Operating Expenditures"
# [24] "Total Operating Expend Per Pupil"
# [25] "Regular Education"
# [26] "Un-Allocated Expenditures"
# [27] "Special Education"
# [28] "Compensatory-Accelerated Education"
# [29] "Bilingual-ESL Education"
# [30] "Career and Technology Education"
# [31] "Gifted and Talented Education"
# [32] "Athletics-Related Activities"
# [33] "High School Allotment"
# [34] "Prekindergarten"
# [35] "Fund Balance-EOY"
# [36] "Fund Balance of Budget"
# [37] "Percent Tested"
# [38] "Percent At Or Above Criterion"
# [39] "SAT-Mean Total Score"
# [40] "ACT-Mean Composite Score"
# [41] "Number of Students Per Total Staff"
# [42] "Number of Students Per Teacher"
# [43] "Total Staff FTE"
# [44] "Total Teacher FTE"
# [45] "Average Central Administr Salary"
# [46] "Average School Administr Salary"
# [47] "Average Profess Support Staff Salary"
# [48] "Average Teacher Salary"
# [49] "Minority"
# [50] "School Administrative"
# [51] "Professional Support Staff"
# [52] "Teachers"
# [53] "Educational Aides"
# [54] "Auxiliary Staff"
# [55] "White"
# [56] "Hispanic"
# [57] "African American"
# [58] "American Indian"
# [59] "Asian"
# [60] "Pacific Islander"
# [61] "2 or More Races"
# [62] "Economically Disadvantaged"
# [63] "SDAA Met ARD"
# [64] "All Subjects"
# [65] "Reading-ELA"
# [66] "Writing"
# [67] "Mathematics"
# [68] "Science"
# [69] "Social Studies"
# [70] "English Language Learners (ELL)"
# [71] "Career and Technology Ed"
# [72] "Gifted and Talented Ed"
# [73] "State"
# [74] "Local and Other"
# [75] "Federal"
# [76] "Taxable Value Per Pupil"
# [77] "Locally Adopted Tax Rate"
# [78] "Total Revenue"
# [79] "Total Revenue Per Pupil"
# [80] "Total Operating Revenue"
# [81] "Total Other Revenue"
# [82] "Two or More Races"
# [83] "With 5 or Fewer Years Experience"
# [84] "Average Years of Experience"
# [85] "With Advanced Degrees"
# [86] "Teacher Turnover Rate"
# [87] "Compensatory Education"
