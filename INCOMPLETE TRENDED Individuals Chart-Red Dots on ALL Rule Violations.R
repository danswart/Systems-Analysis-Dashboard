
# TRENDED CONTROL CHART WITH RULE #1 AND ANHOEJ RUN-RULES ---------------------------------------------------

# Load libraries
library(tidyverse)
library(plotly)
library(DT)


# Create a toy dataset
set.seed(123)
n <- 30
x <- 1:n
y <- 10 + 0.5 * x + rnorm(n, mean = 0, sd = 1)
data <- data.frame(x = x, y = y)


# Introduce a perturbation to shift the mean higher after point 15
# data$y[data$x > 15] <- data$y[data$x > 15] + 3  # Add 3 to the y values after point 15


# Fit a linear model and create a summary object
model <- lm(y ~ x, data = data)

model_summary <- summary(model)

# Calculate a standard deviation that reflects the trend
residuals_sd <- sd(residuals(model))

# Calculate control limits (3 sigma, NOT 3 std dev)
data$centerline <- predict(model, newdata = data)
data$ucl <- data$centerline + 3 * (residuals_sd/1.128)
data$lcl <- data$centerline - 3 * (residuals_sd/1.128)

# Set Shewhart Rule #1 boundaries
sigma.signals <- y < data$lcl | y > data$ucl   # this is a logical vector

# Add check for unusually long runs or unusually few crossings.

# add runs analysis
runs        <- sign(y - data$centerline)
runs        <- runs[runs != 0]
runs        <- rle(runs)$lengths
n.obs       <- sum(runs)
longest.run <- max(runs,
                   na.rm = TRUE
                   )
n.runs      <- length(runs)
n.crossings <- n.runs - 1

longest.run.max <- round(log2(n.obs) + 3)
n.crossings.min <- qbinom(.05, n.obs - 1, 0.5)

runs.signal <- longest.run > longest.run.max | n.crossings < n.crossings.min   # this is a logical vector


# Initialize a Plot for a trended control chart
ggplot(data,
       aes(x = x,
           y = y
           )
       ) +


# Connect dots with lines
  geom_line() +


# Draw CL, UCL & LCL
  geom_line(aes(y = centerline
                ),
            color = 'royalblue',
            linetype = runs.signal +1,
            linewidth = 2
            ) +

  geom_line(aes(y = ucl),
            color = "red",
            linetype = "solid",
            linewidth = 2
            ) +

  geom_line(aes(y = lcl
                ),
            color = "red",
            linetype = "solid",
            linewidth = 2
            ) +

  # Convert a logical vector to a numeric value that sets the color of the point to red

# Plot with conditional color based on sigma.signals
  # geom_point(aes(col = factor(sigma.signals)),  # Convert logical to factor (TRUE = 1, FALSE = 0)
  #            shape = 19,
  #            size = 5) +
  # scale_color_manual(values = c("black", "red")) +  # Map FALSE to black and TRUE to red


  geom_point(aes(x = x,
                 y = y
                 ),
             shape = 19,
             size = 5,
             col = sigma.signals + 1
             ) +

# Add labels
  labs(title = "Trended I-Chart",
       subtitle = "with Shewhart Rule #1 and Anhoej Run Rules",
       caption = "Data is synthetic",
       x = "Observation",
       y = "Value"
       ) +

# Theme it
  theme_minimal(base_size = 26,
                base_family = 'Merriweather',
                base_line_size = 3
                )


