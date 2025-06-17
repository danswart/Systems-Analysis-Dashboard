# Load libraries
library(tidyverse)

# Create a toy dataset
set.seed(123)
n <- 30
x <- 1:n
y <- 10 + 0.5 * x + rnorm(n, mean = 0, sd = 1)
data <- data.frame(x = x, y = y)

# Introduce a perturbation to shift the mean higher after point 15
data$y[data$x > 15] <- data$y[data$x > 15] + 5  # Add 15 to the y values after point 15

# Split the data into two segments
data_first_15 <- data[data$x <= 15, ]
data_remaining <- data[data$x > 15, ]

# Fit linear models for each segment
model_first_15 <- lm(y ~ x, data = data_first_15)
model_remaining <- lm(y ~ x, data = data_remaining)

# Calculate standard deviations for each segment
residuals_sd_first_15 <- sd(residuals(model_first_15))
residuals_sd_remaining <- sd(residuals(model_remaining))

# Calculate control limits for each segment
data$centerline_first_15 <- predict(model_first_15, newdata = data)
data$ucl_first_15 <- data$centerline_first_15 + 3 * (residuals_sd_first_15 / 1.128)
data$lcl_first_15 <- data$centerline_first_15 - 3 * (residuals_sd_first_15 / 1.128)

data$centerline_remaining <- predict(model_remaining, newdata = data)
data$ucl_remaining <- data$centerline_remaining + 3 * (residuals_sd_remaining / 1.128)
data$lcl_remaining <- data$centerline_remaining - 3 * (residuals_sd_remaining / 1.128)

# Set Shewhart Rule #1 boundaries
sigma.signals <- y < data$lcl_first_15 | y > data$ucl_first_15 | y < data$lcl_remaining | y > data$ucl_remaining

# Add check for unusually long runs or unusually few crossings.

# runs analysis
runs        <- sign(y - ifelse(x <= 15, data$centerline_first_15, data$centerline_remaining))
runs        <- runs[runs != 0]
runs        <- rle(runs)$lengths
n.obs       <- sum(runs)
longest.run <- max(runs, na.rm = TRUE)
n.runs      <- length(runs)
n.crossings <- n.runs - 1

longest.run.max <- round(log2(n.obs) + 3)
n.crossings.min <- qbinom(.05, n.obs - 1, 0.5)

runs.signal <- longest.run > longest.run.max | n.crossings < n.crossings.min

# Plot a trended control chart
ggplot(data, aes(x = x, y = y)) +
  geom_point(aes(x = x, y = y), shape = 19, size = 3, col = sigma.signals + 1) +
  geom_line() +
  
  # Draw CL, UCL & LCL for the first 15 points
  geom_line(aes(y = ifelse(x <= 15, centerline_first_15, NA)), color = "blue", linetype = "solid", linewidth = 1) +
  geom_line(aes(y = ifelse(x <= 15, ucl_first_15, NA)), color = "red", linetype = "dotted", linewidth = 1) +
  geom_line(aes(y = ifelse(x <= 15, lcl_first_15, NA)), color = "red", linetype = "dotted", linewidth = 1) +
  
  # Draw CL, UCL & LCL for the remaining points
  geom_line(aes(y = ifelse(x > 15, centerline_remaining, NA)), color = "green", linetype = "solid", linewidth = 1) +
  geom_line(aes(y = ifelse(x > 15, ucl_remaining, NA)), color = "red", linetype = "dotted", linewidth = 1) +
  geom_line(aes(y = ifelse(x > 15, lcl_remaining, NA)), color = "red", linetype = "dotted", linewidth = 1) +
  
  # Add labels
  labs(title = "Trended Control Chart with Recalced Limits",
       subtitle = "Using Shewhart Rule #1 and Anhoej Run Rules",
       caption = "Perturbation of 3 at Point 16",
       x = "Observation",
       y = "Value") +
  
  # Theme it
  theme_minimal()
