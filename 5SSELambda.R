# Load necessary libraries
library(readr)
library(ggplot2)
library(forecast)

# Load the data
data <- read_csv("C:/Users/steak/Downloads/Forecasting/Spring2022 Electricity Weather and RIT Calendar Data/Spring2022 Electricity Weather and RIT Calendar Data/BuildingBNetDemand.csv")

# Assuming the time series data is in a column named 'Net_Demand'
# Convert the data to a time series object
ts_data <- ts(data$Net_Demand, frequency = 24)  # Adjust frequency as needed

# Define a function to calculate the SSE for a given lambda
calculate_sse <- function(lambda, ts_data) {
  # Apply exponential smoothing
  fit <- HoltWinters(ts_data, alpha = lambda, beta = FALSE, gamma = FALSE)
  
  # Calculate the sum of squared errors
  sse <- sum((ts_data - fit$fitted[,1])^2, na.rm = TRUE)
  
  return(sse)
}

# Define a range of lambda values to test
lambda_values <- seq(0.01, 0.99, by = 0.01)

# Calculate the SSE for each lambda value
sse_values <- sapply(lambda_values, calculate_sse, ts_data = ts_data)

# Create a data frame for plotting
sse_df <- data.frame(lambda = lambda_values, SSE = sse_values)

# Plot the SSE against lambda
ggplot(sse_df, aes(x = lambda, y = SSE)) +
  geom_line() +
  geom_point() +
  labs(title = "Sum of Squared Errors (SSE) vs Lambda",
       x = "Lambda",
       y = "Sum of Squared Errors (SSE)") +
  theme_minimal()

# Find the lambda with the minimum SSE
optimal_lambda <- lambda_values[which.min(sse_values)]
cat("Optimal lambda:", optimal_lambda, "\n")

data <- read_csv("C:/Users/steak/Downloads/Forecasting/Spring2022 Electricity Weather and RIT Calendar Data/Spring2022 Electricity Weather and RIT Calendar Data/BuildingBNetDemand.csv")

# Convert to time series (assuming 'NetDemand' is the column of interest)
ts_data <- ts(data$NetDemand, frequency = 24*7)  # Adjust frequency based on your data granularity

# Apply Holt-Winters Exponential Smoothing with Multiplicative Seasonality
hw_model <- HoltWinters(ts_data, seasonal = "multiplicative")

# Plot the fitted values
plot(hw_model)


