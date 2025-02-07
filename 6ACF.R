# Load necessary libraries
library(tidyverse)
library(lubridate)
library(forecast)

# Define file path #CHANGE TO YOUR DESIRED PATH
file_path <- "C:/Users/steak/Downloads/Forecasting/Spring2022 Electricity Weather and RIT Calendar Data/Spring2022 Electricity Weather and RIT Calendar Data/BuildingBNetDemand.csv"

# Load dataset
data <- read_csv(file_path)

# Convert Time column to datetime format
data$Time <- as.POSIXct(data$Time, format = "%m/%d/%Y %H:%M", tz = "UTC")

# Format Time column to ISO 8601 format (YYYY-MM-DDTHH:MM:SSZ)
data$Time <- format(data$Time, "%Y-%m-%dT%H:%M:%SZ")

# Check data structure
glimpse(data)

# Extract Net_Demand column
net_demand <- data$Net_Demand

# Convert to time series (5-minute intervals, 288 per day)
ts_data <- ts(net_demand, frequency = 288)

# Autocorrelation Function (ACF)
acf(ts_data, lag.max = 100, main="Autocorrelation of Net Demand")

# Autocorrelation Function (ACF) Plot
png("ACF_Plot.png", width = 900, height = 600)  # Adjust resolution
acf(ts_data, lag.max = 100, main = expression(bold("Autocorrelation of Net Demand")),
    cex.main = 2.4,  # Increase title size
    cex.lab = 1.6,   # Increase axis label size
    cex.axis = 1.4,  # Increase tick mark size
    col.lab = "black")  # Keep labels black for readability

