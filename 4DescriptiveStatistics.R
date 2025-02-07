#hello

# Load necessary libraries
library(dplyr)
library(readr)
library(lubridate)

#change to desired path
setwd("C:/Users/steak/Downloads/Forecasting/Spring2022 Electricity Weather and RIT Calendar Data/Spring2022 Electricity Weather and RIT Calendar Data/")

## change to desired path
# File path
file_path <- "C:/Users/steak/Downloads/Forecasting/Spring2022 Electricity Weather and RIT Calendar Data/Spring2022 Electricity Weather and RIT Calendar Data/BuildingBNetDemand.csv"

# Load the dataset
data <- read_csv(file_path)

# Convert the "Time" column to datetime format using the specified format
data$Time <- as.POSIXct(data$Time, format = "%m/%d/%Y %H:%M")

# Extract year, month, and Year-Month combination
data <- data %>%
  mutate(Year = year(Time),
         Month = month(Time, label = TRUE),  # Month as a factor
         YearMonth = format(Time, "%Y-%m"))  # YYYY-MM format

# Select only the required numeric columns
selected_cols <- c("BuildingDemand_B", "SolarF02_B", "Net_Demand")

# Function to calculate descriptive statistics
calc_stats <- function(df, group_by_cols = NULL) {
  df %>%
    group_by(across(all_of(group_by_cols))) %>%
    summarise(across(all_of(selected_cols), list(
      Max = max, 
      Min = min, 
      Mean = mean, 
      Variance = var, 
      StdDev = sd
    ), na.rm = TRUE), .groups = "drop")
}

# 1. Descriptive statistics for the entire time series
overall_stats <- calc_stats(data)

# 2. Yearly statistics
yearly_stats <- calc_stats(data, "Year")

# 3. Monthly statistics (aggregated across all years)
monthly_stats <- calc_stats(data, "Month")

# 4. Year-Month combinatory statistics
year_month_stats <- calc_stats(data, "YearMonth")

# Save results to CSV files
write_csv(overall_stats, "BuildingB_Overall_Stats.csv")
write_csv(yearly_stats, "BuildingB_Yearly_Stats.csv")
write_csv(monthly_stats, "BuildingB_Monthly_Stats.csv")
write_csv(year_month_stats, "BuildingB_YearMonth_Stats.csv")

cat("Descriptive statistics calculated and saved as CSV files.\n")

