## this file is a mess and very much needed help from generative AI

# Load necessary libraries
library(tidyverse)
library(zoo)
library(ggplot2)
library(forecast)
library(lubridate)

# Load data
data <- read_csv("C:/Users/steak/Downloads/Forecasting/Spring2022 Electricity Weather and RIT Calendar Data/Spring2022 Electricity Weather and RIT Calendar Data/BuildingBNetDemand.csv")

# Convert Time column to datetime format
data$Time <- as.POSIXct(data$Time, format = "%m/%d/%Y %H:%M")

# Extract year for faceting
data$Year <- year(data$Time)

# Ensure data is sorted by time
data <- data %>% arrange(Time)

# Find rows with missing values in the 'Net_Demand' column
missing_values <- which(is.na(data$Net_Demand))

# Display the indices of missing values
missing_values

# Get the Time values for the missing Net_Demand data
missing_times <- data$Time[missing_values]
missing_times


# Apply Moving Average (window size = 10)
data$Moving_Avg <- rollmean(data$Net_Demand, k = 10, fill = NA, align = "right")

# Apply Moving Median (window size = 10)
data$Moving_Median <- rollapply(data$Net_Demand, width = 10, FUN = median, fill = NA, align = "right")

# --- Find Optimal Alpha for Single Exponential Smoothing (SES) ---
ses_model_opt <- ets(data$Net_Demand, model = "ANN")  # 'ANN' for SES
optimal_alpha <- ses_model_opt$par["alpha"]

# Apply SES with optimized alpha
ses_model <- ses(data$Net_Demand, alpha = optimal_alpha, initial = "optimal")
data$SES <- fitted(ses_model)

# --- Find Optimal Alpha and Beta for Double Exponential Smoothing (Holt’s Method) ---
holt_model_opt <- ets(data$Net_Demand, model = "AAN")  # 'AAN' for Holt's method (additive trend)
optimal_alpha <- holt_model_opt$par["alpha"]
optimal_beta <- holt_model_opt$par["beta"]

# Apply Holt’s method with optimized parameters
holt_model <- holt(data$Net_Demand, alpha = optimal_alpha, beta = optimal_beta, initial = "optimal")
data$DES <- fitted(holt_model)

# --- Print optimal values ---
cat("Optimal Alpha for SES:", optimal_alpha, "\n")
cat("Optimal Alpha for Holt’s:", optimal_alpha, "\n")
cat("Optimal Beta for Holt’s:", optimal_beta, "\n")


# Function to create faceted plots by Year with enhanced readability
plot_smoothing_by_year <- function(data, y_var, title, color) {
  ggplot(data, aes(x = Time)) +
    geom_line(aes(y = Net_Demand), color = "black", size = 1) +  # Raw data in solid black
    geom_line(aes_string(y = y_var), color = color, size = 1, alpha = 0.4) +  # Smoothed data with transparency
    labs(title = title, x = "Time", y = "Net Demand (kW)") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Larger x-axis labels
      axis.text.y = element_text(size = 12),  # Larger y-axis labels
      axis.title = element_text(size = 14),  # Larger axis titles
      plot.title = element_text(size = 16, face = "bold"),  # Larger and bold plot title
      panel.background = element_rect(fill = "white"),   # White panel background
      plot.background = element_rect(fill = "white")     # White overall background
    )
}
# File path and output directory
output_dir <- "C:\\Users\\steak\\Downloads\\Forecasting\\Spring2022 Electricity Weather and RIT Calendar Data\\Spring2022 Electricity Weather and RIT Calendar Data\\Plots\\"

# Create separate plots for each year with even better resolution and styling
years <- unique(data$Year)
for (year in years) {
  year_data <- data %>% filter(Year == year)
  
  # Generate and save each plot for the current year
  p1 <- plot_smoothing_by_year(year_data, "Moving_Avg", paste("Moving Average Smoothing for Year", year), "blue")
  ggsave(paste0(output_dir, "Moving_Avg_", year, ".png"), plot = p1, dpi = 600, width = 12, height = 7)
  
  p2 <- plot_smoothing_by_year(year_data, "Moving_Median", paste("Moving Median Smoothing for Year", year), "red")
  ggsave(paste0(output_dir, "Moving_Median_", year, ".png"), plot = p2, dpi = 600, width = 12, height = 7)
  
  p3 <- plot_smoothing_by_year(year_data, "SES", paste("Single Exponential Smoothing for Year", year), "green")
  ggsave(paste0(output_dir, "SES_", year, ".png"), plot = p3, dpi = 600, width = 12, height = 7)
  
  p4 <- plot_smoothing_by_year(year_data, "DES", paste("Double Exponential Smoothing for Year", year), "purple")
  ggsave(paste0(output_dir, "DES_", year, ".png"), plot = p4, dpi = 600, width = 12, height = 7)
}

# Generate and save a single plot for all years with even better resolution
p1_all_years <- plot_smoothing_all_years(data, "Moving_Avg", "Moving Average Smoothing for All Years", "blue")
ggsave(paste0(output_dir, "Moving_Avg_All_Years.png"), plot = p1_all_years, dpi = 600, width = 12, height = 7)

p2_all_years <- plot_smoothing_all_years(data, "Moving_Median", "Moving Median Smoothing for All Years", "red")
ggsave(paste0(output_dir, "Moving_Median_All_Years.png"), plot = p2_all_years, dpi = 600, width = 12, height = 7)

p3_all_years <- plot_smoothing_all_years(data, "SES", "Single Exponential Smoothing for All Years", "green")
ggsave(paste0(output_dir, "SES_All_Years.png"), plot = p3_all_years, dpi = 600, width = 12, height = 7)

p4_all_years <- plot_smoothing_all_years(data, "DES", "Double Exponential Smoothing for All Years", "purple")
ggsave(paste0(output_dir, "DES_All_Years.png"), plot = p4_all_years, dpi = 600, width = 12, height = 7)

# Create and save the combined plot for 2022 with raw black data and smoothing methods (Enhanced Readability)
p_2022_all_smoothings <- ggplot(data_2022, aes(x = Time)) +
  # Raw data in black
  geom_line(aes(y = Net_Demand), color = "black", size = 1.5) +  # Thicker raw data line for clarity
  # Smoothed data in different colors
  geom_line(aes(y = Moving_Avg, color = "Moving Average"), size = 1.5) +  
  geom_line(aes(y = Moving_Median, color = "Moving Median"), size = 1.5) +
  geom_line(aes(y = SES, color = "Single Exponential Smoothing"), size = 1.5) +
  geom_line(aes(y = DES, color = "Double Exponential Smoothing"), size = 1.5) +
  labs(title = "Smoothing Methods for 2022", x = "Time", y = "Net Demand (kW)") +
  scale_color_manual(name = "Smoothing Method",
                     values = c("Moving Average" = "#1f77b4",  
                                "Moving Median" = "#d62728",  
                                "Single Exponential Smoothing" = "#2ca02c",  
                                "Double Exponential Smoothing" = "#9467bd")) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Increased font size for x-axis labels
    axis.text.y = element_text(size = 14),  # Increased font size for y-axis labels
    axis.title = element_text(size = 16, face = "bold"),  # Larger and bold axis titles
    plot.title = element_text(size = 18, face = "bold"),  # Even larger and bold plot title
    legend.title = element_text(size = 16, face = "bold"),  # Bigger and bold legend title
    legend.text = element_text(size = 14),   # Increased legend text size
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    legend.position = "top"  # Move legend to the top for more space
  )

# Save the combined plot for 2022 with raw data and smoothing methods
ggsave("C:\\Users\\steak\\Downloads\\Forecasting\\Spring2022 Electricity Weather and RIT Calendar Data\\Spring2022 Electricity Weather and RIT Calendar Data\\Plots\\All_Smoothings_2022.png", 
       plot = p_2022_all_smoothings, 
       dpi = 600,      # Set the DPI to 600 for even better resolution
       width = 12,      # Set width (in inches)
       height = 7)     # Set height (in inches)

# Function to create the overlapping DES plot by Year (Enhanced Readability)
plot_overlapping_des <- function(data, y_var, title) {
  ggplot(data, aes(x = Time)) +
    geom_line(aes_string(y = y_var, color = "factor(Year)"), size = 1.5) +  # Thicker lines for better visibility
    labs(title = title, x = "Time", y = "Net Demand (kW)") +
    scale_color_manual(name = "Year", 
                       values = c("2018" = "red", 
                                  "2019" = "blue", 
                                  "2020" = "green", 
                                  "2021" = "purple", 
                                  "2022" = "orange")) +  # Clear color mapping for years
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Increased font size for x-axis labels
      axis.text.y = element_text(size = 14),  # Increased font size for y-axis labels
      axis.title = element_text(size = 16, face = "bold"),  # Larger and bold axis titles
      plot.title = element_text(size = 18, face = "bold"),  # Even larger and bold plot title
      legend.title = element_text(size = 16, face = "bold"),  # Bigger and bold legend title
      legend.text = element_text(size = 14),   # Increased legend text size
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"),
      legend.position = "top"  # Move legend to the top for better spacing
    )
}


# Create the overlapping DES plot for all years
p_overlapping_des <- plot_overlapping_des(data, "DES", "Double Exponential Smoothing for All Years")

# Save the overlapping DES plot for all years
ggsave("C:\\Users\\steak\\Downloads\\Forecasting\\Spring2022 Electricity Weather and RIT Calendar Data\\Spring2022 Electricity Weather and RIT Calendar Data\\Plots\\Overlapping_DES_All_Years.png", 
       plot = p_overlapping_des, 
       dpi = 600,      # Set the DPI to 600 for even better resolution
       width = 12,      # Set width (in inches)
       height = 7)     # Set height (in inches)


# Required libraries
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

# Step 1: Add month and day information for alignment
data_aligned <- data %>%
  mutate(
    # Extract month and day components
    Month = month(Time),
    Day = day(Time),
    Hour = hour(Time),
    Minute = minute(Time),
    Second = second(Time),
    # Create a standardized date using 2000 as reference year
    Aligned_Time = as.POSIXct(
      sprintf("2000-%02d-%02d %02d:%02d:%02d",
              Month, Day, Hour, Minute, Second),
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    )
  )

# Function to create the overlapping plot (Maximized Readability)
plot_overlapping_seasons <- function(data, y_var, title) {
  if (!y_var %in% names(data)) {
    stop(sprintf("Column '%s' not found in the data", y_var))
  }
  
  ggplot(data, aes(x = Aligned_Time)) +
    geom_line(aes(y = .data[[y_var]], color = factor(Year)), size = 1.8) +  # Thicker lines for better visibility
    labs(
      title = title,
      x = "Month",
      y = "Net Demand (kW)"
    ) +
    scale_color_manual(
      name = "Year",
      values = c(
        "2018" = "red",
        "2019" = "blue",
        "2020" = "green",
        "2021" = "purple",
        "2022" = "orange"
      )
    ) +
    scale_x_datetime(
      labels = date_format("%b"),  # Show month names only
      date_breaks = "1 month",
      expand = c(0, 0)
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16, face = "bold"),  # Bigger x-axis text
      axis.text.y = element_text(size = 16, face = "bold"),  # Bigger y-axis text
      axis.title = element_text(size = 18, face = "bold"),  # Bigger and bold axis titles
      plot.title = element_text(size = 22, face = "bold"),  # Even bigger title
      legend.title = element_text(size = 18, face = "bold"),  # Bigger and bold legend title
      legend.text = element_text(size = 16),  # Bigger legend text
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"),
      legend.position = "top"  # Move legend to the top for better spacing
    )
}

# Create and save the plot
tryCatch({
  p_overlapping_seasons <- plot_overlapping_seasons(
    data_aligned,
    "DES",
    "Overlapping Net Demand by Year"
  )
  
  # Display the plot
  print(p_overlapping_seasons)
  
  # Save the plot
  ggsave(
    "Seasonal_Pattern_Comparison.png",
    plot = p_overlapping_seasons,
    dpi = 600,
    width = 14,
    height = 8
  )
}, error = function(e) {
  message("Error occurred: ", e$message)
})



