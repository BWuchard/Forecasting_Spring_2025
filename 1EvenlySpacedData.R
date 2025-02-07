# Load necessary libraries
library(ggplot2)
library(readr)
library(lubridate)
library(scales)

# Define the file path ## change this to your work directory
file_path <- "C:/Users/steak/Downloads/Forecasting/Spring2022 Electricity Weather and RIT Calendar Data/Spring2022 Electricity Weather and RIT Calendar Data/201806 202112 Raw Electricity Data.csv"

# Read the CSV file
data <- read_csv(file_path)

# Select required columns
selected_data <- data[, c("Time", "BuildingDemand_B", "SolarF02_B")]

# Convert the "Time" column to datetime format using the specified format
selected_data$Time <- as.POSIXct(selected_data$Time, format = "%m/%d/%Y %H:%M")

# --- Count duplicate timestamps ---
duplicate_counts <- selected_data %>%
  group_by(Time) %>%
  summarise(Count = n()) %>%
  filter(Count > 1) %>%
  summarise(Total_Duplicates = sum(Count) - n())  # Count excess occurrences

cat("Total duplicate timestamps:", duplicate_counts$Total_Duplicates, "\n")

# --- Count timestamps NOT in 5-minute increments --- ##change to the amount you want
non_five_min_increments <- selected_data %>%
  filter(minute(Time) %% 5 != 0) %>% ### again change to the amount you want
  nrow()

cat("Total timestamps not in 5-minute increments:", non_five_min_increments, "\n")

# Define a function to create evenly spaced data with a (5-minute interval) ##ditto
create_evenly_spaced_data <- function(data) {
  # Define a sequence of all 5-minute intervals within the dataset range ## ditto ditty
  start <- min(data$Time, na.rm = TRUE)
  end <- max(data$Time, na.rm = TRUE)
  
  # Generate the time sequence
  time_seq <- seq.POSIXt(from = start, to = end, by = "5 min") ##:0 or any minute
  
  # Create a new data frame for the evenly spaced time
  time_df <- data.frame(Time = time_seq)
  
  # Merge the original data with the time sequence
  evenlySpacedData <- merge(time_df, data, by = "Time", all.x = TRUE)
  
  return(evenlySpacedData)
}


# Apply function to create evenly spaced data
selected_data <- create_evenly_spaced_data(selected_data)

# Save the dataframe to a new file
write_csv(selected_data, "C:/Users/steak/Downloads/Forecasting/Spring2022 Electricity Weather and RIT Calendar Data/Spring2022 Electricity Weather and RIT Calendar Data/Electricity_Data_B_EvenlySpaced.csv")

# Define a function to create time series plots for the specified columns
create_time_series_plots <- function(data) {
  # Find the min and max dates for the data
  min_date <- min(data$Time, na.rm = TRUE)
  max_date <- max(data$Time, na.rm = TRUE)
  
  # Generate breaks every 3 months within the range of the data
  breaks_seq <- seq.POSIXt(from = min_date, to = max_date, by = "3 months")
  
  # Plot for Building Demand
  plot_demand <- ggplot(data, aes(x = Time, y = BuildingDemand_B)) +
    geom_line(color = "blue") +
    scale_x_datetime(breaks = breaks_seq,
                     labels = date_format("%b %Y")) +  # Month and Year
    labs(title = "Building B Demand (in kW) after even spacing",
         x = "Time",
         y = "Power (kW)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20, face = "bold"),  # Increase title size
      axis.title.x = element_text(size = 16),  # Increase x-axis label size
      axis.title.y = element_text(size = 16),  # Increase y-axis label size
      axis.text.x = element_text(size = 14, angle = 45, hjust = 1),  # Increase x-axis tick size and rotate
      axis.text.y = element_text(size = 14)  # Increase y-axis tick size
    )
  
  # Plot for Solar Generation
  plot_solar <- ggplot(data, aes(x = Time, y = SolarF02_B)) +
    geom_line(color = "green") +
    scale_x_datetime(breaks = breaks_seq,
                     labels = date_format("%b %Y")) +  # Month and Year
    labs(title = "Solar Generation (in kW) for Building B after even spacing",
         x = "Time",
         y = "Power (kW)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20, face = "bold"),  # Increase title size
      axis.title.x = element_text(size = 16),  # Increase x-axis label size
      axis.title.y = element_text(size = 16),  # Increase y-axis label size
      axis.text.x = element_text(size = 14, angle = 45, hjust = 1),  # Increase x-axis tick size and rotate
      axis.text.y = element_text(size = 14)  # Increase y-axis tick size
    )
  
  return(list(plot_demand, plot_solar))
}


# Generate the plots
plots <- create_time_series_plots(selected_data)

# Print the plots
print(plots[[1]])
print(plots[[2]])


