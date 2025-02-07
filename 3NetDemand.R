# Load necessary libraries
library(readr)
library(ggplot2)
library(scales)  # For date formatting

# Define the file path for the new data file ## change to your desired path
file_path <- "C:/Users/steak/Downloads/Forecasting/Spring2022 Electricity Weather and RIT Calendar Data/Spring2022 Electricity Weather and RIT Calendar Data/Electricity_Data_B_Interpolated.csv"

# Read the CSV file
data <- read_csv(file_path)

# Create a new column 'Net_Demand' by subtracting SolarF02_B from BuildingDemand_B
data$Net_Demand <- data$BuildingDemand_B - data$SolarF02_B

# Generate breaks every 3 months within the range of the data
min_date <- min(data$Time, na.rm = TRUE)
max_date <- max(data$Time, na.rm = TRUE)
breaks_seq <- seq.POSIXt(from = min_date, to = max_date, by = "3 months")

ggplot(data, aes(x = Time, y = Net_Demand)) +
  geom_line(color = "purple") +
  scale_x_datetime(breaks = breaks_seq, labels = date_format("%b %Y")) +  # 3-month breaks with Month Year labels
  labs(title = "Net Electrical Demand of Building B in kW (Building Demand - Solar Generation)",
       x = "Time", y = "Net Electrical Demand (kW)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),  # Increase title size
    axis.title.x = element_text(size = 16),  # Increase x-axis label size
    axis.title.y = element_text(size = 16),  # Increase y-axis label size
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),  # Increase x-axis tick size and rotate
    axis.text.y = element_text(size = 14)  # Increase y-axis tick size
  )


# Save the updated data to a new file ## your desired path
write_csv(data, "C:/Users/steak/Downloads/Forecasting/Spring2022 Electricity Weather and RIT Calendar Data/Spring2022 Electricity Weather and RIT Calendar Data/BuildingBNetDemand.csv")

