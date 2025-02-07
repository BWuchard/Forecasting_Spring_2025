# Load necessary libraries
library(readr)
library(imputeTS)  # For imputing missing values
library(ggplot2)
library(scales)
library(dplyr)

# Define the file path for the new data file ## change to your desired paths
file_path <- "C:/Users/steak/Downloads/Forecasting/Spring2022 Electricity Weather and RIT Calendar Data/Spring2022 Electricity Weather and RIT Calendar Data/Electricity_Data_B_EvenlySpaced.csv"
new_file_path <- "C:/Users/steak/Downloads/Forecasting/Spring2022 Electricity Weather and RIT Calendar Data/Spring2022 Electricity Weather and RIT Calendar Data/Electricity_Data_B_Interpolated.csv"

# Read the CSV file
evenlySpacedData <- read_csv(file_path)

# Count values above 8000
count_above_8000 <- sum(evenlySpacedData$BuildingDemand_B > 8000, na.rm = TRUE)

# Count values below 1500
count_below_1500 <- sum(evenlySpacedData$BuildingDemand_B <= 1500, na.rm = TRUE)

# Count missing values before converting values to NA
count_missing_before <- sum(is.na(evenlySpacedData$BuildingDemand_B))

# Replace low values (<= 1500) with NA
evenlySpacedData$BuildingDemand_B[evenlySpacedData$BuildingDemand_B <= 1500] <- NA

# Replace high values (> 8000) with NA
evenlySpacedData$BuildingDemand_B[evenlySpacedData$BuildingDemand_B > 8000] <- NA

# Count NA values after replacement
count_na_after <- sum(is.na(evenlySpacedData$BuildingDemand_B))

# Print the counts
cat("Count of values above 8000:", count_above_8000, "\n")
cat("Count of values below 1500:", count_below_1500, "\n")
cat("Count of missing values before replacement:", count_missing_before, "\n")
cat("Count of NA values after replacement:", count_na_after, "\n")

# Generate breaks every 3 months within the range of the data
min_date <- min(evenlySpacedData$Time, na.rm = TRUE)
max_date <- max(evenlySpacedData$Time, na.rm = TRUE)
breaks_seq <- seq.POSIXt(from = min_date, to = max_date, by = "3 months")

# Perform linear interpolation only for missing values (NA)
interpolated_data <- evenlySpacedData
interpolated_data$BuildingDemand_B <- ifelse(is.na(evenlySpacedData$BuildingDemand_B),
                                             na_interpolation(evenlySpacedData$BuildingDemand_B, option = "linear"),
                                             evenlySpacedData$BuildingDemand_B)

# Identify interpolated values
interpolated_data$Interpolated <- ifelse(is.na(evenlySpacedData$BuildingDemand_B) & !is.na(interpolated_data$BuildingDemand_B), "Interpolated", "Original")

# Time series plot of the Building Demand data (with NAs preserved) and 3-month breaks
ggplot() +
  # Plot the original data (with NAs preserved)
  geom_line(data = evenlySpacedData, aes(x = Time, y = BuildingDemand_B), color = "blue") +
  # Highlight the interpolated values as points (not connected by lines)
  geom_point(data = interpolated_data[interpolated_data$Interpolated == "Interpolated", ], 
             aes(x = Time, y = BuildingDemand_B), color = "red") +
  scale_x_datetime(breaks = breaks_seq, labels = date_format("%b %Y")) +  # 3-month breaks with Month Year labels
  labs(title = "Building B Demand (in kW) with Interpolated Values Highlighted",
       x = "Time", y = "Building Demand (kW)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),  # Increase title size
    axis.title.x = element_text(size = 16),  # Increase x-axis label size
    axis.title.y = element_text(size = 16),  # Increase y-axis label size
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),  # Increase x-axis tick size and rotate
    axis.text.y = element_text(size = 14)  # Increase y-axis tick size
  )

# Convert the "Time" column to datetime format
interpolated_data$Time <- as.POSIXct(interpolated_data$Time, format = "%m/%d/%Y %H:%M")

# Extract year and month for grouping
interpolated_data$YearMonth <- format(interpolated_data$Time, "%Y-%m")  # YYYY-MM format
interpolated_data$Hour <- hour(interpolated_data$Time)

#change to your desired path
new_file_path <- "C:/Users/steak/Downloads/Forecasting/Spring2022 Electricity Weather and RIT Calendar Data/Spring2022 Electricity Weather and RIT Calendar Data/Electricity_Data_B_Interpolated.csv"

# Ensure every SolarF02_B value in 2018 is set to 0
interpolated_data$SolarF02_B[year(interpolated_data$Time) == 2018] <- 0

# Count all NA values in the SolarF02_B column before replacement
count_na_solar <- sum(is.na(interpolated_data$SolarF02_B))

# Replace NA values in SolarF02_B column with 0
interpolated_data$SolarF02_B_with_zeros <- interpolated_data$SolarF02_B
interpolated_data$SolarF02_B_with_zeros[is.na(interpolated_data$SolarF02_B_with_zeros)] <- 0
interpolated_data$SolarF02_B_with_zeros[interpolated_data$SolarF02_B_with_zeros == "NA"] <- 0

# Extract year and month for grouping
interpolated_data$YearMonth <- format(interpolated_data$Time, "%Y-%m")  # YYYY-MM format
interpolated_data$Hour <- hour(interpolated_data$Time)

# Count all zero values in SolarF02_B_with_zeros between 6 AM and 6 PM
count_zero_daytime <- sum(interpolated_data$SolarF02_B_with_zeros == 0 & 
                            interpolated_data$Hour >= 6 & interpolated_data$Hour <= 18)

# Print the counts
cat("Count of NA values in SolarF02_B:", count_na_solar, "\n")
cat("Count of zero values between 6 AM - 6 PM:", count_zero_daytime, "\n")

# Calculate the monthly average excluding zeros and NAs
monthly_avg <- interpolated_data %>%
  filter(SolarF02_B_with_zeros > 0) %>%  # Exclude zeros from the average calculation
  group_by(YearMonth) %>%
  summarise(MonthlyAvg = mean(SolarF02_B_with_zeros, na.rm = TRUE))

# Merge the monthly averages back into the original dataset
interpolated_data <- left_join(interpolated_data, monthly_avg, by = "YearMonth")

#stupid fix to dumb problem
interpolated_data$MonthlyAvg[is.na(interpolated_data$SolarF02_B_with_zeros)] <- 0
interpolated_data$MonthlyAvg[interpolated_data$SolarF02_B_with_zeros == "NA"] <- 0

# Replace zeros in the SolarF02_B_with_zeros column (only during 6 AM - 6 PM) with the monthly average
interpolated_data$SolarF02_B_Adjusted <- ifelse(
  interpolated_data$SolarF02_B_with_zeros == 0 & 
    interpolated_data$Hour >= 6 & interpolated_data$Hour <= 18, 
  interpolated_data$MonthlyAvg, 
  interpolated_data$SolarF02_B_with_zeros
)


#hey Dr. Katie if you read this, this was a very stupid fix to a problem involving NAs that came up when I was doing single/double exponential smoothing.
interpolated_data$SolarF02_B <- interpolated_data$SolarF02_B_Adjusted
interpolated_data$SolarF02_B[is.na(interpolated_data$SolarF02_B)] <- 0
interpolated_data$SolarF02_B[interpolated_data$SolarF02_B == "NA"] <- 0


# Generate breaks every 3 months within the range of the data
min_date <- min(interpolated_data$Time, na.rm = TRUE)
max_date <- max(interpolated_data$Time, na.rm = TRUE)
breaks_seq <- seq.POSIXt(from = min_date, to = max_date, by = "3 months")

# Time series plot of SolarF02_B data with adjusted values
ggplot() +
  # Original solar data
  geom_line(data = interpolated_data, aes(x = Time, y = SolarF02_B), color = "green") +
  # Highlight adjusted daytime zero values in orange
  geom_point(data = interpolated_data[interpolated_data$SolarF02_B_with_zeros == 0 & 
                                        interpolated_data$Hour >= 6 & interpolated_data$Hour <= 18, ], 
             aes(x = Time, y = SolarF02_B_Adjusted), 
             color = "orange", size = 1.5) +
  scale_x_datetime(breaks = breaks_seq, labels = date_format("%b %Y")) +  # 3-month breaks with Month Year labels
  labs(title = "Solar Generation for Building B with Adjusted Daytime Zeros",
       x = "Time", y = "Solar Generation (kW)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),  # Increase title size
    axis.title.x = element_text(size = 16),  # Increase x-axis label size
    axis.title.y = element_text(size = 16),  # Increase y-axis label size
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),  # Increase x-axis tick size and rotate
    axis.text.y = element_text(size = 14)  # Increase y-axis tick size
  )


# Export the updated data to a new CSV file
write_csv(interpolated_data, new_file_path)

cat("Updated data saved successfully to", new_file_path, "\n")

