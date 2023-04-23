# Load required libraries
library(ggplot2)
library(dplyr)
library(zoo)

# Read in CSV file
data <- read.csv("/Users/yujing/Desktop/FYP_working-main/DATA/PART1/10.csv")

# Convert the date to a Date object
data$Index <- as.Date(data$Index)

# Calculate moving averages for volume
data$MA10_Volume <- rollmean(data$Volume, 10, fill = NA)
data$MA30_Volume <- rollmean(data$Volume, 30, fill = NA)

# Reshape data for ggplot
data_long <- data %>%
  tidyr::pivot_longer(cols = c("Volume", "MA10_Volume", "MA30_Volume"), names_to = "Type", values_to = "Value")

# Plot volume and moving averages of volume
ggplot(data_long, aes(x = Index, y = Value, color = Type)) +
  geom_line() +
  labs(title = "Volume and Moving Averages of Volume (series10)", x = "Date", y = "Volume") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red", "grey"))
