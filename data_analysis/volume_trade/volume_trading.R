# Load required libraries
library(ggplot2)
library(dplyr)
library(zoo)

# Read in CSV file
data <- read.csv("/Users/yujing/Desktop/FYP_working-main/DATA/PART1/10.csv")

# Convert the date to a Date object
data$Index <- as.Date(data$Index)

# Calculate moving averages
data$MA30 <- rollmean(data$Volume, 30, fill = NA)
data$MA100 <- rollmean(data$Volume, 100, fill = NA)

# Reshape data for ggplot
data_long <- data %>% 
  tidyr::gather(key = "Type", value = "Value", MA30, MA100)

# Plot closing prices with moving averages
ggplot(data_long, aes(x = Index, y = Value, color = Type)) +
  geom_line() +
  labs(title = "Volume with 30 and 100 Day Moving Averages(series10)", x = "Date", y = "Volume") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"))
