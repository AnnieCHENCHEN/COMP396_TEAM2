# Load required libraries
library(ggplot2)
library(dplyr)
library(zoo)

# Read in CSV file
data <- read.csv("/Users/yujing/Desktop/FYP_working-main/DATA/PART1/10.csv")

# Convert the date to a Date object
data$Index <- as.Date(data$Index)

# Calculate moving averages
data$MA10 <- rollmean(data$Close, 10, fill = NA)
data$MA30 <- rollmean(data$Close, 30, fill = NA)
data$MA50 <- rollmean(data$Close, 50, fill = NA)
data$MA200 <- rollmean(data$Close, 200, fill = NA)

# Reshape data for ggplot
data_long <- data %>% 
  select(Index, Close, MA10, MA30, MA50, MA200) %>%  # Select only relevant columns
  tidyr::gather(key = "Type", value = "Value", -Index)

# Plot closing prices with moving averages
ggplot(data_long, aes(x = Index, y = Value, color = Type)) +
  geom_line() +
  labs(title = "Closing Prices with Moving Averages (series10)", x = "Date", y = "Price") +
  theme_minimal() +
  scale_color_manual(values = c("black", "blue", "green", "orange", "red"))
