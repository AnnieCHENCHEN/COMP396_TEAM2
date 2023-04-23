# Load required libraries
library(quantmod)
library(ggplot2)
library(dplyr)
library(zoo)

# Read in CSV file
data <- read.csv("/Users/yujing/Desktop/FYP_working-main/DATA/PART1/10.csv")
data$Index <- as.Date(data$Index, format="%Y-%m-%d")

# Define a function to calculate n-day historical volatility
historical_volatility <- function(series_data, n = 30) {
  daily_returns <- diff(log(series_data))[-1]
  volatility <- rollapply(daily_returns, width = n, FUN = sd, fill = NA) * sqrt(252) * 100
  return(volatility)
}

# Calculate volatility for the "Close" price series
series_volatility <- historical_volatility(data$Close, 30)

# Create a data frame with the calculated volatility data
volatility_data <- data.frame(
  Date = data$Index[-1],
  Series = "Close",
  volatility = c(NA, series_volatility)
)

# Draw a volatility graph
ggplot(volatility_data, aes(x = Date, y = volatility, color = Series)) +
  geom_line(na.rm = TRUE) + # Add na.rm = TRUE argument
  labs(title = "30-Day Historical Volatility for serie10 (Close Price)", x = "Date", y = "Volatility (%)") +
  theme_minimal()
