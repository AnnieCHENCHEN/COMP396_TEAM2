# Read in CSV file
data <- read.csv("/Users/wenyangyao/Desktop/FYP_TEAM2/FYP_TEAM2/DATA/PART1/01.csv")

# Create time series object
data_ts <- ts(data$Close, start = 1, end = length(data$Close), frequency = 1)

# Create line plot of data
plot(data_ts, type = "l", xlab = "Index", ylab = "Close Price", main = "Time Series Plot 1")