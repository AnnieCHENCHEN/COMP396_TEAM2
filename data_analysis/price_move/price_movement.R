# Read in CSV file
data <- read.csv("/Users/yujing/Desktop/FYP_working-main/DATA/PART1/10.csv")

# Create time series object
data_ts <- ts(data$Close, start = 1, end = length(data$Close), frequency = 1)

# Create line plot of data
plot(data_ts, type = "l", xlab = "Index", ylab = "Close Price", main = "Time Series Plot 10")
