source('framework/data.R'); 
source('framework/backtester.R')
source('framework/comparison.R'); 
source('framework/utilities.R'); # for backtestAndPlot function
source('example_strategies.R');

# Load data
dataList_part1 <- getData(directory="PART1")
dataList_part2 <- getData(directory="PART2")
dataList_part3 <- getData(directory="PART3")

# choose strategy from example_strategies
strategy <- "TEAM2"

# check that the choice is valid
is_valid_example_strategy <- function(strategy) { 
  strategy %in% example_strategies
}
stopifnot(is_valid_example_strategy(strategy))

# load in strategy and params
load_strategy(strategy) # function from example_strategies.R

# Set in-sample days for each data list
inSampDays_part1 <- 1100
inSampDays_part2 <- 1100
inSampDays_part3 <- 1044

# in-sample period
dataList_part1 <- lapply(dataList_part1, function(x) x[1:inSampDays_part1])
dataList_part2 <- lapply(dataList_part2, function(x) x[1:inSampDays_part2])
dataList_part3 <- lapply(dataList_part3, function(x) x[1:inSampDays_part3])

sMult <- 0.20 # slippage multiplier

# Run strategy code for each data list
results_part1 <- backtest(dataList_part1, getOrders, params, sMult)
results_part2 <- backtest(dataList_part2, getOrders, params, sMult)
results_part3 <- backtest(dataList_part3, getOrders, params, sMult)

# Generate and print the total returns comparison plot
total_returns_comparison_plot <- plotTotalReturnsComparison(results_part1, results_part2, results_part3)
print(total_returns_comparison_plot)
