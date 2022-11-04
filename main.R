source('C:/Users/13708/Desktop/COMP396fyp/COMP396_TEAM2/framework/data.R'); 
source('C:/Users/13708/Desktop/COMP396fyp/COMP396_TEAM2/framework/backtester.R')
source('C:/Users/13708/Desktop/COMP396fyp/COMP396_TEAM2/framework/processResults.R'); 
source('C:/Users/13708/Desktop/COMP396fyp/COMP396_TEAM2/framework/utilities.R'); # for backtestAndPlot function
source('example_strategies.R');

# load data
dataList <- getData(directory="PART1")

# choose strategy from example_strategies
strategy <- "bbands_contrarian"
          
# check that the choice is valid
is_valid_example_strategy <- function(strategy) { 
    strategy %in% example_strategies
}
stopifnot(is_valid_example_strategy(strategy))

# load in strategy and params
load_strategy(strategy) # function from example_strategies.R

# split data in two (e.g. for in/out test)
numDays <- nrow(dataList[[2]])
inSampDays <- 550
print(numDays)


# in-sample period
dataList <- lapply(dataList, function(x) x[1:inSampDays])

# out-of-sample period
dataList <- lapply(dataList, function(x) x[(inSampDays+1):numDays])

sMult <- 0.20 # slippage multiplier

results <- backtest(dataList,getOrders,params,sMult)
pfolioPnL <- plotResults(dataList,results,plotType='ggplot2')

