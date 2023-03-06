source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('framework/utilities.R'); # for backtestAndPlot function
source('strategies/rsi_contrarian.R') 

#################################################################
# DATA
#################################################################
dataList <- getData(directory="PART1")

#################################################################
# STRATEGY
#################################################################
strategyFile <-'strategies/rsi_contrarian.R'
cat("Sourcing",strategyFile,"\n")
source(strategyFile) # load in getOrders

################################################################
# BACKTEST PARAMETERS
#numOfDays <- 200 # don't use all available days to start with!
dataList  <- lapply(dataList, function(x) x[1:numOfDays])
#dataList  <- lapply(dataList, function(x) x[(numOfDays+1):nrow(dataList[[1]])])

sMult <- 0.20 # slippage multiplier

################################################################
# DO BACKTEST
################################################################

path <- paste0("lect4images/")

################################################################
# STRATEGY PARAMETERS
PAR <- list(lookback=10,threshold=10,series=1:10, posSizes=rep(1,10))
params<- PAR # buy and hold equal positions

numOfDays <- 500 
# dataList  <- lapply(dataList, function(x) x[(numOfDays+1):nrow(dataList[[1]])])
dataList  <- lapply(dataList, function(x) x[1:numOfDays])
backtestAndPlot(path=path,
                filename="buy_and_hold_one_PART1",
                main="Equal position sizes")

# bbands
#params<- list(lookback=10,sdParam=1.25,series=1:5,positionSizes=positionSizes) # bbands
# Open on open differences

closeDiffs <- lapply(dataList,function(x) diff(x$Close))

toPlot <- do.call(cbind,closeDiffs)
colnames(toPlot) <- paste("Series",sprintf("%02d",1:10)) 

plot.zoo(toPlot,
         main="Close on close simple differences",
         cex.axis=1.2,
         cex.main=2,
         yax.flip=TRUE)
dev.copy(pdf,file.path(path,"closediffs.pdf"))
#dev.off()


absCloseDiffs    <- lapply(closeDiffs,abs)
avgAbsDiffs <- sapply(absCloseDiffs,mean,na.rm=TRUE)
closesOnFirstDay <- sapply(dataList,function(x) first(x)$Close)

tab <- cbind(closesOnFirstDay,
             avgAbsDiffs,
             abs(avgAbsDiffs)/closesOnFirstDay)
colnames(tab) <- c("Close","Mean abs diff","Mean abs diff/Close")

print(tab)

closes <- sapply(dataList,function(x) first(x)$Close)
largestClose <- max(closes)
positionSizes <- round(largestClose/closes)
PAR <- list(lookback=10,threshold=10,series=1:10,posSizes=positionSizes)
params<- PAR# inversely proportional to starting open
print(positionSizes)
backtestAndPlot(path=path,
                filename="inversely_prop_close",
                main="Position sizes inversely proportional to Close")


largestAvgAbsDiffs <- max(avgAbsDiffs)
positionSizes <- round(largestAvgAbsDiffs/avgAbsDiffs)
PAR <- list(lookback=10,threshold=10,series=1:10,posSizes=positionSizes)
params<- PAR # inversely proportional to average abs diff
print(positionSizes)
backtestAndPlot(path=path,
                filename="inversely_prop_diffs",
                main="Position sizes inversely proportional to Average Abs Diffs")


# estCostToBuy <- sum(positionSizes * closes)
# 
# target <- 300000 # Try to spend this much
# multiplier <- target / estCostToBuy
# positionSizes <- round(multiplier * positionSizes)
# print(positionSizes)
# PAR <- list(lookback=10,threshold=10,series=1:10,posSizes=positionSizes)
# params<- PAR 
# 
# backtestAndPlot(path=path,
#                 filename="spend_target",
#                 main=paste0("Spending 300,000"))

