source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('framework/utilities.R'); # for backtestAndPlot function
source('strategies/momentum.R') 

#################################################################
# DATA
#################################################################
dataList <- getData(directory="PART1")

#################################################################
# STRATEGY
#################################################################
strategyFile <-'strategies/momentum.R'
cat("Sourcing",strategyFile,"\n")
source(strategyFile) # load in getOrders

################################################################
# BACKTEST PARAMETERS
numOfDays <- 200 # don't use all available days to start with!
dataList  <- lapply(dataList, function(x) x[1:numOfDays])
#dataList  <- lapply(dataList, function(x) x[(numOfDays+1):nrow(dataList[[1]])])

sMult <- 0.20 # slippage multiplier

################################################################
# DO BACKTEST
################################################################

path <- paste0("lect4images/")

################################################################
# STRATEGY PARAMETERS
params1 <- list(lookbacks=list(short=5,medium=10,long=20),sdParam=1.5,series=1:10,posSizes=rep(1,10))
 # buy and hold equal positions
params <- params1
numOfDays <- 500 
# dataList  <- lapply(dataList, function(x) x[(numOfDays+1):nrow(dataList[[1]])])
dataList  <- lapply(dataList, function(x) x[1:numOfDays])
backtestAndPlot(path=path,
                filename="buy_and_hold_one_PART1",
                main="Equal position sizes")

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
p2 <- round(largestClose/closes)
PAR <- list(lookbacks=list(short=5,medium=10,long=20),sdParam=1.5,series=1:10,posSizes=p2)
params<- PAR# inversely proportional to starting open
print(p2)
backtestAndPlot(path=path,
                filename="inversely_prop_close",
                main="Position sizes inversely proportional to Close")


largestAvgAbsDiffs <- max(avgAbsDiffs)
p3 <- round(largestAvgAbsDiffs/avgAbsDiffs)
PAR <- list(lookbacks=list(short=5,medium=10,long=20),sdParam=1.5,series=1:10,posSizes=p3)
params<- PAR # inversely proportional to average abs diff
print(p3)
backtestAndPlot(path=path,
                filename="inversely_prop_diffs",
                main="Position sizes inversely proportional to Average Abs Diffs")

# 
# estCostToBuy <- sum(p3 * closes)
# 
# target <- 300000 # Try to spend this much
# multiplier <- target / estCostToBuy
# p4 <- round(multiplier * p3)
# print(p4)
# PAR <- list(lookbacks=list(short=5,medium=10,long=20),sdParam=1.5,series=1:10,posSizes=p4)
# params<- PAR
# 
# backtestAndPlot(path=path,
#                 filename="spend_target",
#                 main=paste0("Spending 300,000"))
