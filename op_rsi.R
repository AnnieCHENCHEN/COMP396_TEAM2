source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/rsi_contrarian.R') 

#training dyas = 500  validation days = 250  testing days = 250
#split data into 3 parts
training_days <- 500  
validation_days <- 250  
testing_days <- 250
##########################################################################
#training days
numOfDays <- training_days  
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
########################################################################
#validation 
# StartDay <- training_days+1
# EndDay <- training_days+250
# dataList <- getData(directory="PART1")
# dataList <- lapply(dataList, function(x) x[StartDay:EndDay])
#######################################################################
#test
# StartDay <- 751
# EndDay <- validation_days+250
# dataList <- getData(directory="PART1")
# dataList <- lapply(dataList, function(x) x[StartDay:EndDay])
######################################################################

sMult <- 0.2 # slippage multiplier
#sMult <- 0
lookbackSeq <- seq(from=10,to=70,by=10)
threshold  <- seq(from=5,to=20,by=5) 
paramsList  <- list(lookbackSeq,threshold)
numberComb <- prod(sapply(paramsList,length))

print(numberComb)

resultsMatrix <- matrix(nrow=numberComb,ncol=3)
colnames(resultsMatrix) <- c("lookback","threshold","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (lb in lookbackSeq) {
  for (n in threshold) {
    params <- list(lookback=lb,threshold=n,series=,posSizes=rep(1,10)) 
    results <- backtest(dataList, getOrders, params, sMult)
    pfolioPnL <- plotResults(dataList,results)
    resultsMatrix[count,] <- c(lb,n,pfolioPnL$fitAgg)
    pfolioPnLList[[count]]<- pfolioPnL
    cat("Just completed",count,"out of",numberComb,"\n")
    print(resultsMatrix[count,])
    count <- count + 1
  }
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),])

