source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/momentum.R') 

training_days <- 500  
validation_days <- 250  
testing_days <- 250
# training dyas = 500  validation days = 250  testing days = 250
#split data into 3 parts
numOfDays <- training_days  
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier

params_short <- c(5,10)
params_medium <- c(30,50)
params_long <- c(70,100)
params_series <- list(c(1,2),c(1,3), c(1,4),c(2,3),c(2,4),c(3,4),
                      c(1,2,3),c(1,2,4),c(1,3,4),c(2,3,4),c(1,2,3,4))
params_comb <- expand.grid(short=params_short,medium=params_medium,
                           long=params_long,series=params_series)

sdParamSeq  <- seq(from=1.5,to=2,by=0.5) 
paramsList  <- list(lookbackSeq,sdParamSeq)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb,ncol=3)
colnames(resultsMatrix) <- c("lookback","sdParam","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (lb in lookbackSeq) {
  for (sdp in sdParamSeq) {
    params <- list(lookback=lb,sdParam=sdp,series=1:4,posSizes=rep(1,10)) 
    results <- backtest(dataList, getOrders, params, sMult)
    pfolioPnL <- plotResults(dataList,results)
    resultsMatrix[count,] <- c(lb,sdp,pfolioPnL$fitAgg)
    pfolioPnLList[[count]]<- pfolioPnL
    cat("Just completed",count,"out of",numberComb,"\n")
    print(resultsMatrix[count,])
    count <- count + 1
  }
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),])
