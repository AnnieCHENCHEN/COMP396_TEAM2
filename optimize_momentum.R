source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/momentum.R')

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
sdParam <- seq(from=0.5,to=2,by=0.5) 
params_short <- c(5,10,15)
params_medium <- c(30,50,70)
params_long <- c(90,100,110)
# params_series <- list(c(1,2),c(1,3), c(1,4),c(2,3),c(2,4),c(3,4),
#                       c(1,2,3),c(1,2,4),c(1,3,4),c(2,3,4),c(1,2,3,4))
params_comb <- expand.grid(short=params_short,medium=params_medium,
                           long=params_long, sd=sdParam)
resultsMatrix <- matrix(nrow=nrow(params_comb),ncol=5)
colnames(resultsMatrix) <- c("short","medium","long","sdParam","PD Ratio")
# pfolioPnLList <- vector(mode="list",length=nrow(params_comb)) 

for(i in 1:nrow(params_comb)){
  
  params$series <- 1:10
  params$lookback <- list(short=params_comb$short[[i]], medium=params_comb$medium[[i]],
                           long=params_comb$long[[i]])
  params$sdParam <- params_comb$sd[[i]]
  # Do backtest
  results <- backtest(dataList,getOrders,params,sMult)
  pfolioPnL <- plotResults(dataList,results)
  # pfolioPnLList[[i]]<- pfolioPnL
  resultsMatrix[i,] <- c(params_comb$short[[i]],params_comb$medium[[i]],
                         params_comb$long[[i]],params_comb$sd[[i]],pfolioPnL$fitAgg)
  cat("Just completed",i,"out of",nrow(params_comb),"\n")
  print(resultsMatrix[i,])
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),])



