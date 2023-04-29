source('framework/data.R');
source('framework/backtester.R')
source('framework/processResults.R');
source('strategies/Momentum2.R')

sink("TMA-Optimise5",split=TRUE)

#training dyas = 500  testing days = 550
#split data into 3 parts
training_days <- 550
testing_days <- 550
##########################################################################
#training days
numOfDays <- training_days
dataList <- getData(directory="PART2")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
########################################################################
#test
# StartDay <- training_days + 1
# EndDay <- 550
# dataList <- getData(directory="PART2")
# dataList <- lapply(dataList, function(x) x[StartDay:EndDay])
######################################################################
sMult <- 0.2 # slippage multiplier
####in sample parameter
params_short <- c(5,10,15,20,25,30)
params_medium <- c(50,55,60)
params_long <- c(70,75,80,85,90,95,100)
params_moneyRate <- c(0.1,0.2,0.3,0.4)
params_stopRatio <- c(0.5)
params_riskPortion=c(0.001,0.002,0.003)
params_riskPerShare=c(3)
params_spreadPercentage=c(0.001)
##############################################
#out-sample parameter
# params_short <- c(30)
# params_medium <- c(50)
# params_long <- c(100)
# params_moneyRate <- c(0.4)
# params_stopRatio <- c(0.5)
# params_riskPortion=c(0.001)
# params_riskPerShare=c(3)
# params_spreadPercentage=c(0.001)
###############################################
params_comb <- expand.grid(short=params_short,medium=params_medium,long=params_long,
                           moneyRate=params_moneyRate,stopRatio=params_stopRatio,riskPortion=params_riskPortion,
                           riskPerShare=params_riskPerShare, spreadPercentage=params_spreadPercentage)

resultsMatrix <- matrix(nrow=nrow(params_comb),ncol=9)
colnames(resultsMatrix) <- c("short","medium","long","moneyRate","stopRatio",
                             "riskPortion", "riskPerShare", "spreadPercentage", "PD Ratio")
pfolioPnLList <- vector(mode="list",length=nrow(params_comb))
print(nrow(params_comb))
for(i in 1:nrow(params_comb)){
  #params$series <-c(1,5,9,10)
 #1. params$series <-c(1,5,7)
 #2. params$series <-c(1,5,9)
 #3. params$series <-c(1,5,10)
 #4. params$series <-c(5,9,10)
 
 # params$series <- 1:10
  params$series <-c(1,5,10)
  params$lookbacks <- list(short=params_comb$short[[i]], medium=params_comb$medium[[i]],
                           long=params_comb$long[[i]])
  params$moneyRate <- params_comb$moneyRate[[i]]
  params$stopRatio <- params_comb$stopRatio[[i]]
  params$riskPortion <- params_comb$riskPortion[[i]]
  params$riskPerShare <- params_comb$riskPerShare[[i]]
  params$spreadPercentage <- params_comb$spreadPercentage[[i]]
  
  # Do backtest
  results <- backtest(dataList,getOrders,params,sMult)
  pfolioPnL <- plotResults(dataList,results)
  pfolioPnLList[[i]]<- pfolioPnL
  #if(all(params_comb$pSize[[i]] == P_size[[1]]) == TRUE){
  resultsMatrix[i,] <- c(params_comb$short[[i]],params_comb$medium[[i]],params_comb$long[[i]],
                         params_comb$moneyRate[[i]],params_comb$stopRatio[[i]],params_comb$riskPortion[[i]],
                         params_comb$riskPerShare[[i]], params_comb$spreadPercentage[[i]],
                         pfolioPnL$fitAgg)
  # }
  cat("Just completed",i,"out of",nrow(params_comb),"\n")
  print(resultsMatrix[i,])
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"],decreasing = TRUE),])
sink()
