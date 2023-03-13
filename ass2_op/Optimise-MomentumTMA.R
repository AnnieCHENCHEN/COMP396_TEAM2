source('framework/data.R');
source('framework/backtester.R')
source('framework/processResults.R');
source('strategies/Momentum2.R')
sink("TMA-Optimise1-training",split=TRUE)
#training dyas = 500  validation days = 250  testing days = 250
#split data into 3 parts
training_days <- 550
validation_days <- 750
testing_days <- 350
##########################################################################
#training days
 numOfDays <- training_days
 dataList <- getData(directory="PART1")
 dataList <- lapply(dataList, function(x) x[1:numOfDays])
########################################################################
#test
# StartDay <- training_days + 1
# EndDay <- 1100
# dataList <- getData(directory="PART1")
# dataList <- lapply(dataList, function(x) x[StartDay:EndDay])
######################################################################
sMult <- 0.2 # slippage multiplier
####in sample parameter
params_short <- c(5,15,30)
params_medium <- c(35,45,55)
params_long <- c(70,85,100)
params_moneyRate <- c(0.2,0.3)
params_stopRatio <- c(0.2,0.3)
params_riskPortion=c(0.002,0.004)
params_riskPerShare=c(3,4)
params_spreadPercentage=c(0.001)

##############################################
#out-sample parameter
# sdParam <- c(1.5)
# params_short <- c(10)
# params_medium <- c(50)
# params_long <- c(90)
# params_stopRatio <- c(0.4)
# params_riskPortion=c(0.00001)
# params_riskPerShare=c(3.5)
# params_spreadPercentage=c(0.001)
###############################################

params_comb <- expand.grid(short=params_short,medium=params_medium,long=params_long,
                           moneyRate=params_moneyRate, stopRatio=params_stopRatio,riskPortion=params_riskPortion,
                           riskPerShare=params_riskPerShare, spreadPercentage=params_spreadPercentage)
#/, "spreadPercentage"/
#params$spreadPercentage <- params_comb$spreadPercentage[[i]]/, params_comb$spreadPercentage[[i]]

resultsMatrix <- matrix(nrow=nrow(params_comb),ncol=9)
colnames(resultsMatrix) <- c("short","medium","long","moneyRate","stopRatio",
                             "riskPortion", "riskPerShare", "spreadPercentage", "PD Ratio")
pfolioPnLList <- vector(mode="list",length=nrow(params_comb))
print(nrow(params_comb))

for(i in 1:nrow(params_comb)){
  params$series <- 1:10
  params$lookbacks <- list(short=params_comb$short[[i]], medium=params_comb$medium[[i]],
                           long=params_comb$long[[i]])
  params$moneyRate <- params_comb$moneyRate[[i]]
  params$stopRatio <- params_comb$stopRatio[[i]]
  params$riskPortio <- params_comb$riskPortion[[i]]
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
