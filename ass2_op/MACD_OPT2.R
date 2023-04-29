source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('ass2Strategies/MACD.R') 

sink("ass2_op/opti_MACD_data2.txt")

#training days = 500  validation days = 250  testing days = 250
#split data into 3 parts
training_days <- 550  
testing_days <- 250
##########################################################################
#training days
numOfDays <- training_days  
dataList <- getData(directory="PART2")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
#######################################################################
#test
#StartDay <- training_days + 1
#EndDay <- 1100
#dataList <- getData(directory="PART2")
#dataList <- lapply(dataList, function(x) x[StartDay:EndDay])
######################################################################
sMult <- 0.2 # slippage multiplier
# in-sample parameters
lookbackSeq <- seq(from=50,to=100,by=10)
multiple <- 3
riskRatio <- 0.01
initUnit <- seq(from=50, to=500, by=25)
spreadPercentage=0.001
moneyRatio <-0.3
series_com <- c(3，9) #choose serie 3 and 9 in MACD strategy

params_comb <- expand.grid(lookback=lookbackSeq,mul=multiple, Ratio=riskRatio,unit=initUnit,
                           spread=spreadPercentage,money=moneyRatio)


resultsMatrix <- matrix(nrow=nrow(params_comb),ncol=6)
colnames(resultsMatrix) <- c("lookback","multiple","riskRatio","initUnit","moneyRatio","PD Ratio")
pfolioPnLList <- vector(mode="list",length=nrow(params_comb)) 
print(nrow(params_comb))

for (i in 1:nrow(params_comb)) {
  params <- list(lookback=params_comb$lookback[[i]],multiple=params_comb$mul[[i]],spreadPercentage=params_comb$spread[[i]],
                 moneyRatio=params_comb$money[[i]],riskRatio=params_comb$Ratio[[i]],initUnit=params_comb$unit[[i]],series=c(3,9)) 
  results <- backtest(dataList, getOrders, params, sMult)
  pfolioPnL <- plotResults(dataList,results)
  
  # Do backtest
  resultsMatrix[i,] <- c(params_comb$lookback[[i]],params_comb$mul[[i]],params_comb$Ratio[[i]],
                         params_comb$unit[[i]],params_comb$money[[i]],pfolioPnL$fitAgg)
  
  pfolioPnLList[[i]]<- pfolioPnL
  cat("Just completed",i,"out of",nrow(params_comb),"\n")
  print(resultsMatrix[i,])
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"],decreasing = TRUE),])
sink()
