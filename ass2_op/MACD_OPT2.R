source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/MACD.R') 

sink("optim/opti_MACD1.txt")

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
initUnit <- seq(from=10, to=500, by=30)
spreadPercentage=0.001
moneyRatio <-0.3
series_com <- c(2,4,9) #randomly pick 4 series as a group to optimize

#out-sample parameters
#lookbackSeq
#multiple
#riskRatio
#initUnit
#series_combation


params_comb <- expand.grid(lookback=lookbackSeq,mul=multiple, Ratio=riskRatio,unit=initUnit,
                           spread=spreadPercentage,money=moneyRatio)

# Create a data frame for the series combinations
series_df <- as.data.frame(series_com)
colnames(series_df) <- paste0("series_", 1:4)

# Combine the parameter grid with the series combinations
params_comb <- merge(params_comb, series_df, all=TRUE)

resultsMatrix <- matrix(nrow=nrow(params_comb),ncol=10)
colnames(resultsMatrix) <- c("lookback","multiple","riskRatio","initUnit","moneyRatio","series_use1","series_use2","series_use3","series_use4","PD Ratio")
pfolioPnLList <- vector(mode="list",length=nrow(params_comb)) 
print(nrow(params_comb))

for (i in 1:nrow(params_comb)) {
  params <- list(lookback=params_comb$lookback[[i]],multiple=params_comb$mul[[i]],spreadPercentage=params_comb$spread[[i]],
                 moneyRatio=params_comb$money[[i]],riskRatio=params_comb$Ratio[[i]],initUnit=params_comb$unit[[i]],series=as.numeric(params_comb[, paste0("series_", 1:4)][i, ])) 
  results <- backtest(dataList, getOrders, params, sMult)
  pfolioPnL <- plotResults(dataList,results)
  
  # Do backtest
  resultsMatrix[i,] <- c(params_comb$lookback[[i]],params_comb$mul[[i]],params_comb$Ratio[[i]],
                         params_comb$unit[[i]],params_comb$money[[i]],as.numeric(params_comb[, paste0("series_", 1:4)][i, ]),pfolioPnL$fitAgg)
  
  pfolioPnLList[[i]]<- pfolioPnL
  cat("Just completed",i,"out of",nrow(params_comb),"\n")
  print(resultsMatrix[i,])
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"],decreasing = TRUE),])
sink()
