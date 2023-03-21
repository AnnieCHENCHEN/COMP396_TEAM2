source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/MACD.R') 

sink("optimise_data/INSAMPLE_MACD_data1.txt")

#training days = 500  validation days = 250  testing days = 250
#split data into 3 parts
training_days <- 550  
testing_days <- 250
##########################################################################
#training days
numOfDays <- training_days  
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
#######################################################################
#test
#StartDay <- training_days + 1
#EndDay <- 1100
#dataList <- getData(directory="PART1")
#dataList <- lapply(dataList, function(x) x[StartDay:EndDay])
######################################################################
sMult <- 0.2 # slippage multiplier
# in-sample parameters
lookbackSeq <- seq(from=50,to=110,by=20)
multiple <- 3
riskRatio <- 0.02
initUnit <- seq(from=20, to=200, by=60)
spreadPercentage=0.001
moneyRatio <-0.3
series_com <- t(combn(1:10,3)) #randomly pick 4 series as a group to optimize

#out-sample parameters
#lookbackSeq
#multiple
#riskRatio
#initUnit
#series_combation


params_comb <- expand.grid(lookback=lookbackSeq,unit=initUnit)

# Create a data frame for the series combinations
series_df <- as.data.frame(series_com)
colnames(series_df) <- paste0("series_", 1:3)

# Combine the parameter grid with the series combinations
params_comb <- merge(params_comb, series_df, all=TRUE)

resultsMatrix <- matrix(nrow=nrow(params_comb),ncol=6)
colnames(resultsMatrix) <- c("lookback","initUnit","series_use1","series_use2","series_use3","PD Ratio")
pfolioPnLList <- vector(mode="list",length=nrow(params_comb)) 
print(nrow(params_comb))

for (i in 1:nrow(params_comb)) {
  params <- list(lookback=params_comb$lookback[[i]],multiple=3,spreadPercentage=0.001,
                 moneyRatio=0.3,riskRatio=0.02,initUnit=params_comb$unit[[i]],series=as.numeric(params_comb[, paste0("series_", 1:3)][i, ])) 
  results <- backtest(dataList, getOrders, params, sMult)
  pfolioPnL <- plotResults(dataList,results)
  
  # Do backtest
  resultsMatrix[i,] <- c(params_comb$lookback[[i]],params_comb$unit[[i]],as.numeric(params_comb[, paste0("series_", 1:3)][i, ]),pfolioPnL$fitAgg)
  
  pfolioPnLList[[i]]<- pfolioPnL
  cat("Just completed",i,"out of",nrow(params_comb),"\n")
  print(resultsMatrix[i,])
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"],decreasing = TRUE),])
sink()

