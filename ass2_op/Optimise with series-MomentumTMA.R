source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/Momentum2.R') 

sink("TMA-Optimise-series",split=TRUE)

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
params_short <- c(10,25,40)
params_medium <- c(50,65,70)
params_long <- c(80,95,100)
params_moneyRate <- c(0.1,0.2,0.3,0.4)
params_stopRatio <- c(0.5, 0.7,0.9)
params_riskPortion=c(0.01,0.02,0.03)
params_riskPerShare=c(3,4,5)
params_spreadPercentage=c(0.001)
series_com <- t(combn(1:10,4)) #randomly pick 4 series as a group to optimize
########################################
#out-sample parameters
# params_short <- c(30)
# params_medium <- c(55)
# params_long <- c(95)
# params_moneyRate <- c(,0.4)
# params_stopRatio <- c(0.5)
# params_riskPortion=c(0.001)
# params_riskPerShare=c(3)
# params_spreadPercentage=c(0.001)
#series_com <- t(combn(1:10,4)) #randomly pick 4 series as a group to optimize
##########################################

params_comb <- expand.grid(short=params_short,medium=params_medium,long=params_long,
                           moneyRate=params_moneyRate,stopRatio=params_stopRatio,riskPortion=params_riskPortion,
                           riskPerShare=params_riskPerShare, spreadPercentage=params_spreadPercentage)
# Create a data frame for the series combinations
series_df <- as.data.frame(series_com)
colnames(series_df) <- paste0("series_", 1:4)

# Combine the parameter grid with the series combinations
params_comb <- merge(params_comb, series_df, all=TRUE)

resultsMatrix <- matrix(nrow=nrow(params_comb),ncol=13)
colnames(resultsMatrix) <- c("short","medium","long","stopRatio","moneyRate",
                             "riskPortion", "riskPerShare", "spreadPercentage",
                             "series_a","series_b","series_c","series_d","PD Ratio")

pfolioPnLList <- vector(mode="list",length=nrow(params_comb)) 
print(nrow(params_comb))
for(i in 1:nrow(params_comb)){
  params$series <- as.numeric(params_comb[, paste0("series_", 1:4)][i, ])
  params$lookbacks <- list(short=params_comb$short[[i]], medium=params_comb$medium[[i]],
                           long=params_comb$long[[i]])
  params$moneyRate <- params_comb$moneyRate[[i]]
  params$stopRatio <- params_comb$stopRatio[[i]]
  params$riskPortion <- params_comb$riskPortion[[i]]
  params$riskPerShare <- params_comb$riskPerShare[[i]]
  params$spreadPercentage <- params_comb$spreadPercentage[[i]]

  results <- backtest(dataList, getOrders, params, sMult)
  pfolioPnL <- plotResults(dataList,results)
  
  # Do backtest
  resultsMatrix[i,] <- c(params_comb$short[[i]],params_comb$medium[[i]],params_comb$long[[i]],
                         params_comb$moneyRate[[i]],params_comb$stopRatio[[i]],params_comb$riskPortion[[i]],
                         params_comb$riskPerShare[[i]], params_comb$spreadPercentage[[i]],
                         as.numeric(params_comb[, paste0("series_", 1:4)][i, ]),pfolioPnL$fitAgg)
  
  pfolioPnLList[[i]]<- pfolioPnL
  cat("Just completed",i,"out of",nrow(params_comb),"\n")
  print(resultsMatrix[i,])
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"],decreasing = TRUE),])
sink()
