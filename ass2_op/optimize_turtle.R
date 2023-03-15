source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/turtle_trade.R') 

sink("optim/opti_Turtle.txt")

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
periods_short <- seq(from=10, to=50, by=5)
periods_med <- seq(from=15,to=60,by=5)
periods_long <- seq(from=40, to=90,by=5)
Multi_N <- seq(from=5, to=10, by=1)
multi <- seq(from=3, to=5, by=0.5)
capi_Ratio <- seq(from=0.3,to=0.7,by=0.1)
spreadPercentage=0.001
moneyRatio =seq(from=0.02,to=0.05,by=0.01)
series_com <- t(combn(1:10,4)) #randomly pick 4 series as a group to optimize

#out-sample parameters
#lookbackSeq
#multiple
#riskRatio
#initUnit
#series_combation


params_comb <- expand.grid(ex_1=periods_short, en_1=periods_med,
                           en_2=periods_long,mul=Multi_N, Ratio=moneyRatio,mutiple=multi,
                           spread=spreadPercentage,money=capi_Ratio)
# Create a data frame for the series combinations
series_df <- as.data.frame(series_com)
colnames(series_df) <- paste0("series_", 1:4)

# Combine the parameter grid with the series combinations
params_comb <- merge(params_comb, series_df, all=TRUE)

resultsMatrix <- matrix(nrow=nrow(params_comb),ncol=12)
colnames(resultsMatrix) <- c("Ex_1","En_1","En_2","mult_N","moneyRatio","multi","captialRatio","series_use1","series_use2","series_use3","series_use4","PD Ratio")
pfolioPnLList <- vector(mode="list",length=nrow(params_comb)) 
print(nrow(params_comb))

for (i in 1:nrow(params_comb)) {
  params <- list(periods=list(Ex_1=params_comb$ex_1[[i]],En_1=params_comb$en_1[[i]],En_2=params_comb$en_2[[i]]),mutilN=params_comb$mul[[i]],moneyR=params_comb$Ratio[[i]],
                 mutiple=params_comb$mutiple[[i]],spreadPercentage=params_comb$spread[[i]],captial=params_comb$money[[i]],series=as.numeric(params_comb[, paste0("series_", 1:4)][i, ])) 
  results <- backtest(dataList, getOrders, params, sMult)
  pfolioPnL <- plotResults(dataList,results)
  
  # Do backtest
  if(all(params_comb$ex_1[[i]] < params_comb$en_1[[i]] && params_comb$en_1[[i]] <params_comb$en_2[[i]]) == TRUE){
  resultsMatrix[i,] <- c(params_comb$ex_1[[i]],params_comb$en_1[[i]],params_comb$en_2[[i]],params_comb$mul[[i]],params_comb$Ratio[[i]],
                         params_comb$mutiple[[i]],captial=params_comb$money[[i]],as.numeric(params_comb[, paste0("series_", 1:4)][i, ]),pfolioPnL$fitAgg)
  
  pfolioPnLList[[i]]<- pfolioPnL
  }
  cat("Just completed",i,"out of",nrow(params_comb),"\n")
  print(resultsMatrix[i,])
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"],decreasing = TRUE),])
sink()
