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
lookbackSeq <- as.integer(c(50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200))
multiple <- seq(from=1, to=5, by=1)
riskRatio <- c(0.01,0.02)
initUnit <- as.integer(c(5,10,15,25,35,45))
spreadPercentage=0.001
moneyRatio <-seq(from=0.1,to=0.5,by=0.1)
#series_com <- list(t(combn(1:10,4))) #randomly pick 4 series as a group to optimize

#out-sample parameters
#lookbackSeq
#multiple
#riskRatio
#initUnit
#series_combation


params_comb <- expand.grid(lookback=lookbackSeq,mul=multiple, Ratio=riskRatio,unit=initUnit,
                           spread=spreadPercentage,money=moneyRatio)

resultsMatrix <- matrix(nrow=nrow(params_comb),ncol=6)
colnames(resultsMatrix) <- c("lookback","multiple","riskRatio","initUnit","moneyRatio","PD Ratio")
pfolioPnLList <- vector(mode="list",length=nrow(params_comb)) 
print(nrow(params_comb))

for (i in 1:nrow(params_comb)) {
  params <- list(lookback=params_comb$lookback[[i]],multiple=params_comb$mul[[i]],spreadPercentage=params_comb$spread[[i]],
                 moneyRatio=params_comb$money[[i]],riskRatio=params_comb$Ratio[[i]],initUnit=params_comb$unit[[i]],series=1:10) 
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
