source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/MACD.R') 

#training dyas = 500  validation days = 250  testing days = 250
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
 lookbackSeq <- as.integer(c(50:300))
 multiple <- seq(from=0.5, to=3, by=0.1)
 riskRatio <- c(0.01,0.02)
 initUnit <- as.integer(c(1:100))
 spreadPercentage=0.001
 moneyRatio =0.3
#out-sample parameters
#lookbackSeq <- c(45)
#sdParam <- c(2)
#P_size <- list(c(136,4860,11,123,21,21066,1,1652,6,95))


params_comb <- expand.grid(lookback=lookbackSeq,mul=multiple, Ratio=riskRatio,unit=initUnit,
                           spread=spreadPercentage,money=moneyRatio)

resultsMatrix <- matrix(nrow=nrow(params_comb),ncol=5)
colnames(resultsMatrix) <- c("lookback","multiple","riskRatio","initUnit","PD Ratio")
pfolioPnLList <- vector(mode="list",length=nrow(params_comb)) 
print(nrow(params_comb))

for (i in 1:nrow(params_comb)) {
  
    params <- list(lookback=params_comb$lookback[[i]],multiple=params_comb$mul[[i]],spreadPercentage=params_comb$spread[[i]],
                   moneyRatio=params_comb$money[[i]],series=1:10,
                   riskRatio=params_comb$Ratio[[i]],initUnit=params_comb$unit[[i]]) 
    results <- backtest(dataList, getOrders, params, sMult)
    pfolioPnL <- plotResults(dataList,results)
    
    # Do backtest
    #if(all(params_comb$unit[[i]] == initUnit[[i]]) == TRUE){
      resultsMatrix[i,] <- c(params_comb$lookback[[i]],params_comb$mul[[i]],params_comb$Ratio[[i]],
                             params_comb$unit[[i]],pfolioPnL$fitAgg)
   # }
    pfolioPnLList[[i]]<- pfolioPnL
    cat("Just completed",i,"out of",nrow(params_comb),"\n")
    print(resultsMatrix[i,])
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"],decreasing = TRUE),])
