source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/meanReversion.R') 

#training dyas = 500  validation days = 250  testing days = 250
#split data into 3 parts
training_days <- 550  
testing_days <- 250
##########################################################################
#training days
# numOfDays <- training_days  
# dataList <- getData(directory="PART1")
# dataList <- lapply(dataList, function(x) x[1:numOfDays])
#######################################################################
#test
StartDay <- training_days + 1
EndDay <- 1100
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[StartDay:EndDay])
######################################################################
sMult <- 0.2 # slippage multiplier
# in-sample parameters
# lookbackSeq <- c(5,15,25,35,45,55,65,75,85,95,105,115)
# sdParam <- c(0.5,1,1.5,2)
# P_size <- list(c(464,15544,40,391,51,10663,1,5715,17,106),
#                c(136,4860,11,123,21,21066,1,1652,6,95))
#out-sample parameters
lookbackSeq <- c(45)
sdParam <- c(2)
P_size <- list(c(136,4860,11,123,21,21066,1,1652,6,95))

params_comb <- expand.grid(lookback=lookbackSeq,sd=sdParam, pSize=P_size)

resultsMatrix <- matrix(nrow=nrow(params_comb),ncol=4)
colnames(resultsMatrix) <- c("lookback","sdParam","posSize","PD Ratio")
pfolioPnLList <- vector(mode="list",length=nrow(params_comb)) 
print(nrow(params_comb))

for (i in 1:nrow(params_comb)) {
    params <- list(lookback=params_comb$lookback[[i]],sdParam=params_comb$sd[[i]],
                   series=1:10,posSizes=params_comb$pSize[[i]]) 
    results <- backtest(dataList, getOrders, params, sMult)
    pfolioPnL <- plotResults(dataList,results)
    if(all(params_comb$pSize[[i]] == P_size[[1]]) == TRUE){
      resultsMatrix[i,] <- c(params_comb$lookback[[i]],params_comb$sd[[i]],
                             "AADS",pfolioPnL$fitAgg)
    }
    if(all(params_comb$pSize[[i]] == P_size[[2]]) == TRUE){
      resultsMatrix[i,] <- c(params_comb$lookback[[i]],params_comb$sd[[i]],
                             "AADS",pfolioPnL$fitAgg)
    }
    pfolioPnLList[[i]]<- pfolioPnL
    cat("Just completed",i,"out of",nrow(params_comb),"\n")
    print(resultsMatrix[i,])
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"],decreasing = TRUE),])
