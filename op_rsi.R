source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/rsi_contrarian.R') 

#training dyas = 500  validation days = 250  testing days = 250
#split data into 3 parts
training_days <- 500  
validation_days <- 250  
testing_days <- 250
##########################################################################
#training days
numOfDays <- training_days  
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
########################################################################
#validation 
# StartDay <- training_days+1
# EndDay <- training_days+250
# dataList <- getData(directory="PART1")
# dataList <- lapply(dataList, function(x) x[StartDay:EndDay])
#######################################################################
#test
# StartDay <- 751
# EndDay <- validation_days+250
# dataList <- getData(directory="PART1")
# dataList <- lapply(dataList, function(x) x[StartDay:EndDay])
######################################################################

sMult <- 0.2 # slippage multiplier
#sMult <- 0
lookbackSeq <- c(5,15,25,35,45,55,65,75,85,95,105,115)
threshold <- c(5,10,15,20)
P_size <- list(c(136,4860,11,123,21,21066,1,1652,6,95),
               c(811,28972,66,733,125,125581,6,9848,36,566),
               c(272,9541,22,248,44,41488,2,3346,14,180))

params_comb <- expand.grid(lookback=lookbackSeq,threshold=threshold, pSize=P_size)

resultsMatrix <- matrix(nrow=nrow(params_comb),ncol=4)
colnames(resultsMatrix) <- c("lookback","threshold","posSize","PD Ratio")
pfolioPnLList <- vector(mode="list",length=nrow(params_comb)) 
print(nrow(params_comb))

for (i in 1:nrow(params_comb)) {
  params <- list(lookback=params_comb$lookback[[i]],threshold=params_comb$threshold[[i]],
                 series=1:10,posSizes=params_comb$pSize[[i]]) 
  results <- backtest(dataList, getOrders, params, sMult)
  pfolioPnL <- plotResults(dataList,results)
  if(params_comb$pSize[[i]] == P_size[[1]]){
    resultsMatrix[i,] <- c(params_comb$lookback[[i]],params_comb$threshold[[i]],
                           "close_AADS",pfolioPnL$fitAgg)
  }
  if(params_comb$pSize[[i]] == P_size[[2]]){
    resultsMatrix[i,] <- c(params_comb$lookback[[i]],params_comb$threshold[[i]],
                           "close_ST",pfolioPnL$fitAgg)
  }
  if(params_comb$pSize[[i]] == P_size[[3]]){
    resultsMatrix[i,] <- c(params_comb$lookback[[i]],params_comb$threshold[[i]],
                           "open_ST",pfolioPnL$fitAgg)
  }
  pfolioPnLList[[i]]<- pfolioPnL
  cat("Just completed",i,"out of",nrow(params_comb),"\n")
  print(resultsMatrix[i,])
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"],decreasing=TRUE),])
