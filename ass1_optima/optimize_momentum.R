source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/momentum.R')

#training dyas = 500  validation days = 250  testing days = 250
#split data into 3 parts
training_days <- 550  
validation_days <- 250  
testing_days <- 350
##########################################################################
#training days
# numOfDays <- training_days  
# dataList <- getData(directory="PART1")
# dataList <- lapply(dataList, function(x) x[1:numOfDays])
########################################################################
#test
StartDay <- training_days + 1
EndDay <- 1100
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[StartDay:EndDay])
######################################################################
sMult <- 0.2 # slippage multiplier

####in sample parameter
# sdParam <- c(1,1.5,2)
# params_short <- c(5,10,15)
# params_medium <- c(30,50,60)
# params_long <- c(80,90,100)
# P_size <- list(c(464,15544,40,391,51,10663,1,5715,17,106),
#                c(136,4860,11,123,21,21066,1,1652,6,95))
##############################################
#out-sample parameter
sdParam <- c(1.5)
params_short <- c(10)
params_medium <- c(50)
params_long <- c(90)
P_size <- list(c(136,4860,11,123,21,21066,1,1652,6,95))

params_comb <- expand.grid(short=params_short,medium=params_medium,
                           long=params_long,sd=sdParam, pSize=P_size)

resultsMatrix <- matrix(nrow=nrow(params_comb),ncol=6)
colnames(resultsMatrix) <- c("short","medium","long","sdParam","posSize","PD Ratio")
pfolioPnLList <- vector(mode="list",length=nrow(params_comb)) 
print(nrow(params_comb))

for(i in 1:nrow(params_comb)){
  params$series <- 1:10
  params$lookbacks <- list(short=params_comb$short[[i]], medium=params_comb$medium[[i]],
                           long=params_comb$long[[i]])
  params$sdParam <- params_comb$sd[[i]]
  params$posSizes <- params_comb$pSize[[i]]
  # Do backtest
  results <- backtest(dataList,getOrders,params,sMult)
  pfolioPnL <- plotResults(dataList,results)
  pfolioPnLList[[i]]<- pfolioPnL
  if(all(params_comb$pSize[[i]] == P_size[[1]]) == TRUE){
    resultsMatrix[i,] <- c(params_comb$short[[i]],params_comb$medium[[i]],
                           params_comb$long[[i]],params_comb$sd[[i]],
                           "AADS",pfolioPnL$fitAgg)
  }
  # if(all(params_comb$pSize[[i]] == P_size[[2]]) == TRUE){
  #   resultsMatrix[i,] <- c(params_comb$short[[i]],params_comb$medium[[i]],
  #                          params_comb$long[[i]],params_comb$sd[[i]],
  #                          "AADS",pfolioPnL$fitAgg)
  # }
  cat("Just completed",i,"out of",nrow(params_comb),"\n")
  print(resultsMatrix[i,])
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"],decreasing = TRUE),])


