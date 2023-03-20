source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/turtle_trade.R') 

sink("optim/opti_Turtle.txt")
#training days = 500, validation days = 250, testing days = 250
#split data into 3 parts
training_days <- 550  
testing_days <- 250

#training days
numOfDays <- training_days  
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[1:numOfDays])

sMult <- 0.2 # slippage multiplier

# in-sample parameters
periods_short <- seq(from=5,to=15,by=5)
periods_med <- seq(from=20,to=60,by=10)
periods_long <- seq(from=80,to=110,by=10)
Multi_N <- seq(from=5, to=10, by=3) 
multi <- c(3,4,5)
capi_Ratio <- c(0.3,0.4,0.5)
spreadPercentage=0.001
moneyRatio <-c(0.02,0.03,0.04,0.05)
series_use <- c(3,6,8)

params_comb <- expand.grid(ex_1=periods_short, en_1=periods_med,
                           en_2=periods_long,mul=Multi_N, Ratio=moneyRatio,
                           mutiple=multi,money=capi_Ratio,series=series_use)

resultsMatrix <- matrix(nrow=nrow(params_comb), ncol=8)
colnames(resultsMatrix) <- c("Ex_1","En_1","En_2","mult_N","moneyRatio",
                             "multi","captialRatio","PD Ratio")
pfolioPnLList <- vector(mode="list",length=nrow(params_comb)) 

for (i in 1:nrow(params_comb)) {
  params <- list(periods=list(Ex_1=params_comb$ex_1[[i]],
                              En_1=params_comb$en_1[[i]],
                              En_2=params_comb$en_2[[i]]),
                 ##########
                 size=0.01,
                 
                 ##########
                 Multi_N=params_comb$mul[[i]],
                 moneyRatio=params_comb$Ratio[[i]],
                 multi=params_comb$mutiple[[i]],
                 spreadPercentage=0.001,
                 capi_Ratio=params_comb$money[[i]],
                 series=params_comb$series[[i]])
  
  results <- backtest(dataList, getOrders, params, sMult)
  pfolioPnL <- plotResults(dataList,results)
  
  # Do backtest
  if(params_comb$ex_1[[i]] < params_comb$en_1[[i]] && params_comb$en_1[[i]] <params_comb$en_2[[i]]){
    
      resultsMatrix[i,] <- c(params_comb$ex_1[[i]],params_comb$en_1[[i]],params_comb$en_2[[i]],params_comb$mul[[i]],params_comb$Ratio[[i]],
                             params_comb$mutiple[[i]],params_comb$money[[i]],pfolioPnL$fitAgg)
    
    pfolioPnLList[[i]]<- pfolioPnL
  }
  cat("Just completed",i,"out of",nrow(params_comb),"\n")
  print(resultsMatrix[i,])
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"],decreasing = TRUE),])
sink()
