source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/turtle_trade.R') 

#sink("optimise_data/INSAMPLE_TR_DATA2_468.txt")
#training days = 500, validation days = 250, testing days = 250
#split data into 3 parts
training_days <- 550  
testing_days <- 250

#training days
numOfDays <- training_days  
dataList <- getData(directory="PART2")
dataList <- lapply(dataList, function(x) x[1:training_days])

sMult <- 0.2 # slippage multiplier

# in-sample parameters
periods_short <- seq(from=5,to=15,by=5)
periods_med <- seq(from=20,to=60,by=10)
periods_long <- seq(from=80,to=110,by=10)
Multi_N <- seq(from=5, to=10, by=2) 
multi <- seq(form=3,to=7,by=2)
capi_Ratio <- c(0.3,0.4,0.5,0.6,0.7)
spreadPercentage=0.001
moneyRatio <-c(0.02,0.03,0.04,0.05)

# periods_short <- 10
# periods_med <- 20
# periods_long <- c(55,65,85,95,105,115)
# Multi_N <- 7 
# multi <- 7
# capi_Ratio <- c(0.1,0.2,0.3,0.4,0.5)
# spreadPercentage=0.001
# moneyRatio <- 0.02
# series_use <- c(2,4,8)

params_comb <- expand.grid(ex_1=periods_short, en_1=periods_med,
                           en_2=periods_long,mul=Multi_N, Ratio=moneyRatio,
                           mutiple=multi,money=capi_Ratio,series=c(3,6,8))

#params_comb <- expand.grid(en_2=periods_long, capiR=capi_Ratio)

resultsMatrix <- matrix(nrow=nrow(params_comb), ncol=8)
colnames(resultsMatrix) <- c("Ex_1","En_1","En_2","capi_Ratio","PD Ratio")
pfolioPnLList <- vector(mode="list",length=nrow(params_comb)) 
print(nrow(params_comb))

for (i in 1:nrow(params_comb)) {
  params <- list(periods=list(Ex_1=10,
                              En_1=20,
                              En_2=params_comb$en_2[[i]]),
                 ##########
                 size=0.01,
                 ##########
                 Multi_N=7,
                 moneyRatio=0.02,
                 multi=7,
                 spreadPercentage=0.001,
                 capi_Ratio=params_comb$capiR[[i]],
                 series=c(2,4,8))

  results <- backtest(dataList, getOrders, params, sMult)
  pfolioPnL <- plotResults(dataList,results)
  
  # Do backtests
  resultsMatrix[i,] <- c(10,20,params_comb$en_2[[i]],params_comb$capiR[[i]],pfolioPnL$fitAgg)
  pfolioPnLList[[i]]<- pfolioPnL
  
  cat("Just completed",i,"out of",nrow(params_comb),"\n")
  print(resultsMatrix[i,])
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"],decreasing = TRUE),])
#sink()
