example_strategies <- c("fixed", 
                        "big_spender",
                        "bankrupt", 
                        "copycat", 
                        "random", 
                        "rsi_contrarian", 
                        "bbands_trend_following",
                        "bbands_contrarian",
                        "bbands_holding_period",
                        "simple_limit",
                        "extreme_limit",
                        "meanReversion",
                        "Momentum-TMA",
                        "MACD",
                        "turtle_trade",
                        "TEAM2"
)

example_params <- list(
  "fixed"=list(sizes=rep(1,10)),
  "big_spender"=list(sizes=rep(1,10)),
  "bankrupt"=list(leverage=40000000),
  "copycat"=NULL,
  "random"=list(maxLots=100),
  "rsi_contrarian"=list(lookback=10,threshold=10,series=1:10),
  "bbands_contrarian"=list(lookback=20,sdParam=1.5,series=1:10,posSizes=rep(1,10)),
  "bbands_trend_following"=list(lookback=50,sdParam=1.5,series=c(1,3,5,7,8,9),posSizes=rep(1,10)),
  "bbands_holding_period"=list(lookback=50,sdParam=1.5,series=c(1,3),posSizes=rep(1,10),holdPeriod=6),
  "simple_limit"=list(spreadPercentage=0.001,inventoryLimits=rep(10,10)),
  "extreme_limit"=list(spreadPercentage=0.001,inventoryLimits=rep(10,10)),
  "meanReversion"=list(lookback=20,sdParam=1.5,series=1,posSizes=rep(1,10), profit_target=50000),
  "Momentum-TMA" =list(lookbacks=list(short=as.integer(30),medium=as.integer(50),long=as.integer(100)),series=c(1,5,10),stopRatio=0.5,
                    riskPortion=0.001,riskPerShare=3,moneyRate=0.4,spreadPercentage=0.001),
  "MACD" = list(lookback=100,series=c(2,3,9),spreadPercentage=0.001,
                multiple=3,moneyRatio=0.3,riskRatio=0.00043,initUnit=80),
  "turtle_trade"=list(periods=list(Ex_1=10,En_1=20, En_2=110),size=0.01,unitRatio=0.02,
                      capi_Ratio=0.2,multi=7,Multi_N=7,series=c(2,4,8)),
  "TEAM2" = list(periods=list(Ex_1=10,En_1=20, En_2=110),size=0.01,unitRatio=0.02,
                 capi_Ratio=0.2,multi=7,Multi_N=7,
                 LOOKBACK=50,multiple=4,moneyRatio=0.3,riskRatio=0.03,initUnit=20,
                 lookbacks=list(short=30,medium=50,long=100),stopRatio=0.5,
                 riskPortion=0.001,riskPerShare=3,series=1:10,moneyRate=0.4,spreadPercentage=0.001)
)

load_strategy <- function(strategy) {
  
  strategyFile <- file.path('ass2Strategies', paste0(strategy,'.R'))
  
  # load strategy
  cat("Sourcing",strategyFile,"\n")
  source(strategyFile) # load in getOrders
  
  # set params
  params <<- example_params[[strategy]]
  print("Parameters:")
  print(params)
}
