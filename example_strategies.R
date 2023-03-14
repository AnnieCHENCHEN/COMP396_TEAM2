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
                        "Momentum4",
                        "MACD",
                        "turtle_trade"
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
  "Momentum4" =list(lookbacks=list(short=as.integer(5),medium=as.integer(10),long=as.integer(20)),sdParam=1.5,series=1:10,posSizes=rep(1,10)),
  "MACD" = list(lookback=80,series=1:10,spreadPercentage=0.001,
                multiple=4,moneyRatio=0.3,riskRatio=0.01,initUnit=30),
  "turtle_trade"=list(periods=list(Ex_1=10,En_1=20, En_2=55), series=3, size=0.01, moneyRatio=0.02, Units=0,
                      capi_Ratio=0.3, spreadPercentage=0.001, multi=4, Multi_N=5)
)

load_strategy <- function(strategy) {
  
  strategyFile <- file.path('strategies', paste0(strategy,'.R'))
  
  # load strategy
  cat("Sourcing",strategyFile,"\n")
  source(strategyFile) # load in getOrders
  
  # set params
  params <<- example_params[[strategy]]
  print("Parameters:")
  print(params)
}
