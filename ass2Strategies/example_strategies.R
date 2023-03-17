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
                        "TMA",
                        "turtle_trade",
                        "MACD",
                        "combination"
                        )

example_params <- list(
                    "fixed"=list(sizes=rep(1,10)),
                    "big_spender"=list(sizes=rep(1,10)),
                    "bankrupt"=list(leverage=40000000),
                    "copycat"=NULL,
                    "random"=list(maxLots=100),
                    "rsi_contrarian"=list(lookback=10,threshold=10,series=1:10, posSizes=rep(1,10)),
                    "bbands_contrarian"=list(lookback=20,sdParam=1.5,series=1,posSizes=rep(1,10)),
                    "bbands_trend_following"=list(lookback=50,sdParam=1.5,series=c(1,3,5,7,8,9),posSizes=rep(1,10)),
                    "bbands_holding_period"=list(lookback=50,sdParam=1.5,series=c(1,3),posSizes=rep(1,10),holdPeriod=6),
                    "simple_limit"=list(spreadPercentage=0.001,inventoryLimits=rep(10,10)),
                    "extreme_limit"=list(spreadPercentage=0.001,inventoryLimits=rep(10,10)),
                    "meanReversion"=list(lookback=20,sdParam=1.5,series=1:10,posSizes=c(136,4773,11,124,22,20754,1,1674,7,90)),
                    "TMA"=list(lookbacks=list(short=15,medium=40,long=90), series=7,stopRatio=0.7,
                               riskPortion=0.01,riskPerShare=4,moneyRate=0.03,spreadPercentage=0.001),
                    "turtle_trade"=list(periods=list(Ex_1=10,En_1=20, En_2=55), series=1:10, size=0.01, 
                                        moneyRatio=0.02,capi_Ratio=0.3,spreadPercentage=0.001,
                                        multi=4, Multi_N=5),
                    "MACD" = list(lookback=50,series=1,spreadPercentage=0.001,multiple=1,moneyRatio=0.3,
                                  riskRatio=0.02,initUnit=5),
                    "combination" = list(periods=list(Ex_1=10,En_1=20, En_2=55),size=0.01,unitRatio=0.02,
                                        capi_Ratio=0.3,multi=4,Multi_N=5,
                                        LOOKBACK=50,multiple=1,moneyRatio=0.3,riskRatio=0.02,initUnit=5,
                                        lookbacks=list(short=15,medium=40,long=90),stopRatio=0.3,
                                        riskPortion=0.001,riskPerShare=4,series=c(2,3,4,6,8,10),spreadPercentage=0.001)
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
    print(is.list(params))
}
