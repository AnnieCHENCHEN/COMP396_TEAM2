# TEAM 2
# combination of 3 strategies(turtle trade, MACD, TMA)
#
# 
# "TEAM2" = list(periods=list(Ex_1=10,En_1=20, En_2=55),size=0.01,unitRatio=0.02,
#                      capi_Ratio=0.3,multi=4,Multi_N=5,
#                      LOOKBACK=90,multiple=3,moneyRatio=0.3,riskRatio=0.02,initUnit=5,
#                      lookbacks=list(short=10,medium=85,long=95),stopRatio=0.9,
#                      riskPortion=0.003,riskPerShare=3,series=1:10,moneyRate=0.4,spreadPercentage=0.001)
#
require(TTR)
require(ggplot2)
maxRows <- 3100 # depends on the row number of series

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series, params$periods)
  
  marketOrders <- -currentPos
  #### TR trade variables
  pos_long <- allzero
  pos_short <- allzero
  pos_M <- allzero
  
  #### MACD variables
  Bstop_price <-allzero
  Sstop_price <- allzero
  agap <- allzero
  units <- allzero
  
  #### TMA variables
  posSizes <- allzero
  
  if(store$iter > params$lookbacks$long){
    
    ######## Turtle Trade 3 lines ########
    storeOfEnEx <- getIndicatorsAndATR(newRowList,params$series,store,params$periods)
    
    for(i in 1:length(params$series)) {
      
      #### calculate ATR value for MACD and Turtle Trade
      store_data <- data.frame(high=store$Hi[,i], low=store$Lo[,i], close=store$cl[,i])
      N_value <- ATR(store_data, n=20, maType=EMA, wilder=TRUE)[,'atr']

      #-------------------------Turtle Trade Strategy----------------------#
      if(i == 3 || i == 9 || i==10){

        UnitSize <- as.numeric(trunc((params$size * 1000000 * params$capi_Ratio* params$unitRatio)/(N_value[store$iter] * params$multi)))
        # Initiate SHORT position
        if(as.numeric(store$Hi[store$iter,i]) > as.numeric(storeOfEnEx$Max_En2[store$iter-1,i])){

          pos_short[params$series[i]] = -1 * UnitSize

          N_short = as.numeric(N_value[store$iter])
          TxnPrice_S = as.numeric(store$cl[store$iter,i])
          StopPrice_S = TxnPrice_S - params$Multi_N * N_short #the price of exiting market and stop loss
          StopPrice_S2 = TxnPrice_S - (params$Multi_N-3) * N_short #the price of close half position

          # Add to SHORT position
          if(store$Hi[store$iter,i] > ( TxnPrice_S + N_short * (params$Multi_N-2) )) {

            pos_short[params$series[i]] = -2 * UnitSize #add position
          }
        }else{
          # Initiate LONG position
          if(as.numeric(store$Lo[store$iter,i]) < as.numeric(storeOfEnEx$Min_En2[store$iter-1,i])){

            pos_long[params$series[i]] = 1 * UnitSize

            N_long = as.numeric(N_value[store$iter])
            TxnPrice_L = as.numeric(store$cl[store$iter,i])
            StopPrice_L = TxnPrice_L + params$Multi_N*N_long #the price of exiting market and stop loss
            StopPrice_L2 = TxnPrice_L + (params$Multi_N-3)*N_long #the price of close half position

            # Add to LONG position
            if(store$Lo[store$iter,i] < ( TxnPrice_L - N_long*(params$Multi_N-2) )) {

              pos_long[params$series[i]] = 2 * UnitSize#add position
            }
          }
        }
        # Position exits and stops   pos != 0
        #close half position 
        if(( pos_short[params$series[i]] < 0 && (as.numeric(store$Lo[store$iter,i]) < as.numeric(storeOfEnEx$Min_En1[store$iter-1,i]) || as.numeric(store$Lo[store$iter,i]) < StopPrice_S2) ) ||
           ( pos_long[params$series[i]] > 0 && (as.numeric(store$Hi[store$iter,i]) > as.numeric(storeOfEnEx$Max_En1[store$iter-1,i]) || as.numeric(store$Hi[store$iter,i]) > StopPrice_L2) )) {
          
          pos_M[params$series[i]] = trunc((-currentPos[i] / 2))

          #clsoe all position
          if((pos_short[params$series[i]] < 0 && store$Lo[store$iter,i] < StopPrice_S) || (pos_long[params$series[i]] > 0 && store$Hi[store$iter,i] > StopPrice_L)){

            pos_M[params$series[i]] = -currentPos[i]
          }
        }
      }

      #-------------------------MACD Strategy----------------------#
      if(i == 2 || i == 4 || i == 6 || i == 8){
        #Calculate MACD indicators
        macd_data <- MACD(store$cl[1:store$iter,i],nFast = 12, nSlow = 26, nSig =9, percent = TRUE)
        DIFF <- macd_data[,1]
        DEA <- macd_data[,2]

        #risk management part
        #set stop loss price for long and short
        Bstop_price[params$series[i]] <- store$cl[store$iter-1,i]-params$multiple*N_value[store$iter]
        Sstop_price[params$series[i]] <- store$cl[store$iter-1,i]+params$multiple*N_value[store$iter]

        #account risk, got 30% of 10000000 in this strategy
        account_risk = params$riskRatio*params$moneyRatio*10000000

        # Calculate position size based on account risk
        units[params$series[i]] <- round(account_risk / params$multiple*N_value[store$iter])

        if(units[params$series[i]]<=0 || units[params$series[i]]>store$Vol[store$iter,i]){
          units[params$series[i]]<- 50
        }else{
          units[params$series[i]]<-units[params$series[i]]
        }

        #Compare MACD line and signal line, get the position signal
        #Entry market conditions
        if (DIFF[store$iter]>0 && DEA[store$iter]>0 && DIFF[store$iter]>DEA[store$iter]
            && DIFF[store$iter-1]<DEA[store$iter-1]) {

          pos_long[params$series[i]] <- 1*units[params$series[i]]  #buy,+1
        }
        else if (DIFF[store$iter]<0 && DEA[store$iter]<0 && DIFF[store$iter]<DEA[store$iter]
                 && DIFF[store$iter-1]>DEA[store$iter-1] && currentPos[i] !=0) {

          pos_short[params$series[i]] <- -1*units[params$series[i]] #sell, -1
        }
        #----Exit market----if store$cl[store$iter,i]<=stop_price[i], we trade units that we have as market order.
        else if (store$cl[store$iter-1,i]>Bstop_price[i] && store$cl[store$iter,i]<=Bstop_price[i]
                 ||store$cl[store$iter,i]>=Sstop_price[params$series[i]] && currentPos[i]>=0){
          pos_M[params$series[i]] <- -currentPos[params$series[i]]

        }
      }
      if(i == 1 || i == 5 || i == 7){
        #-------------------------TMA Strategy----------------------#
        # get positioin sizing. We calculate the position size with the close price
        #get a list:each column stores returns for each series and calculate the mean value for each series
        CloseDiffs <- diff(store$cl)
        absCloseDiffs    <- matrix(abs(CloseDiffs),ncol = length(params$series),byrow=TRUE)

        # Calculate the column means for non-zero elements and find the largest mean value
        avgAbsDiffs <- apply(absCloseDiffs, 2, function(x) mean(x[x > 0]))

        #calculate position size
        #params$riskPortion:The maximum amount of money you are willing to lose on a single trade.
        #params$riskPerShare:The risk per share as a multiple of the average absolute difference.
        #split total investment for TMA strategy
        #balance=info$balance*params$moneyRate

        posSizes <- round((params$riskPortion*params$moneyRate*1000000)/(params$riskPerShare*avgAbsDiffs))

        ##set stop loss: params$stopRatio
        stopRate <- params$stopRatio

        #calculate the stop loss price level for long position
        #find the max and min close price in past days(medium lookback)
        highestPrice <- tail(cummax(store$cl[(store$iter-params$lookbacks$medium):store$iter,i]),1)
        maxStopLoss_price <- (1-stopRate) * highestPrice

        lowestPrice <- tail(cummin(store$cl[(store$iter-params$lookbacks$medium):store$iter,i]),1)
        minStopLoss_price <- (1+stopRate) * lowestPrice

        # ###get TMA ratio
        TMA_list = list(short = 0, medium = 0, long = 0)
        # 
        for (m in 1 : length(params$lookbacks)){
          #calculate SMA for each lookbacks
          sma <- SMA(store$cl[1:store$iter,i], n = params$lookbacks[[m]])
          TMA_list [m] = as.numeric(sma[length(sma)])
        }
        #TMA_list <- getTMAIndicators(params$lookbacks,store$cl[1:store$iter,i])

        ###get trade signal and position according to TMA ratio
        if (TMA_list$short < TMA_list$medium && TMA_list$medium < TMA_list$long){
          pos_long[params$series[i]] <- 1 * posSizes[i]
        }
        else if (TMA_list$short > TMA_list$medium && TMA_list$medium > TMA_list$long){
          pos_short[params$series[i]] <- -1 * posSizes[i]
        }

        # Exit Market Condition
        if( (pos_long[params$series[i]] > 0 && store$cl[store$iter,i] <= maxStopLoss_price)
            || (pos_short[params$series[i]] < 0 && store$cl[store$iter,i] >= minStopLoss_price)){

          pos_M[params$series[i]] <- -currentPos[params$series[i]]
        }
      }
      
    }
  }
  
  spread <- sapply(1:length(newRowList),function(i)
    params$spreadPercentage * (newRowList[[i]]$High -newRowList[[i]]$Low))
  
  limitOrders1  <- pos_long # BUY LIMIT ORDERS
  limitPrices1  <- sapply(1:length(newRowList),function(i)
    newRowList[[i]]$Close - spread[i]/2)
  
  limitOrders2  <- pos_short# SELL LIMIT ORDERS
  limitPrices2  <- sapply(1:length(newRowList),function(i)
    newRowList[[i]]$Close + spread[i]/2)
  
  marketOrders <- pos_M
  
  return(list(store=store,marketOrders=pos_M,
              limitOrders1=limitOrders1,limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,limitPrices2=limitPrices2))
  
}

################# get TMA ratio ###########################
# getTMAIndicators <- function(loockbacks,prices){
#   
#   TMA_list = list(short = 0, medium = 0, long = 0)
#   
#   for (i in 1 : length(params$lookbacks)){
#     #calculate SMA for each lookbacks
#     sma <- SMA(prices, n = params$lookbacks[[i]])
#     TMA_list[i] = as.numeric(sma[length(sma)])
#   }
#   return(TMA_list)
# }


############ calculate indicators for turtle trade #################
getIndicatorsAndATR <- function(newRowList,series,store,periods){
  storeOfEnEx <- list(Min_En1=matrix(0,nrow=maxRows,ncol=length(series)),Max_En1=matrix(0,nrow=maxRows,ncol=length(series)), 
                      Min_En2=matrix(0,nrow=maxRows,ncol=length(series)),Max_En2=matrix(0,nrow=maxRows,ncol=length(series)),
                      Min_Ex=matrix(0,nrow=maxRows,ncol=length(series)),Max_Ex=matrix(0,nrow=maxRows,ncol=length(series)))
  
  for (i in 1:length(series)){
    storeOfEnEx$Min_En1[,i] <- runMin(store$Lo[,i], params$periods$En_1)
    storeOfEnEx$Max_En1[,i] <- runMax(store$Hi[,i], params$periods$En_1)
    
    storeOfEnEx$Min_En2[,i] <- runMin(store$Lo[,i], params$periods$En_2)
    storeOfEnEx$Max_En2[,i] <- runMax(store$Hi[,i], params$periods$En_2)
    
    storeOfEnEx$Min_Ex[,i] <- runMin(store$Lo[,i], params$periods$Ex_1)
    storeOfEnEx$Max_Ex[,i] <- runMax(store$Hi[,i], params$periods$Ex_1)
  }
  return(storeOfEnEx)
}
############################ intiate #######################
initHiStore  <- function(newRowList,series) {
  HiStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(HiStore)
}
initLoStore  <- function(newRowList,series) {
  LoStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(LoStore)
}
initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
initVolStore  <- function(newRowList,series) {
  VolStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(VolStore)
}
##################### update ##############################
updateHiStore <- function(HiStore, newRowList, series, iter){
  for (i in 1:length(series))
    HiStore[iter,i] <- as.numeric(newRowList[[series[i]]]$High)
  return(HiStore)
}
updateLoStore <- function(LoStore, newRowList, series, iter){
  for (i in 1:length(series))
    LoStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Low)
  return(LoStore)
}
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}
updateVolStore <- function(VolStore, newRowList, series, iter) {
  for (i in 1:length(series))
    VolStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Volume)
  return(VolStore)
}
############# initate and update store ####################
initStore <- function(newRowList,series) {
  return(list(iter=0,Hi=initHiStore(newRowList,series), Lo=initLoStore(newRowList,series), 
              cl=initClStore(newRowList,series), Vol=initVolStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series, params) {
  store$iter <- store$iter + 1
  store$Hi <- updateHiStore(store$Hi,newRowList,series,store$iter)
  store$Lo <- updateLoStore(store$Lo,newRowList,series,store$iter)
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)
  store$Vol <- updateVolStore(store$Vol,newRowList,series,store$iter)
  
  return(store)
}