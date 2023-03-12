#turtle strategy

require(TTR)
require(ggplot2)
maxRows <- 3100 # depends on the row number of series

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series, params$periods)
  
  marketOrders <- -currentPos
  pos_long <- allzero
  pos_short <- allzero
  pos_M <- allzero
  Units_long <- 0
  Units_short <- 0
  
  if(store$iter > params$periods[3]){
    
    storeOfEnEx <- getIndicatorsAndATR(newRowList,params$series,store,params$periods)
    
    for(i in 1:length(params$series)){
      
      store_data <- data.frame(high=store$Hi[,i], low=store$Lo[,i], close=store$cl[,i])
      N_value <- ATR(store_data, n=params$periods[2], maType=EMA, wilder=TRUE)[,'atr']
      
      UnitSize <- as.numeric(trunc((params$size * params$capitals * params$percentage)/(N_value[store$iter] * params$multi)))
      
      # Initiate Long position
      if(as.numeric(store$Hi[store$iter,i]) > as.numeric(storeOfEnEx$Max_En2[store$iter-1,i])){
        
        Units_long <- params$Units + 1
        pos_long[i] = Units_long * UnitSize
        N_long = as.numeric(N_value[store$iter])
        
        TxnPrice_L = as.numeric(store$cl[store$iter,i])
        StopPrice_L = TxnPrice_L - 5*N_long
        
      }else{
        # Initiate Short position
        if(as.numeric(store$Lo[store$iter,i]) < as.numeric(storeOfEnEx$Min_En2[store$iter-1,i])){
          
          Units_short <- params$Units - 1
          pos_short[i] = Units_short * UnitSize
          N_short = as.numeric(N_value[store$iter])
          
          TxnPrice_S = as.numeric(store$cl[store$iter,i])
          StopPrice_S = TxnPrice_S + 5*N_short
          
        }
      }
      
      # Position exits and stops 退出和止损条件  pos != 0 
      if(( pos_long[params$series[i]] > 0 && ( as.numeric(store$Lo[store$iter,i]) < as.numeric(storeOfEnEx$Min_En1[store$iter-1,i]) || store$Lo[store$iter,i] < StopPrice_L )) || 
         ( pos_short[params$series[i]] < 0 && ( as.numeric(store$Hi[store$iter,i]) > as.numeric(storeOfEnEx$Max_En1[store$iter-1,i]) || store$Hi[store$iter,i] > StopPrice_S ))) {
        # addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, 
        #        TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0, verbose=verbose)
        
        pos_M[i] = -currentPos[i]
        
        # updateStrat(Portfolio = portfolio, Symbol = symbol, TxnDate = CurrentDate, 
        #             PosUnitsQty = 0, UnitSize = UnitSize, StopPrice = NA, 
        #             TxnPrice = ClosePrice, TxnN = N)
      }else{
        # Add to long position   买的加仓情况
        if( pos_long[params$series[i]] > 0 && Units_long < params$maxUnits && store$Hi[store$iter,i] > ( TxnPrice_L + N_long * 3 )) {

          Units_long = params$Units + 2
          pos_long[i] = Units_long * UnitSize
          N_long = as.numeric(N_value[store$iter])

          TxnPrice_L = as.numeric(store$cl[store$iter,i])
          StopPrice_L = TxnPrice_L - 5*N_long

        } else{
          # Add to short position  卖的加仓情况
          if( pos_short[params$series[i]] > 0 && Units_short < params$maxUnits && store$Lo[store$iter,i] < ( TxnPrice_S - N_short * 3 )) {

            Units_short = params$Units - 2
            pos_short[i] = Units_short * UnitSize
            N_short = as.numeric(N_value[store$iter])

            TxnPrice_S = as.numeric(store$cl[store$iter,i])
            StopPrice_S = TxnPrice_S + 5*N_short

          }
        }
      }
      
    }# for循环终点
    
  }
  
  spread <- sapply(1:length(newRowList),function(i)
    params$spreadPercentage * (newRowList[[i]]$High - newRowList[[i]]$Low))
  
  limitOrders1  <- pos_long # BUY LIMIT ORDERS
  limitPrices1  <- sapply(1:length(newRowList),function(i) 
    newRowList[[i]]$Close - spread[i]/2)
  
  limitOrders2  <- pos_short # SELL LIMIT ORDERS
  limitPrices2  <- sapply(1:length(newRowList),function(i) 
    newRowList[[i]]$Close + spread[i]/2)
  
  return(list(store=store,marketOrders=pos_M,
              limitOrders1=limitOrders1,limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,limitPrices2=limitPrices2))
}

getIndicatorsAndATR <- function(newRowList,series,store,periods){
  storeOfEnEx <- list(Min_En1=matrix(0,nrow=maxRows,ncol=length(series)),Max_En1=matrix(0,nrow=maxRows,ncol=length(series)), 
                      Min_En2=matrix(0,nrow=maxRows,ncol=length(series)),Max_En2=matrix(0,nrow=maxRows,ncol=length(series)),
                      Min_Ex=matrix(0,nrow=maxRows,ncol=length(series)),Max_Ex=matrix(0,nrow=maxRows,ncol=length(series)))
  
  for(i in 1:length(series)){
    storeOfEnEx$Min_En1[,i] <- runMin(store$Lo[,i], params$periods[2])
    storeOfEnEx$Max_En1[,i] <- runMax(store$Hi[,i], params$periods[2])
    
    storeOfEnEx$Min_En2[,i] <- runMin(store$Lo[,i], params$periods[3])
    storeOfEnEx$Max_En2[,i] <- runMax(store$Hi[,i], params$periods[3])
    
    storeOfEnEx$Min_Ex[,i] <- runMin(store$Lo[,i], params$periods[1])
    storeOfEnEx$Max_Ex[,i] <- runMax(store$Hi[,i], params$periods[1])
  }
  return(storeOfEnEx)
}

# intiate
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
#update
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

initStore <- function(newRowList,series) {
  return(list(iter=0,Hi=initHiStore(newRowList,series), Lo=initLoStore(newRowList,series), 
              cl=initClStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series, params) {
  store$iter <- store$iter + 1
  store$Hi <- updateHiStore(store$Hi,newRowList,series,store$iter)
  store$Lo <- updateLoStore(store$Lo,newRowList,series,store$iter)
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)

  return(store)
}