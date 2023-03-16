#turtle strategy
#
#Parameters : "turtle_trade"=list(periods=list(Ex_1=10,En_1=20, En_2=55), series=3, size=0.01, moneyRatio=0.02,
#capi_Ratio=0.3, spreadPercentage=0.001, multi=4, Multi_N=5)
#
#####periods, moneyRatio, multi, Multi_N, series, capi_Ratio 需要优化
#####Multi_N 的优化范围从5开始，不能低于5
#####原先params$Units 已删除
#####做多和做空的位置已调换好
#
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
  
  if(store$iter > params$periods$En_2){
    
    storeOfEnEx <- getIndicatorsAndATR(newRowList,params$series,store,params$periods)
    
    for(i in 1:length(params$series)){
      
      store_data <- data.frame(high=store$Hi[,i], low=store$Lo[,i], close=store$cl[,i])
      N_value <- ATR(store_data, n=params$periods$En_1, maType=EMA, wilder=TRUE)[,'atr']
      UnitSize <- as.numeric(trunc((params$size * 1000000 * params$capi_Ratio* params$moneyRatio)/(N_value[store$iter] * params$multi)))
      
      # Initiate SHORT position
      if(as.numeric(store$Hi[store$iter,i]) > as.numeric(storeOfEnEx$Max_En2[store$iter-1,i])){
        
        pos_short[params$series[i]] = -1 * UnitSize
        
        N_short = as.numeric(N_value[store$iter])
        TxnPrice_S = as.numeric(store$cl[store$iter,i])
        StopPrice_S = TxnPrice_S - params$Multi_N * N_short #退出/止损价格
        StopPrice_S2 = TxnPrice_S - (params$Multi_N-3) * N_short #平部分仓的价格
        
        # Add to SHORT position   
        if(store$Hi[store$iter,i] > ( TxnPrice_S + N_short * (params$Multi_N-2) )) {
          
          pos_short[params$series[i]] = -2 * UnitSize #加仓
        }
      }else{
        # Initiate LONG position
        if(as.numeric(store$Lo[store$iter,i]) < as.numeric(storeOfEnEx$Min_En2[store$iter-1,i])){
          
          pos_long[params$series[i]] = 1 * UnitSize
          
          N_long = as.numeric(N_value[store$iter])
          TxnPrice_L = as.numeric(store$cl[store$iter,i])
          StopPrice_L = TxnPrice_L + params$Multi_N*N_long #退出/止损价格
          StopPrice_L2 = TxnPrice_L + (params$Multi_N-3)*N_long #平部分仓的价格
          
          # Add to LONG position
          if(store$Lo[store$iter,i] < ( TxnPrice_L - N_long*(params$Multi_N-2) )) {
            
            pos_long[params$series[i]] = 2 * UnitSize#加仓
          }
        }
      }
      # Position exits and stops 退出和止损条件  pos != 0 
      #平部分仓
      if(( pos_short[params$series[i]] < 0 && (as.numeric(store$Lo[store$iter,i]) < as.numeric(storeOfEnEx$Min_En1[store$iter-1,i]) || as.numeric(store$Lo[store$iter,i]) < StopPrice_S2) ) ||
         ( pos_long[params$series[i]] > 0 && (as.numeric(store$Hi[store$iter,i]) > as.numeric(storeOfEnEx$Max_En1[store$iter-1,i]) || as.numeric(store$Hi[store$iter,i]) > StopPrice_L2) )) {
        # addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate,
        #        TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0, verbose=verbose)
        
        pos_M[params$series[i]] = trunc((-currentPos[i] / 2))
        # updateStrat(Portfolio = portfolio, Symbol = symbol, TxnDate = CurrentDate,
        #             PosUnitsQty = 0, UnitSize = UnitSize, StopPrice = NA,
        #             TxnPrice = ClosePrice, TxnN = N)
        #全部退出
        if((pos_short[params$series[i]] < 0 && store$Lo[store$iter,i] < StopPrice_S) || (pos_long[params$series[i]] > 0 && store$Hi[store$iter,i] > StopPrice_L)){
          
          pos_M[params$series[i]] = -currentPos[i]
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
    storeOfEnEx$Min_En1[,i] <- runMin(store$Lo[,i], params$periods$En_1)
    storeOfEnEx$Max_En1[,i] <- runMax(store$Hi[,i], params$periods$En_1)
    
    storeOfEnEx$Min_En2[,i] <- runMin(store$Lo[,i], params$periods$En_2)
    storeOfEnEx$Max_En2[,i] <- runMax(store$Hi[,i], params$periods$En_2)
    
    storeOfEnEx$Min_Ex[,i] <- runMin(store$Lo[,i], params$periods$Ex_1)
    storeOfEnEx$Max_Ex[,i] <- runMax(store$Hi[,i], params$periods$Ex_1)
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
