#turtle strategy

require(TTR)
maxRows <- 3100 # depends on the row number of series

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  store <- getIndicatorsAndATR(newRowList,series,store,params)
  
  marketOrders <- -currentPos
  pos <- allzero
  
  #Portfolio Parameters
  size = 0.01
  maxUnits = 4
  Units=0
  verbose=TRUE #存疑
  
  for(i in params$period[3]+2:length(params$series)){
    ClosePrice <- as.numeric(cl(store[i,])) #存疑
    UnitSize <- as.numeric(trunc(size * 100000000)/(store$N[i-1] * ClosePrice))
    
    if(pos[params$series[i]] == 0){
      # Initiate Long position
      if(as.numeric(store$Hi[i-1]) > as.numeric(store$Max_Entry2[i-2])){
        # addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, 
        #        TxnQty = UnitSize , TxnFees=0, verbose=verbose)
        N = as.numeric(store$N[i-1])
        # updateStrat(Portfolio=portfolio, Symbol=symbol, TxnDate = CurrentDate, PosUnitsQty = 1, 
        #             UnitSize = UnitSize, StopPrice = (ClosePrice-2*N), TxnPrice = ClosePrice, TxnN = N)
      }else{
        # Initiate Short position
        if(as.numeric(store$Lo[i-1]) < as.numeric(store$Min_Entry2[i-2])){
          # addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice,
          #        TxnQty = -UnitSize , TxnFees=0, verbose=verbose)
          N = as.numeric(store$N[i-1])
          # updateStrat(Portfolio=portfolio, Symbol = symbol, TxnDate = CurrentDate, 
          #             PosUnitsQty = Units, UnitSize = UnitSize, StopPrice = (ClosePrice +2*N), 
          #             TxnPrice = ClosePrice, TxnN = N)
        }
      }
    }else{ #  pos != 0
      # Position exits and stops 退出和止损条件   存疑
      # Stop 存疑
      if(( pos[params$series[i]] > 0 && ( as.numeric(store$Lo[i-1]) < as.numeric(store$Min_Entry1[i-2]) || store$Lo[i-1] < Stop )) || 
        ( pos[params$series[i]] < 0 && ( as.numeric(store$Hi[i-1]) > as.numeric(store$Max_Entry1[i-2]) || store$Hi[i-1] > Stop ))) {
        # addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, 
        #        TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0, verbose=verbose)
        N = as.numeric(store$N[i-1])
        # updateStrat(Portfolio = portfolio, Symbol = symbol, TxnDate = CurrentDate, 
        #             PosUnitsQty = 0, UnitSize = UnitSize, StopPrice = NA, 
        #             TxnPrice = ClosePrice, TxnN = N)
      }else{
        # Add to long position   买的加仓情况
        # Units, maxUnits, TxnPrice 存疑
        if( pos[params$series[i]] > 0 && Units < maxUnits && Hi(x[i-1,]) > ( TxnPrice + N * 0.5 )) {
          # addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, 
          #        TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0, verbose=verbose)
          N = as.numeric(store$N[i-1])
          # updateStrat(Portfolio = portfolio, Symbol = symbol, TxnDate = CurrentDate, 
          #             PosUnitsQty = Units+1, UnitSize = UnitSize, StopPrice = (ClosePrice-2*N), 
          #             TxnPrice = ClosePrice, TxnN = N)
        } else{
          # Add to short position  卖的加仓情况
          # Units, maxUnits, TxnPrice  存疑
          if( Posn < 0 && Units < maxUnits && Lo(x[i-1,])  < ( TxnPrice - N * 0.5 ) ) {
            # addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=Cl(x[i,]), 
            #        TxnQty = -UnitSize , TxnFees=0, verbose=verbose)
            N = as.numeric(store$N[i-1])
            # updateStrat(Portfolio=portfolio, Symbol=symbol, TxnDate = CurrentDate, 
            #             PosUnitsQty = Units+1, UnitSize = UnitSize, StopPrice = (ClosePrice+2*N), 
            #             TxnPrice = ClosePrice, TxnN = N)
          }
        }
      }
    }
    
  } # for循环终点
  

}
#存疑
getIndicatorsAndATR <- function(newRowList,series,store,params){
  
  store$Min_Entry1 <- runMin(store$Lo, params$periods[2])
  store$Max_Entry1 <- runMax(store$Hi, params$periods[2])
  
  store$Min_Entry2 <- runMin(store$Lo, params$periods[3])
  store$Max_Entry2 <- runMax(store$Hi, params$periods[3])
  
  store$Min_Exit <- runMin(store$Lo, params$periods[1])
  store$Max_Exit <- runMax(store$Hi, params$periods[1])
  
  store$N <- ATR(store[,c(2,3,4)], n=params$periods[2], maType=EMA, wilder=TRUE)[,'atr']
  
  return(store)
}

# to get high and low
initHiStore  <- function(newRowList,series) {
  HiStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(HiStore)
}
initLoStore  <- function(newRowList,series) {
  LoStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(LoStore)
}
#to get high price and low price of the stock
getHighprice <- function(HiStore, newRowList, series, iter){
  for (i in 1:length(series))
    HiStore[iter,i] <- as.numeric(newRowList[[series[i]]]$High)
  return(HiStore)
}
getLowprice <- function(LoStore, newRowList, series, iter){
  for (i in 1:length(series))
    LoStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Low)
  return(LoStore)
}
# for close price
initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}

initStore <- function(newRowList,series) {
  return(list(iter=0,Hi=initHiStore(newRowList,series), Lo=initLoStore(newRowList,series), 
              cl=initClStore(newRowList,series), Min_Entry1=initClStore(newRowList,series), 
              Max_Entry1=initClStore(newRowList,series), Min_Entry2=initClStore(newRowList,series),
              Max_Entry2=initClStore(newRowList,series), Min_Exit=initClStore(newRowList,series),
              Max_Exit=initClStore(newRowList,series), N=initClStore(newRowList,series)))
  #(list 里面存了iter代表天数和cl--一个matrix用来存close price)
}
#store里面存了close, high, low price, entry and exyt line
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$Hi <- getHighprice(store$Hi,newRowList,series,store$iter)
  store$Lo <- getLowprice(store$Lo,newRowList,series,store$iter)
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)

  return(store)
}


