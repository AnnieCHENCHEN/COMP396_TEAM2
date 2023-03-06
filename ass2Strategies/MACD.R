library(TTR)

maxRows <- 3100 


getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) 
  spread <- sapply(1:length(newRowList),function(i)
    params$spreadPercentage * (newRowList[[i]]$High -
                                 newRowList[[i]]$Low))
  limitPrices1  <- sapply(1:length(newRowList),function(i) 
    newRowList[[i]]$Close - spread[i]/2)
  limitPrices2  <- sapply(1:length(newRowList),function(i) 
    newRowList[[i]]$Close + spread[i]/2)
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  Trade <- updateTrade(Trade,store,params$series,store$iter)
  
  #initial all lists
  pos <- allzero
  pos1 <- allzero
  pos2 <-allzero
  pos3 <- allzero
  buyTrans <-allzero
  sellTrans <- allzero
  
  atr <- allzero
  agap <- allzero
  units <- allzero
  stop_price <- allzero
  target_price <- allzero
  
  
  if (store$iter > params$lookback) {
    
    for (i in 1:length(params$series)) {
      
      macd_data <- MACD(store$cl[1:store$iter,i],nFast = 12, nSlow = 26, nSig =9, percent = TRUE)
      DIFF <- macd_data[,1]
      DEA <- macd_data[,2]
      print(Trade)
      
      #Compare MACD line and signal line, get the position signal
      #Entry market conditions
      if (DIFF[store$iter]>0 && DEA[store$iter]>0 && DIFF[store$iter]>DEA[store$iter]
          && DIFF[store$iter-1]<DEA[store$iter-1]) {
        
        #buy,+1
        pos[params$series[i]] <- params$posSizes[params$series[i]]
        buyTrans[params$series[i]]<- Trade$BuyPrice[Trade$count,i] ##problem1
        
        
      }
      else if (DIFF[store$iter]<0 && DEA[store$iter]<0 && DIFF[store$iter]<DEA[store$iter]
               && DIFF[store$iter-1]>DEA[store$iter-1] && currentPos[i] !=0) {
        
        #sell, -1
        pos[params$series[i]] <- -params$posSizes[params$series[i]]
        sellTrans[params$series[i]] <- Trade$SellPrice[Trade$count,i] ##problem2
        
      }
      
      
      
      #Calculate ATR indicator. Use it in the stop loss
      object <- data.frame(High = store$h[1:store$iter,i],Low=store$l[1:store$iter,i],Close = store$cl[1:store$iter,i])
      atr[i] <- tail(ATR(object,n=10,maType = "EMA")[,2], n=1)
      
      #risk management part
    # if(last(buyTrans[params$series[i]]) !=0){
        # set stop loss price (stop loss)
      #  stop_price[params$series[i]] <- tail(buyTrans[params$series[i]],n=1)-params$multiple2*atr[i] ##problem3
        
        # set target price (profit target)
      #  target_price[params$series[i]] <- apply(sellTrans[params$series[i]], 2, max) + params$multiple1 * atr[i] ##problem4
     # }
      
      #account risk, got 30% of 10000000 in this strategy
      account_risk = params$riskRatio*params$moneyRatio*info$balance
      
      # Calculate position size based on account risk, stop loss distance, and profit target distance
      units[i] <- round(account_risk / params$multiple2*atr[i])
      
      
      #calculate position sizes for limit orders and market order 
      if(store$cl[store$iter,i]>stop_price[i] && store$cl[store$iter,i]< target_price[i]){
        
        if(pos[params$series[i]] ==1){
          pos1[params$series[i]] <- pos[params$series[i]]*units[i]
        }else if(pos[params$series[i]] == -1){
          pos2[params$series[i]] <- pos[params$series[i]]*units[i]
        }
        
      }
      else if (store$cl[store$iter-1,i]>stop_price[i] && store$cl[store$iter,i]<=stop_price[i]
               || store$cl[store$iter,i]>= target_price[i] && currentPos[i]>=0){ #if store$cl[store$iter,i]<=stop_price[i], we trade units that we have as market order. Exit market
        pos3[params$series[i]] <- -currentPos[i]
        
      }
      
    }
  }
  
  #set limit price & limit orders
  
  limitOrders1  <- pos1 # BUY LIMIT ORDERS
  
  limitOrders2  <- pos2# SELL LIMIT ORDERS
  
  return(list(store=store,marketOrders=pos3,
              limitOrders1=limitOrders1,limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,limitPrices2=limitPrices2))
}




#***********************init Function starts*************************
initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
initOpStore  <- function(newRowList,series) {
  OpStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(OpStore)
}
initHiStore  <- function(newRowList,series) {
  HiStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(HiStore)
}
initLoStore  <- function(newRowList,series) {
  LoStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(LoStore)
}
initVoStore  <- function(newRowList,series) {
  VoStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(VoStore)
}
initBuyPrice <- function(newRowList,series) {
  BuyPrice <- matrix(0,nrow=maxRows,ncol=length(series))
  return(BuyPrice)
}
initSellPrice <- function(newRowList,series) {
  SellPrice <- matrix(0,nrow=maxRows,ncol=length(series))
  return(SellPrice)
}
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),
              h = initHiStore(newRowList,series),l = initLoStore(newRowList,series),
              o = initOpStore(newRowList,series),v = initVoStore(newRowList,series)))
}
initTrade <- function(newRowList,series){
  return(list(count=0, BuyPrice=initBuyPrice(newRowList,series),
              SellPrice=initSellPrice(newRowList,series)))
}
#***********************init Function ends*************************





#***********************update Function starts*************************
updateOpStore <- function(OpStore, newRowList, series, iter) {
  for (i in 1:length(series))
    OpStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Open)
  return(OpStore)
}
updateHiStore <- function(HiStore, newRowList, series, iter) {
  for (i in 1:length(series))
    HiStore[iter,i] <- as.numeric(newRowList[[series[i]]]$High)
  return(HiStore)
}
updateLoStore <- function(LoStore, newRowList, series, iter) {
  for (i in 1:length(series))
    LoStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Low)
  return(LoStore)
}
updateVoStore <- function(VoStore, newRowList, series, iter) {
  for (i in 1:length(series))
    VoStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Volume)
  return(VoStore)
}
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$o <- updateOpStore(store$o,newRowList,series,store$iter)
  store$h <- updateHiStore(store$h,newRowList,series,store$iter)
  store$l <- updateLoStore(store$l,newRowList,series,store$iter)
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)
  store$v <- updateVoStore(store$v,newRowList,series,store$iter)
  return(store)
}
updateBuyPrice <- function(BuyPrice, LimitPrice1,newRowList, series, iter){
  for (i in 1:length(series))
    BuyPrice[iter,i] <- as.numeric(min(LimitPrice1[,i],newRowList[[series[i]]]$High))
  return(BuyPrice)
}
updateSellPrice <- function(SellPrice, LimitPrice2,newRowList, series, iter){
  for (i in 1:length(series))
    SellPrice[iter,i] <- as.numeric(max(LimitPrice2[,i],newRowList[[series[i]]]$Low))
   return(SellPrice)
}
updateTrade <- function(Trade,store,series,iter){
  Trade$count <-store$iter
  Trade$BuyPrice <-updateBuyPrice(Trade$BuyPrice,LimitPrice1,newRowList,series,Trade$count)
  Trade$SellPrice <-updateSellPrice(Trade$SellPrice,LimitPrice2,newRowList,series,Trade$count)
  return(Trade)
}
#***********************update Function ends*************************


