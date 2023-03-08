library(TTR)

maxRows <- 3100 


getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) 
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  #initial all lists
  pos1 <- allzero
  pos2 <-allzero
  pos3 <- allzero
  Bstop_price <-allzero
  Sstop_price <- allzero
  
  atr <- allzero
  agap <- allzero
  units <- allzero
  
  
  if (store$iter > params$lookback) {
    
    for (i in 1:length(params$series)) {
      
      macd_data <- MACD(store$cl[1:store$iter,i],nFast = 12, nSlow = 26, nSig =9, percent = TRUE)
      DIFF <- macd_data[,1]
      DEA <- macd_data[,2]
      
      #Calculate ATR indicator. Use it in the stop loss
      object <- data.frame(High = store$h[1:store$iter,i],Low=store$l[1:store$iter,i],Close = store$cl[1:store$iter,i])
      atr[i] <- tail(ATR(object,n=10,maType = "EMA")[,2], n=1)
      
      #risk management part 
      # set stop loss price for long and short
      Bstop_price[params$series[i]] <- store$cl[store$iter-1,i]-params$multiple*atr[i]
      Sstop_price[params$series[i]] <- store$cl[store$iter-1,i]+params$multiple*atr[i]
      
      
      #account risk, got 30% of 10000000 in this strategy
      account_risk = params$riskRatio*params$moneyRatio*info$balance
      
      # Calculate position size based on account risk
      units[params$series[i]] <- round(account_risk / params$multiple*atr[i])
      
      if(units[params$series[i]]<=0 || units[params$series[i]]>store$v[store$iter,i]){
        units[params$series[i]]<- 50
      }else{
        units[params$series[i]]<-units[params$series[i]]
      }
      
      #Compare MACD line and signal line, get the position signal
      #Entry market conditions
      if (DIFF[store$iter]>0 && DEA[store$iter]>0 && DIFF[store$iter]>DEA[store$iter]
          && DIFF[store$iter-1]<DEA[store$iter-1]) {
        
        #buy,+1
        pos1[params$series[i]] <- 1*units[params$series[i]]
        
        
      }
      else if (DIFF[store$iter]<0 && DEA[store$iter]<0 && DIFF[store$iter]<DEA[store$iter]
               && DIFF[store$iter-1]>DEA[store$iter-1] && currentPos[i] !=0) {
        
        #sell, -1
        pos2[params$series[i]] <- -1*units[params$series[i]]
        
      }
      
      
      else if (store$cl[store$iter-1,i]>Bstop_price[i] && store$cl[store$iter,i]<=Bstop_price[i]
               ||store$cl[store$iter,i]>=Sstop_price[params$series[i]] && currentPos[i]>=0){ #if store$cl[store$iter,i]<=stop_price[i], we trade units that we have as market order. Exit market
        pos3[params$series[i]] <- -currentPos[params$series[i]]
        
      }
      
    }
  }
  
  #set limit price & limit orders
  spread <- sapply(1:length(newRowList),function(i)
    params$spreadPercentage * (newRowList[[i]]$High -newRowList[[i]]$Low))
  
  limitOrders1  <- pos1 # BUY LIMIT ORDERS
  limitPrices1  <- sapply(1:length(newRowList),function(i) 
    newRowList[[i]]$Close - spread[i]/2)
  
  limitOrders2  <- pos2# SELL LIMIT ORDERS
  limitPrices2  <- sapply(1:length(newRowList),function(i) 
    newRowList[[i]]$Close + spread[i]/2)
  
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
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),
              h = initHiStore(newRowList,series),l = initLoStore(newRowList,series),
              o = initOpStore(newRowList,series),v = initVoStore(newRowList,series)))
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
#***********************update Function ends*************************


