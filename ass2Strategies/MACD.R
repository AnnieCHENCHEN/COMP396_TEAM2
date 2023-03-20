#This strategy we use two Limit Orders and one Market Order
#This strategy is based on Momentum strategy with MACD
#The method that we use to calculate position sizing and stop loss is: ATR indicator
library(TTR)

maxRows <- 3100 

#parameters
#lookback=110,series=1:10,spreadPercentage=0.001,multiple=3,moneyRatio=0.3,riskRatio=0.01,initUnit=10

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
  units <- allzero
  new_units <- allzero
  
  
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
      Bstop_price[params$series[i]] <- store$cl[store$iter,i]-params$multiple*atr[i]
      Sstop_price[params$series[i]] <- store$cl[store$iter,i]+params$multiple*atr[i]
      
      
      #account risk, got 30% of 10000000 in this strategy
      account_risk = params$riskRatio*params$moneyRatio*10000000
      
      # Calculate position size based on account risk
      units[params$series[i]] <- round(account_risk / (params$multiple*atr[i]))
      
      if(units[params$series[i]]<=0 || units[params$series[i]]>store$v[store$iter,i]){
        new_units[params$series[i]]<- params$initUnit
      }else{
        new_units[params$series[i]]<- units[params$series[i]]
      }
      
      
      #Compare MACD line and signal line, get the position signal
      #Entry market conditions
      # DIFF & DEA >0 & DIFF>DEA & DIFF (yesterday) is smaller than DEA (yesterday)
      if (DIFF[store$iter]>0 && DEA[store$iter]>0 && DIFF[store$iter]>DEA[store$iter]
          && DIFF[store$iter-1]<DEA[store$iter-1]) {
        
        #sell,signal= -1, position sizes= -1*units
        pos1[params$series[i]] <- -1*new_units[params$series[i]]
        
        
      }
      else if (DIFF[store$iter]<0 && DEA[store$iter]<0 && DIFF[store$iter]<DEA[store$iter]
               && DIFF[store$iter-1]>DEA[store$iter-1] && currentPos[i] !=0) {
              #DIFF & DEA <0 & DIFF<DEA & DIFF (yesterday) is bigger than DEA (yesterday) & currentPos has units
        
        #buy, signal= +1, position sizes= 1*units
        pos2[params$series[i]] <- 1*new_units[params$series[i]]
        
      }
      
      
      else if (store$cl[store$iter-1,i]>Bstop_price[i] && store$cl[store$iter,i]<=Bstop_price[i]
               ||store$cl[store$iter-1,i]<Sstop_price[params$series[i]] && store$cl[store$iter,i]>=Sstop_price[params$series[i]] 
               && currentPos[i]>=0){ #Exit market. When close price <= buy stop price or close price >= sell stop price, we trade our currentPos using market order
        pos3[params$series[i]] <- -currentPos[params$series[i]]
        
      }
      
    }
  }
  
  #set limit price & limit orders
  spread <- sapply(1:length(newRowList),function(i)
    params$spreadPercentage * (newRowList[[i]]$High -newRowList[[i]]$Low))
  
  limitOrders1  <- pos2 # BUY LIMIT ORDERS
  limitPrices1  <- sapply(1:length(newRowList),function(i) 
    newRowList[[i]]$Close - spread[i]/2)
  
  limitOrders2  <- pos1# SELL LIMIT ORDERS
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
              v = initVoStore(newRowList,series)))
}
#***********************init Function ends*************************





#***********************update Function starts*************************
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
  store$h <- updateHiStore(store$h,newRowList,series,store$iter)
  store$l <- updateLoStore(store$l,newRowList,series,store$iter)
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)
  store$v <- updateVoStore(store$v,newRowList,series,store$iter)
  return(store)
}
#***********************update Function ends*************************


