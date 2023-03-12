# Momentum strategy (with TMA with stop loss)
# This strategy uses only marker order

#"Momentum2" =list(lookbacks=list(short=as.integer(15),medium=as.integer(40),long=as.integer(90)),moneyRate=0.3
#                  ,series=1:10,stopRatio=0.3,riskPortion=0.001,riskPerShare=4,spreadPercentage=0.001)

maxRows <-3100

getOrders <- function(store, newRowList, currentPos, info, params) {
  # used for initializing vectors
  allzero  <- rep(0,length(newRowList)) 
  buyPos <- allzero
  sellPos <- allzero
  posSizes <- allzero
  marketPos <- allzero
  
  
  if (is.null(store))
    store <- initStore(newRowList)  
  else
    store <- updateStore(store, newRowList)  
  

  if (store$iter > params$lookbacks$long) {  
    
    for(i in 1:length(params$series)){
      
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
        balance=info$balance*params$moneyRate
        
      posSizes <- round((params$riskPortion*balance)/(params$riskPerShare*avgAbsDiffs))
    
      ###get TMA ratio
      tma_list = list(short = 0, medium = 0, long = 0)
      for (m in 1 : length(params$lookbacks)){
        #calculate SMA for each lookbacks
        sma <- SMA(store$cl[1:store$iter,i], n = params$lookbacks[[m]])
        tma_list [m] = as.numeric(sma[length(sma)])}
   
      ###get trade signal and position according to TMA ratio
      if (tma_list[[1]] != 'NA' && tma_list[[2]] != 'NA' && tma_list[[3]] != 'NA'){
        if (tma_list$short < tma_list$medium && tma_list$medium < tma_list$long){
          buyPos[params$series[i]] <- 1 * posSizes[i]
        }
        else if (tma_list$short > tma_list$medium && tma_list$medium > tma_list$long){
          sellPos[params$series[i]] <- -1 * posSizes[i]
        }
      }
      
      
      ##set stop loss: params$stopRatio
      stopRate <- params$stopRatio
      
      #calculate the stop loss price level for long position
      #find the max and min close price in past days(medium lookback)
      highestPrice <- tail(cummax(store$cl[(store$iter-params$lookbacks$medium):store$iter,i]),1)
      maxStopLoss_price <- (1-stopRate) * highestPrice
      
      lowestPrice <- tail(cummin(store$cl[(store$iter-params$lookbacks$medium):store$iter,i]),1)
      minStopLoss_price <- (1+stopRate) * lowestPrice
      
      #for long position and short position
      if (buyPos[params$series[i]] != 0 && store$cl[store$iter,i] <= maxStopLoss_price && currentPos !=0 
          ||sellPos[params$series[i]] != 0 && store$cl[store$iter,i] >= minStopLoss_price && currentPos !=0){
        #set condition: if price reaches stop loss level, then exit the market
        marketPos[params$series[i]] <- -currentPos[params$series[i]]
      }
    }  
  }

  #set limit price & limit orders
  spread <- sapply(1:length(newRowList),function(i)
    params$spreadPercentage * (newRowList[[i]]$High -newRowList[[i]]$Low))
  
  limitOrders1  <- buyPos # BUY LIMIT ORDERS
  limitPrices1  <- sapply(1:length(newRowList),function(i) 
    newRowList[[i]]$Close - spread[i]/2)
  
  limitOrders2  <- sellPos# SELL LIMIT ORDERS
  limitPrices2  <- sapply(1:length(newRowList),function(i) 
    newRowList[[i]]$Close + spread[i]/2)
  
 
  return(list(store=store,marketOrders=marketPos,
              limitOrders1=limitOrders1,limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,limitPrices2=limitPrices2))
}



initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows, ncol=length(params$series))  
  return(clStore)
}
initHistore <-function(newRowList,series){
  HiStore <- matrix(0,nrow=maxRows, ncol=length(params$series))
  return(HiStore)
}
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,params$series)
              ,hi=initHistore(newRowList,params$series)))
}
updateClStore <- function(clStore, newRowList,series,iter) {
  for(i in 1:length(params$series)){
    clStore[iter,i] <- as.numeric(newRowList[[params$series[i]]]$Close)
  }
  return(clStore)  
}
updateHiStore <- function(HiStore, newRowList,series,iter){
  for(i in 1:length(params$series)){
    HiStore[iter,i] <- as.numeric(newRowList[[params$series[i]]]$High)
  }
  return(HiStore)  
}
updateStore <- function(store, newRowList,series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,params$series,store$iter)
  store$hi <- updateClStore(store$hi,newRowList,params$series,store$iter)
  return(store)  
}
