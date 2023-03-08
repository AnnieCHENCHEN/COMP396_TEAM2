# Momentum strategy (with TMA with stop loss)
# This strategy uses only marker order

maxRows <-3100

#dataList <- getData(directory="PART1")

getOrders <- function(store, newRowList, currentPos, info, params) {
  # used for initializing vectors
  allzero  <- rep(0,length(newRowList)) 
  buyPos <- allzero
  sellPos <- allzero
  posSizes <- allzero


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
        avgAbsDiffs <- colMeans(absCloseDiffs, na.rm = TRUE)
      largestAvgAbsDiffs <- max(avgAbsDiffs)
      
      posSizes <- round(largestAvgAbsDiffs/avgAbsDiffs)
      
      ###get TMA ratio
      tma_list = list(short = 0, medium = 0, long = 0)
      for (m in 1 : length(params$lookbacks)){
        #calculate SMA for each lookbacks
        sma <- SMA(store$cl[1:store$iter,i], n = params$lookbacks[[m]])
        tma_list [m] = as.numeric(sma[length(sma)])}
   
      ##set stop loss: params$stopRatio
      #stopRatio <- 0.1
      stopRate <- params$stopRatio
      
      ###get trade signaland position according to TMA ratio
      if (tma_list[[1]] != 'NA' && tma_list[[2]] != 'NA' && tma_list[[3]] != 'NA'){
        if (tma_list$short < tma_list$medium && tma_list$medium < tma_list$long){
          buyPos[params$series[i]] <- 1 * posSizes[i]
          
          #for long position
          #if (pos[params$series[i]] > 0){
            #calculate the stop loss price level for long position
            highestPrice <- 0
            highestPrice <- tail(cummax(store$cl[(store$iter-params$lookbacks$medium):store$iter,i]),1)
            maxStopLoss_price <- (1-stopRate) * highestPrice
            
            #set condition: if price reaches stop loss level, then exit the market
            if (store$cl[store$iter,i] <= maxStopLoss_price){ 
              #pos[params$series] <- allzero #exit market
              marketPos <- -currentPos
            } 
            #}
        }
        else if (tma_list$short > tma_list$medium && tma_list$medium > tma_list$long){
          sellPos[params$series[i]] <- -1 * posSizes[i]
          # for short position 
          #else if (pos[params$series[i]] < 0){ 
            #calculate the stop loss price level for short position
            lowestPrice <- 0
            lowestPrice <- tail(cummin(store$cl[(store$iter-params$lookbacks$medium):store$iter,i]),1)
            minStopLoss_price <- (1+stopRate) * lowestPrice
            
            #set condition: if price reaches stop loss level, then exit the market
            if (store$cl[[i]] >= minStopLoss_price){ 
              #pos[params$series] <- 0 #exit market
              marketPos <- -currentPos

            }
          #}
        }
      }
      
    
     
    }}

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
