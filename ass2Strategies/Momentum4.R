# Momentum strategy (with TMA with stop loss)
# This strategy uses only marker order

maxRows <-3100

dataList <- getData(directory="PART1")

getOrders <- function(store, newRowList, currentPos, info, params) {
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  pos <- allzero
  posSizes <- allzero
  signal <- allzero

  
  if (is.null(store))
    store <- initStore(newRowList)  
  else
    store <- updateStore(store, newRowList)  
  
  ###get positioin sizing
  CloseDiffs <- lapply(dataList,function(x) diff(x$Close))
  absCloseDiffs    <- lapply(CloseDiffs,abs)
  avgAbsDiffs <- sapply(absCloseDiffs,mean,na.rm=TRUE)

  largestAvgAbsDiffs <- max(avgAbsDiffs)
  posSizes <- round(largestAvgAbsDiffs/avgAbsDiffs)
    
  if (store$iter > params$lookbacks$long) {  
    
    for(i in 1:length(params$series)){
      
      ###get TMA ratio
      tma_list = list(short = 0, medium = 0, long = 0)
      for (m in 1 : length(params$lookbacks)){
        #calculate SMA for each lookbacks
        sma <- SMA(store$cl[1:store$iter,i], n = params$lookbacks[[m]])
        tma_list [m] = as.numeric(sma[length(sma)])}
   
      ###get trade signal
      if (tma_list[[1]] != 'NA' && tma_list[[2]] != 'NA' && tma_list[[3]] != 'NA'){
        if (tma_list$short < tma_list$medium && tma_list$medium < tma_list$long){
          signal[params$series[i]] <- 1
        }
        else if (tma_list$short > tma_list$medium && tma_list$medium > tma_list$long){
          signal[params$series[i]] <- -1
        }else{
          signal[params$series[i]] <- 0
        }
      }
      
      ##get final pos
      if(i != 0){
        #CurrentClose <- last(store$cl[[i]])
        pos[params$series[i]] <- signal[params$series[i]] * posSizes[i]
      }
      #######
      stopRatio <- 0.1
      if (pos[params$series[i]] == 1){
        highestPrice <- 0
        highestPrice <- max(store$cl[store$iter-10:store$iter,i])
        maxStopLoss_price <- (1-stopRatio) * highestPrice
        
        if (store$cl[store$iter,i] <= maxStopLoss_price){ 
          pos[params$series] <- allzero #exit market
          
        }}
    
      # short trade  
      else if (pos[params$series[i]] == -1){ 
        lowestPrice <- 0
        lowestPrice <- min(store$cl[store$iter-20:store$iter,i])
        minStopLoss_price <- (1+stopRatio) * lowestPrice
       
        if (store$cl[[i]] >= minStopLoss_price){ 
          pos[params$series] <- allzero #exit market
          
        }
      }
    }}
  
  marketOrders <- -currentPos + pos
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
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
