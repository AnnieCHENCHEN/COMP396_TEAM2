# Momentum strategy (with TMA with traling stop loss)
# This strategy uses only marker order

getOrders <- function(store, newRowList, currentPos, info, params) {
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  pos <- allzero
  stop_loss =0 #set stop loss=0
  
  if (is.null(store)) 
    store <- initStore(newRowList)  
  else
    store <- updateStore(store, newRowList)  
  

  if (store$iter > params$lookbacks$long) {   
    for(i in params$series){
      if(i != 0){
        TMA_List <- getTMA(store$cl[[i]], params$lookbacks)  #return a list
        CurrentClose <- last(store$cl[[i]])
        pos[i] <- getPosSignFromTMA(TMA_List) * getPosSize(CurrentClose)
      
      #traling stop loss
      #long，close price + trailing stop loss (100) = current close price
      # next trade close rice <= current close price，exit the makret (pos <-0),otherwise continue to trade
      if (pos[i] ==1){
        highestPrice = max(store$cl[[1:i]])
        maxStopLoss_price = (1-0.1) * highestPrice
        
        if (store$cl[[i]] <= maxStopLoss_price){ #
          pos[i] <- 0
          }else{ #store$cl[[i]] > maxStopLoss_price
            next #continue stay in a trade
          }
        
      }
        
      else if (pos[i] == -1){
        lowestPrice = min(store$cl[[1:i]])
        minStopLoss_price = (1+0.1) * lowestPrice
        
        if (store$cl[[i]] >= minStopLoss_price){ #
          pos[i] <- 0
        }else{ #store$cl[[i]] < minStopLoss_price
          next #continue stay in a trade
        } 
        
      }
      }
      
    }
  }

  marketOrders <- -currentPos + pos
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}

getTMA <- function(prices, lookbacks) {
  
  ret <- 0
  
  sma <- 0
  ret <- list(short = 0, medium = 0, long =0)
  for (i in 1:length(lookbacks)) {
    sma[i] <- as.numeric(last(SMA(prices$Close, n = lookbacks[[i]])))
    ret[[i]] <- sma[i]
  }
  return(ret)
}

getPosSignFromTMA <- function(tma_list) {

  if(tma_list[[1]] < tma_list[[2]] && tma_list[[2]] < tma_list[[3]])
    return(-1)
  else if(tma_list[[1]] > tma_list[[2]] && tma_list[[2]] > tma_list[[3]])
    return(1)
  else
    return(0)
}

getPosSize <- function(current_close,constant=5000) { 
  RoundDown_int <- floor(constant/current_close)
  return(RoundDown_int)
}


initClStore  <- function(newRowList) {
  clStore <- lapply(newRowList, function(x) x$Close)  
  return(clStore)
}
updateClStore <- function(clStore, newRowList) {
  clStore <- mapply(function(x,y) rbind(x,y$Close),clStore,newRowList,SIMPLIFY=FALSE) 
  return(clStore)  
}
initStore <- function(newRowList,series) {
  return(list(iter=1,cl=initClStore(newRowList))) 
}
updateStore <- function(store, newRowList) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList) 
  return(store)   
}
