# Mean reversion strategy (with BBands with proit target)
# This strategy uses only marker orders

maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

library(xts)
library(TTR)

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- -currentPos #close all positions
  pos <- allzero #set pos as a zero vector
  profit <- 0 #inital profit=0
  
  
  
  if (store$iter > params$lookback) {
    startIndex <-  store$iter - params$lookback 
    for (i in 1:length(params$series)) {
      cl <- newRowList[[params$series[i]]]$Close
      bbands <- last(BBands(store$cl[startIndex:store$iter,i],
                            n=params$lookback,sd=params$sdParam))
      if (cl < bbands[,"dn"]) {
        # if close is relatively low go long (i.e., contrarian type)
        pos[params$series[i]] <- params$posSizes[params$series[i]]
      }
      else if (cl > bbands[,"up"]) {
        # if close is relatively high go short (again, contrarian type)
        pos[params$series[i]] <- -params$posSizes[params$series[i]]
      }
    
      # we want to sum up the profit (1 to i) of the whole trading, we use this profit to compare with our profit target 
      # which is profit target =50000
      profit <- sapply(info$netWorth[i], sum, simplify = TRUE) 
      
      # profit < profit target, we will contiune this strategy and this "if" command
      if (profit < params$profit_target) {
          next
      }
      # profit >= params$profit_target, we will exit the market and stop trading
        else
          pos[params$series[i]] <- 0  #update current position
          break
      }

  }

  marketOrders <- marketOrders + pos
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))

}

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
  return(list(iter=0,cl=initClStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  return(store)
}
