# Momentum strategy (with TMA with stop loss)
# This strategy uses only marker order
#data 5

library(PerformanceAnalytics)
library(quantmod)
source("~/backtester_v5.7/backtester_v5.7/framework/backtester.R")

maxRows <- 3100 # used to initialize a matrix to store closing prices

getOrders <- function(store, newRowList, currentPos, info, params) {
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  pos <- allzero
 
  if (is.null(store))
    store <- initStore(newRowList)  
  else
    store <- updateStore(store, newRowList)  
 

  if (store$iter > params$lookbacks$long) {  
    for(i in params$series){
      if(i != 0){
        TMA_List <- getTMA(store$cl[[i]], params$lookbacks)  #return a list
        CurrentClose <- last(store$cl[[i]])
        pos[params$series[i]] <- getPosSignFromTMA(TMA_List) * getPosSize(current_close)
       
      }
     
    }
  }
 
  marketOrders <- -currentPos + pos

 
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}

# checkE01 <- function(prices, lookbacks) {
#   na <- c(names(lookbacks))
#   if ("short" %in% na & "medium" %in% na & "long" %in% na){
#     return(FALSE)
#   }
#   # Return FALSE if lookbacks contains named elements short, medium, and long
#   # otherwise return TRUE to indicate an error
#   TRUE
# }
# checkE02 <- function(prices, lookbacks) {
#   if(is.integer(lookbacks$short) & is.integer(lookbacks$medium) & is.integer(lookbacks$long)){
#     return(FALSE)
#   }
#   # Return FALSE if all the elements of lookbacks are integers (as in the R
#   # data type) otherwise return TRUE to indicate an error
#   TRUE
# }
# checkE03 <- function(prices, lookbacks) {
#   if (lookbacks$short < lookbacks$medium && lookbacks$medium < lookbacks$long){
#     return(FALSE)
#   }
#   # Return FALSE if lookbacks$short < lookbacks$medium < lookbacks$long
#   # otherwise return TRUE to indicate an error
#   TRUE
# }
# checkE04 <- function(prices, lookbacks) {
#   if (is.xts(prices)){
#     return(FALSE)
#   }
#   # Return FALSE if prices is an xts object, otherwise return TRUE to
#   # indicate an error
#   TRUE
# }
# checkE05 <- function(prices, lookbacks) {
#   if (nrow(prices) >= as.numeric(lookbacks$long)){
#     return(FALSE)
#   }
#   # Return FALSE if prices has enough rows to getTMA otherwise return TRUE
#   # to indicate an error
#   TRUE
# }
# checkE06 <- function(prices, lookbacks) {
#   pn <- c(names(prices))
#   if ("Close" %in% pn){
#     return(FALSE)
#   }
#   # Return FALSE if prices contains a column called "Close" otherwise return
#   # TRUE to indicate an error
#   TRUE
# }
###############################################################################
# You should not edit allChecks
#
# atLeastOneError <- function(prices, lookbacks) {
#   # return TRUE if any of the error checks return TRUE
#   ret <- FALSE
#   ret <- ret | checkE01(prices,lookbacks)
#   ret <- ret | checkE02(prices,lookbacks)
#   ret <- ret | checkE03(prices,lookbacks)
#   ret <- ret | checkE04(prices,lookbacks)
#   ret <- ret | checkE05(prices,lookbacks)
#   ret <- ret | checkE06(prices,lookbacks)
#   return(ret)
# }

###############################################################################

# getTMA <- function(prices, lookbacks, with_checks=FALSE) {
#  
#   # prices and lookbacks should pass (return FALSE) when used with
#   # the 6 checks, as tested in the following call to allChecks that
#   # you should not edit
#   if (with_checks)
#     if (atLeastOneError(close_prices, lookbacks))
#       stop('At least one of the errors E01...E06 occured')
#  
#   ret = list(short = 0, medium = 0, long = 0)
#   for (i in 1 : length(lookbacks)){
#    
#     #calculate SMA for each lookbacks
#     sma <- SMA(prices$Close, n = lookbacks[[i]])
#     ret [i] = as.numeric(sma[length(sma)])
#   }
#   return(ret)
# }

getTMA <- function(store, params) {
  ret <- 0
  sma <- 0
  ret <- list(short = 0, medium = 0, long =0)
 
  for (i in 1:length(params$lookbacks)) {
    sma[i] <- as.numeric(last(SMA(store$Close, n = lookbacks[[i]])))
    ret[[i]] <- sma[i]
  }
  return(ret)
}

getPosSignFromTMA <- function(tma_list) {
 
  if (tma_list[[1]] != 'NA' && tma_list[[2]] != 'NA' && tma_list[[3]] != 'NA'){
    if (tma_list$short < tma_list$medium && tma_list$medium < tma_list$long){
      return(-1)
    }
    else if (tma_list$short > tma_list$medium && tma_list$medium > tma_list$long){
      return(1)
    }else{
      return(0)
    }
  }
}


#question! we don't know how to set position size
getPosSize <- function(current_close,constant=5000) {
  RoundDown_int <- floor(constant/current_close)
  return(RoundDown_int)
}

# getStopLoss <- function(){
#  
#   lossratio = 0.10
#   highest_price = max(close price)(from day 1 to day i); lowest price = min(close price)(from day 1 to day i)
#   max stop loss price = (1-0.1) * highest price
#   min stop loss price = (1+0.1) * lowest price
# }

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
