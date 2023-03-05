# Momentum strategy (with TMA with stop loss)
# This strategy uses only marker order
#data 5
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
        pos[i] <- getPosSignFromTMA(TMA_List) * getPosSize(store,info,params)
       
      }
     
    }
  }
 
  marketOrders <- -currentPos + pos
 
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}
checkE01 <- function(prices, lookbacks) {
  na <- c(names(lookbacks))
  if ("short" %in% na & "medium" %in% na & "long" %in% na){
    return(FALSE)
  }
  # Return FALSE if lookbacks contains named elements short, medium, and long
  # otherwise return TRUE to indicate an error
  TRUE
}
checkE02 <- function(prices, lookbacks) {
  if(is.integer(lookbacks$short) & is.integer(lookbacks$medium) & is.integer(lookbacks$long)){
    return(FALSE)
  }
  # Return FALSE if all the elements of lookbacks are integers (as in the R
  # data type) otherwise return TRUE to indicate an error
  TRUE
}
checkE03 <- function(prices, lookbacks) {
  if (lookbacks$short < lookbacks$medium && lookbacks$medium < lookbacks$long){
    return(FALSE)
  }
  # Return FALSE if lookbacks$short < lookbacks$medium < lookbacks$long
  # otherwise return TRUE to indicate an error
  TRUE
}
checkE04 <- function(prices, lookbacks) {
  if (is.xts(prices)){
    return(FALSE)
  }
  # Return FALSE if prices is an xts object, otherwise return TRUE to
  # indicate an error
  TRUE
}
checkE05 <- function(prices, lookbacks) {
  if (nrow(prices) >= as.numeric(lookbacks$long)){
    return(FALSE)
  }
  # Return FALSE if prices has enough rows to getTMA otherwise return TRUE
  # to indicate an error
  TRUE
}
checkE06 <- function(prices, lookbacks) {
  pn <- c(names(prices))
  if ("Close" %in% pn){
    return(FALSE)
  }
  # Return FALSE if prices contains a column called "Close" otherwise return
  # TRUE to indicate an error
  TRUE
}
###############################################################################
# You should not edit allChecks
atLeastOneError <- function(prices, lookbacks) {
  # return TRUE if any of the error checks return TRUE
  ret <- FALSE
  ret <- ret | checkE01(prices,lookbacks)
  ret <- ret | checkE02(prices,lookbacks)
  ret <- ret | checkE03(prices,lookbacks)
  ret <- ret | checkE04(prices,lookbacks)
  ret <- ret | checkE05(prices,lookbacks)
  ret <- ret | checkE06(prices,lookbacks)
  return(ret)
}
###############################################################################
getTMA <- function(prices, lookbacks, with_checks=FALSE) {
 
  # prices and lookbacks should pass (return FALSE) when used with
  # the 6 checks, as tested in the following call to allChecks that
  # you should not edit
  if (with_checks)
    if (atLeastOneError(close_prices, lookbacks))
      stop('At least one of the errors E01...E06 occured')
 
 
  ret = list(short = 0, medium = 0, long = 0)
  for (i in 1 : length(lookbacks)){
   
    #calculate SMA for each lookbacks
    sma <- SMA(prices$Close, n = lookbacks[[i]])
    ret [i] = as.numeric(sma[length(sma)])
   
   
  }
  # You need to replace the assignment to ret so that the returned object:
  #    - is a list
  #    - has the right names (short, medium, long), and
  #    - contains numeric and not xts objects
  #    - and contains the correct moving average values, which should
  #      have windows of the correct sizes that all end in the
  #      same period, be the last row of prices
 
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
#question! we use kelly formula
getPosSize <- function(store,info,params) {
  kePos <- NULL
  for(n in 1:length(params$series)){
   
  currentClose <- last(store$cl[n])
  returns <- diff(log(store$cl[n]))
  expReturn <- mean(returns)
  expVolatility <- sd(returns)
  p <-mean(returns > 0)
  q <- 1-p
  keRatio <- p - q / p
  #keRatio <- kelly(expReturn, expVolatility)
  fraction <- 0.33
  kePos <- round(fraction*keRatio*info$balance/currentClose)
  }
  return(kePos)
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
