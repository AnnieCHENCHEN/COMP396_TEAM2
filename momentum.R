
getOrders <- function(store, newRowList, currentPos, info, params) {
  
  ###########################################################################
  # You do not need to edit this next part of the code
  ###########################################################################
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  pos <- allzero
  
  if (is.null(store)) 
    store <- initStore(newRowList)  
  else
    store <- updateStore(store, newRowList)  
  ###########################################################################
  
  ###########################################################################
  # This next code section is the only one you
  # need to edit for getOrders
  #
  # The if condition is already correct:
  # you should only start computing the moving 
  # averages when you have enough (close) prices 
  # for the long moving average   
  ###########################################################################
  if (store$iter > params$lookbacks$long) {   
    # ENTER STRATEGY LOGIC HERE
    
    # remember to only consider the series in params$series
    
    # You will need to get the current_close
    # either from newRowList or from store$cl
    
    # You will also need to get prices 
    # from store$cl
    
    # With these you can use getTMA, getPosSignFromTMA
    # and getPosSize to assign positions to the vector pos
    for(i in params$series){
      if(i != 0){
        TMA_List <- getTMA(store$cl[[i]], params$lookbacks)  #return a list
        CurrentClose <- last(store$cl[[i]])
        pos[i] <- getPosSignFromTMA(TMA_List) * getPosSize(CurrentClose)
      }
    }
  }
  #Edit tailing stop loss
  ###########################################################################
  
  ###########################################################################
  # You do not need to edit the rest of this function
  ###########################################################################
  marketOrders <- -currentPos + pos
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}

getTMA <- function(prices, lookbacks) {
  
  # prices and lookbacks should pass (return FALSE) when used with
  # the 6 checks, as tested in the following call to allChecks that 
  # you should not edit
  if (with_checks)
    if (atLeastOneError(close_prices, lookbacks))
      stop('At least one of the errors E01...E06 occured')
  
  ret <- 0
  
  sma <- 0
  ret <- list(short = 0, medium = 0, long =0)
  for (i in 1:length(lookbacks)) {
    sma[i] <- as.numeric(last(SMA(prices$Close, n = lookbacks[[i]])))
    ret[[i]] <- sma[i]
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
  # This function takes a list of numbers tma_list with three elements 
  # called short, medium, and long, which correspond to the SMA values for 
  # a short, medium and long lookback, respectively.
  
  # Note that if both this function and getTMA are correctly implemented 
  # then the following should work with correct input arguments:
  # getPositionFromTMA(getTMA(prices,lookbacks))
  
  # This function should return a single number that is:
  #       -1 if the short SMA < medium SMA < long SMA
  #        1 if the short SMA > medium SMA > long SMA
  #        0 otherwise
  if(tma_list[[1]] < tma_list[[2]] && tma_list[[2]] < tma_list[[3]])
    return(-1)
  else if(tma_list[[1]] > tma_list[[2]] && tma_list[[2]] > tma_list[[3]])
    return(1)
  else
    return(0)
}

getPosSize <- function(current_close,constant=5000) { 
  # This function should return (constant divided by current_close) 
  # rounded down to the nearest integer
  RoundDown_int <- floor(constant/current_close)
  return(RoundDown_int)
}

###############################################################################
# The functions below do NOT need to be edited
###############################################################################
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
