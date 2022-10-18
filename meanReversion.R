

maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- -currentPos; pos <- allzero
  
  if (store$iter > params$lookback) {
    startIndex <-  store$iter - params$lookback
    for (i in 1:length(params$series)) {
      cl <- newRowList[[params$series[i]]]$Close
      vl <- newRowList[[params$series[i]]]$Volume #? need to discuss
      
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
    }
  }
  marketOrders <- marketOrders + pos
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}
#for close price 
initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}
#for profit
initStore_Profit <- function(newRowList,series) {
  profit_store <- matrix(0,nrow=maxRows,ncol=length(series))
  return(profit_store)
}
updateStore_Profit <- function(profit_store, newRowList, series, iter) {
  for (i in 1:length(series))
    profit_store[iter,i] <- as.numeric(newRowList[[series[i]]]$Volume)
                              *as.numeric(newRowList[[series[i]]]$Close)
  return(profit_store)
}
#store = iter and close price and profit
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series), profit=initStore_Profit(newRowList,series)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)
  store$profit <- updateStore_Profit(store$profit,newRowList,series,store$iter)
  return(store)
}
