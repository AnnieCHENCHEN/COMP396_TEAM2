##mean reversion strategy (with SMA)##
getOrders <- function(store, newRowList, currentPos, info, params) {
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  pos <- allzero #set pos is a zero vector
  
  if (is.null(store)) 
    store <- initStore(newRowList)
  else
    store <- updateStore(store, newRowList)
  
  #set market orders; assumes pos(all zero)
  marketOrders <- -currentPos; pos <- allzero
  
  #if (store$iter > params$lookback) {
    startIndex <-  store$iter - params$lookback #set start date
    #select close price in newRowList
    #for(i in 1:length(params$series)){ #在之后combine阶段才需要使用for遍历其他的series
      #cl <- newRowList[[i]]$Close #select close price in series i
       #}
    
 #}
  
  #market orders updated
  marketOrders <- -currentPos + pos
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
  
  
}

#calculate SMA !ask questions!
mySMA <- function(input, n, newRowlist) { # version requested in question
  input <- newRowList[[1]]$Close
  n <- 20

  ret <- vector(mode='numeric', length(input)) 
  print(length(input))
  
  ret[1:n-1] <- NA
  ret[n] <- sum(input[1:n])/n
  for (i in (n+1):length(input)) {
    ret[i] <- ret[i-1] + (input[i]-input[i-n])/n
  } 
}

#the function about "store"
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
