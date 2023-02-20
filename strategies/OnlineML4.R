library(stream)
library(partykit)
library(rpart)


maxRows <- 3100 # equal to nrow(data), can be smaller


getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) 
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  
  pos <- allzero
  
  s=7
 
  train_data <- data.frame(it=data.frame(it = seq(1, store$iter)), 
                          Close = store$cl[1:store$iter,s]) 
  model <- rpart(Close ~ .,data = train_data)
  new_instance <- data.frame(it =store$iter+1)
  predicted_close <- predict(model, new_instance)
  
  
  store <- updateStore(store, newRowList, params$series)
  
  real <- data.frame(it=data.frame(it = seq(1, store$iter)),
                     Close = store$cl[1:store$iter,s]) 

  if (predicted_close>real$Close[store$iter]&store$iter>1) {
    pos[params$series[s]] <- params$posSizes[params$series[s]]
  }
  else if (predicted_close<real$Close[store$iter]&store$iter>1){
    # if close is relatively high go long (again, trend following)
    pos[params$series[s]] <- -params$posSizes[params$series[s]]
  }
  else{
    pos[params$series[s]] <- 0
  }
  
  marketOrders <- pos
  # spread <- sapply(1:length(newRowList),function(i)
  #   params$spreadPercentage * (newRowList[[i]]$High -
  #                                newRowList[[i]]$Low))
  # 
  # limitOrders1  <- rep(1,length(newRowList)) # BUY LIMIT ORDERS
  # limitPrices1  <- sapply(1:length(newRowList),function(i) 
  #   newRowList[[i]]$Close - spread[i]/2)
  # 
  # limitOrders2  <- rep(-1,length(newRowList)) # SELL LIMIT ORDERS
  # limitPrices2  <- sapply(1:length(newRowList),function(i) 
  #   newRowList[[i]]$Close + spread[i]/2)
  # print(length(newRowList))
  # print(store$h[1:store$iter,s])
  print(marketOrders)
  print(info)
  print(real)
  print(predicted_close)
  print("****")
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}
#***********************init Function starts*************************
initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
# initOpStore  <- function(newRowList,series) {
#   OpStore <- matrix(0,nrow=maxRows,ncol=length(series))
#   return(OpStore)
# }
initHiStore  <- function(newRowList,series) {
  HiStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(HiStore)
}
initLoStore  <- function(newRowList,series) {
  LoStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(LoStore)
}
# initVoStore  <- function(newRowList,series) {
#   VoStore <- matrix(0,nrow=maxRows,ncol=length(series))
#   return(VoStore)
# }
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),h = initHiStore(newRowList,series),l = initLoStore(newRowList,series)))
}
#***********************init Function ends*************************





#***********************update Function starts*************************
# updateOpStore <- function(OpStore, newRowList, series, iter) {
#   for (i in 1:length(series))
#     OpStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Open)
#   return(OpStore)
# }
updateHiStore <- function(HiStore, newRowList, series, iter) {
  for (i in 1:length(series))
    HiStore[iter,i] <- as.numeric(newRowList[[series[i]]]$High)
  return(HiStore)
}
updateLoStore <- function(LoStore, newRowList, series, iter) {
  for (i in 1:length(series))
    LoStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Low)
  return(LoStore)
}
# updateVoStore <- function(VoStore, newRowList, series, iter) {
#   for (i in 1:length(series))
#     VoStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Volume)
#   return(VoStore)
# }
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  # store$o <- updateOpStore(store$o,newRowList,series,store$iter)
  store$h <- updateHiStore(store$h,newRowList,series,store$iter)
  store$l <- updateLoStore(store$l,newRowList,series,store$iter)
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)
  # store$v <- updateVoStore(store$v,newRowList,series,store$iter)
  return(store)
}
#***********************update Function ends*************************


