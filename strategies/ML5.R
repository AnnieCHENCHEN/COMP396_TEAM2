library(stream)
library(partykit)
library(rpart)


maxRows <- 3100 


getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) 
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  pos <- allzero
  
  # series x
  s=7
  
  #use decision tree to train model
  train_data <- data.frame(it=data.frame(it = seq(1, store$iter)), 
                           Close = store$cl[1:store$iter,s]) 
  model <- rpart(Close ~ .,data = train_data)
  new_instance <- data.frame(it =store$iter+1)
  
  #use model to predict
  predicted_close <- predict(model, new_instance)
  
  #if it tends to go up, go long
  if (predicted_close>train_data$Close[store$iter]) {
    pos[params$series[s]] <- 1
  }
  #if it tends to go down, go short
  else if (predicted_close<train_data$Close[store$iter]){
    pos[params$series[s]] <- -1
  }
  # else do nothing
  else{
    pos[params$series[s]] <- 0
  }
  
  marketOrders <- pos
  #print(marketOrders)
  #print(info$balance)
  
  print("********************************")
  res = getPositionSize(newRowList=newRowList,series=s,currentPos=currentPos,pos=pos, params=params,aclose=train_data$Close,s=s)
  print(res)
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=res$TradePositions,limitPrices1=res$TradePrices,
              limitOrders2=allzero,limitPrices2=allzero))
}
#***********************calculate position sizes*************************
getPositionSize <- function(newRowList,series,currentPos,pos, params,aclose,s){
  initialFund = 500000
  allzero  <- rep(0,length(newRowList)) 
  TradePositions <- allzero
  TradePrices <- allzero
  largestClose <- max(aclose)
  posThreshold <- round(initialFund/largestClose)
  print(posThreshold)
  #for (i in 1:length(params$series[se])){
    if (currentPos[s] <= posThreshold && currentPos[s]>=0){
      TradePositions[s] = pos[params$series[s]] * currentPos[s]
      TradePrices[s] = closes[s]
    }else {
      TradePositions[s]=0
      TradePrices[s]=0
    }
    
  #}
  return(list(TradePositions, TradePrices))
  
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
initShareStore <- function(newRowList, series){
  ShareStore <- matrix(0, nrow = maxRows, ncol = length(series))
  return(ShareStore)
}
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),h = initHiStore(newRowList,series),l = initLoStore(newRowList,series),
              units=initShareStore(newRowList,series)))
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


