# Machine Learning Method: ML.R
# @author: YWY

maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

# Load the necessary libraries for ML
library(rpart)
library(caret)

getOrders <- function(store, newRowList, currentPos, info, params) {
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  cur_Model <- trainModel(store)
  marketOrders <- -currentPos; pos <- allzero
  prediction <- predict(cur_Model,store)
  print(prediction)
  
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}


trainModel <- function(store){
  model <- rpart(store$Cl ~ ., data=store)
}



initStore <- function(newRowList,series) {
  return(list(iter=0,Op=initEachStore(newRowList,series)$OpStore,Hi=initEachStore(newRowList,series)$HiStore,Lo=initEachStore(newRowList,series)$LoStore,Cl=initEachStore(newRowList,series)$ClStore,Vo=initEachStore(newRowList,series)$VoStore))
}

initEachStore  <- function(newRowList,series) {
  OpStore <- matrix(0,nrow=maxRows,ncol=length(series))
  HiStore <- matrix(0,nrow=maxRows,ncol=length(series))
  LoStore <- matrix(0,nrow=maxRows,ncol=length(series))
  ClStore <- matrix(0,nrow=maxRows,ncol=length(series))
  Vostore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(list(OpStore,HiStore,LoStore,ClStore,Vostore))
}
updateOpStore <- function(OpStore, newRowList, series, iter) {
  for (i in 1:length(series))
    OpStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Open)
  return(OpStore)
}
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
updateVoStore <- function(VoStore, newRowList, series, iter) {
  for (i in 1:length(series))
    VoStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Volume)
  return(VoStore)
}
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}

updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$OpStore <- updateOpStore(store$OpStore,newRowList,series,store$iter)
  store$HiStore <- updateHiStore(store$HiStore,newRowList,series,store$iter)
  store$LoStore <- updateLoStore(store$LoStore,newRowList,series,store$iter)
  store$ClStore <- updateClStore(store$ClStore,newRowList,series,store$iter)
  store$Vostore <- updateVoStore(store$Vostore,newRowList,series,store$iter)
  return(store)
}