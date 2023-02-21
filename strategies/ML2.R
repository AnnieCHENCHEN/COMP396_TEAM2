# ML_2

library(rpart)
library(caret)

maxRows <- 3100


getOrders <- function(store, newRowList, currentPos, info, params){
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- -currentPos; pos <- allzero
  # for (i in 1:length(params$series)){
  i = 1
  all_cl = store$cl[1:store$iter,i]
  df <- data.frame(Close = c(all_cl))
  lags <- 5
  lagged_data <- data.frame(
    Close_1 = lag(df$Close, 1),
    Close_2 = lag(df$Close, 2),
    Close_3 = lag(df$Close, 3),
    Close_4 = lag(df$Close, 4),
    Close_5 = lag(df$Close, 5),
    Close = df$Close
  )
  
  # Train a decision tree model on the lagged data
  model <- rpart(Close ~ ., data = lagged_data)
  
  # Create a new data frame with the previous 3 closing prices for today
  if (nrow(df) >= lags){
  new_data <- data.frame(Close_1 = df$Close[length(df$Close)-4],
                         Close_2 = df$Close[length(df$Close)-3],
                         Close_3 = df$Close[length(df$Close)-2],
                         Close_4 = df$Close[length(df$Close)-1],
                         Close_5 = df$Close[length(df$Close)])
  predicted_close <- predict(model, new_data)
  # print(predicted_close)
  
  # print(store$cl[store$iter,1])
  if(predicted_close>store$cl[store$iter,i]){
    pos[params$series[i]] <- params$posSizes[params$series[i]]
  }else{
    pos[params$series[i]] <- -params$posSizes[params$series[i]]
  }
  }else {
    # Handle the case where df has fewer than 3 rows
  }
  # }
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