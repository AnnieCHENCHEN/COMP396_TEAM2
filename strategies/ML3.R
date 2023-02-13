# ML_2

library(rpart)
library(caret)
library(xts)

maxRows <- 3100


getOrders <- function(store, newRowList, currentPos, info, params){
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- -currentPos; pos <- allzero
  # i means which series
  i = 1
  
  lags <- 4
  all_cl = store$cl[1:store$iter,i]
  df <- data.frame(Close = c(all_cl))
  if (nrow(df) >= lags){
    # Create a new data frame with the previous lags closing prices for today
    
    column_names <- paste0("Close_", 1:lags)
    column_lags <- rev(seq_len(lags))
    Close_lags <- lapply(column_lags, function(x) lag(df$Close, x))
    lagged_data <- data.frame(setNames(Close_lags, column_names), Close = df$Close[length(df$Close) - (lags-1):length(df$Close)])
    # Train a decision tree model on the lagged data
    model <- rpart(Close ~ ., data = lagged_data)
    
    # Create a new data frame with the previous lags closing prices for today
    new_data <- data.frame(
      sapply(rev(paste0("Close_", 1:lags)), function(colname) {
        lagged_col <- lag(df[[colname]], 1)
        if (!is.null(lagged_col)) {
          as.xts(lagged_col)
        }
      }),
      Close = df$Close[length(df$Close) - lags + 1]
    )
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