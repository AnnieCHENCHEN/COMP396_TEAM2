library(vars)
library(tseries)

maxRows <- 3100 


getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) 
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  pos <- allzero
  cp <- allzero
  # series x
  
  s=1
  if(store$iter>20){
  data <- data.frame(it = seq(1, store$iter), Open= store$o[1:store$iter,s],
                     High = store$h[1:store$iter,s],Low = store$l[1:store$iter,s],
                     Close = store$cl[1:store$iter,s],Vol = store$v[1:store$iter,s] )
  
  data_ts <- ts(data[, 2:6], start = 1, end = length(data$Close), frequency = 1)
  adf_result <- adf.test(data_ts[, 1], alternative = "stationary")
  
  # If data is not stationary, apply differencing
  if (adf_result$p.value > 0.05) {
    diff_data_ts <- diff(data_ts)
  } else {
    diff_data_ts <- data_ts
  }
  
  # Estimate the order of the VAR model using AIC
  var_order <- VARselect(data_ts,  type = "const")
  
  
  # Fit the VAR model to the data
  var_model <- VAR(data_ts, p = var_order$selection["AIC"], type = "const")
  
  # Print the summary of the model
  summary(var_model)
  }
  
  
  
  
  marketOrders <- pos
  
  
  return(list(store=store,marketOrders=allzero,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}



generate_trading_signals <- function(var_model, data) {
  # Analyze the forecast error variance decomposition
  fevd <- fevd(var_model)
  
  # Extract the forecast error variance of the "close" variable
  close_fevd <- fevd$`Close`$var
  
  # Calculate the rolling sum of the forecast error variance for the last 10 periods
  close_fevd_rolling_sum <- rollapply(close_fevd, width = 10, sum)
  
  # Generate trading signals based on the forecast error variance
  trading_signals <- ifelse(close_fevd_rolling_sum > 0.5, 1, 0)
  
  # Return the trading signals
  return(trading_signals)
}

#***********************init Function starts*************************
initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
initOpStore  <- function(newRowList,series) {
  OpStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(OpStore)
}
initHiStore  <- function(newRowList,series) {
  HiStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(HiStore)
}
initLoStore  <- function(newRowList,series) {
  LoStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(LoStore)
}
initVoStore  <- function(newRowList,series) {
  VoStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(VoStore)
}
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),
              h = initHiStore(newRowList,series),l = initLoStore(newRowList,series),
              o = initOpStore(newRowList,series),v = initVoStore(newRowList,series)))
}
#***********************init Function ends*************************





#***********************update Function starts*************************
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
  store$o <- updateOpStore(store$o,newRowList,series,store$iter)
  store$h <- updateHiStore(store$h,newRowList,series,store$iter)
  store$l <- updateLoStore(store$l,newRowList,series,store$iter)
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)
  store$v <- updateVoStore(store$v,newRowList,series,store$iter)
  return(store)
}
#***********************update Function ends*************************


