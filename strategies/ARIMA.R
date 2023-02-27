library(forecast)


maxRows <- 3100 


getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) 
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  pos <- allzero
  # ,Open = store$o[1:store$iter,s],
  # High = store$h[1:store$iter,s],Low = store$l[1:store$iter,s],
  # Volume = store$v[1:store$iter,s]
  # series x
  s=1
  if(store$iter>10){
  data <- data.frame(it=data.frame(it = seq(1, store$iter)), 
                           Close = store$cl[1:store$iter,s]
                           ) 
  data_ts <- ts(data$Close, start = 1, end = length(data$Close), frequency = 1)
  fit <- arima(data_ts, order = c(1, 1, 1))
  forecast <- forecast(fit, h = 10)
  last_v <- tail(forecast$mean,1)
  up_ratio <- last_v/data$Close[store$iter]
  # 
  # #if it tends to go up, go long
  if (up_ratio>1&&currentPos<50&&info$balance>=500000) {
    pos[params$series[s]] <- 1
  }
  # #if it tends to go down, go short
  else if (up_ratio<1&&currentPos>0){
    pos[params$series[s]] <- -1
  }
  # # else do nothing
  else{
    pos[params$series[s]] <- 0
  }
  # 
  }
  marketOrders <- pos
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
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
  return(list(iter=0,cl=initClStore(newRowList,series)))
}
# ,h = initHiStore(newRowList,series),
# l = initLoStore(newRowList,series),o = initOpStore(newRowList,series),
# v = initVoStore(newRowList,series))
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
  # store$o <- updateOpStore(store$o,newRowList,series,store$iter)
  # store$h <- updateHiStore(store$h,newRowList,series,store$iter)
  # store$l <- updateLoStore(store$l,newRowList,series,store$iter)
  # store$v <- updateVoStore(store$v,newRowList,series,store$iter)
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)

  return(store)
}
#***********************update Function ends*************************


