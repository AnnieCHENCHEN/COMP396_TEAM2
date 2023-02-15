
require(TTR)
maxRows <- 3100

getOrders <- function(store, newRowList, currentPos, info, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- -currentPos
  pos <- allzero
  
  size = 0.01
  maxUnits = 4
  Units=0
  verbose=TRUE
  
  
  
  
  

}
initIndicatorStore <- function(newRowList,series,iter){
  iter = 0
  Ind_store <- list(iter=iter+1, Min_Entry1=initClStore(newRowList,series), 
                    Max_Entry1=initClStore(newRowList,series), Min_Entry2=initClStore(newRowList,series),
                    Max_Entry2=initClStore(newRowList,series), Min_Exit=initClStore(newRowList,series),
                    Max_Exit=initClStore(newRowList,series), N=initClStore(newRowList,series))
  return(Ind_store)
}

getIndicatorsAndATR <- function(newRowList,series,Ind_store,store,params){
  
  Ind_store$Min_Entry1 <- runMin(store$Lo, params$periods[2])
  Ind_store$Max_Entry1 <- runMax(store$Hi, params$periods[2])
  
  Ind_store$Min_Entry2 <- runMin(store$Lo, params$periods[3])
  Ind_store$Max_Entry2 <- runMax(store$Hi, params$periods[3])
  
  Ind_store$Min_Exit <- runMin(store$Lo, params$periods[1])
  Ind_store$Max_Exit <- runMax(store$Hi, params$periods[1])
  
  Ind_store$N <- ATR(store[,c(2,3,4)], n=params$periods[2], maType=EMA, wilder=TRUE)[,'atr']
  
  return(Ind_store)
}

# to get high and low
initHiStore  <- function(newRowList,series) {
  HiStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(HiStore)
}
initLoStore  <- function(newRowList,series) {
  LoStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(LoStore)
}
#to get high price and low price of the stock
getHighprice <- function(HiStore, newRowList, series, iter){
  for (i in 1:length(series))
    HiStore[iter,i] <- as.numeric(newRowList[[series[i]]]$High)
  return(HiStore)
}
getLowprice <- function(LoStore, newRowList, series, iter){
  for (i in 1:length(series))
    LoStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Low)
  return(LoStore)
}
# for close price
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
  return(list(iter=0,cl=initClStore(newRowList,series), Hi=initHiStore(newRowList,series), 
              Lo=initLoStore(newRowList,series), MinEn1 = initClStore(newRowList,series)))
  #(list 里面存了iter代表天数和cl--一个matrix用来存close price)
}
#store里面存了close, high, low price
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$Hi <- getHighprice(store$Hi,newRowList,series,store$iter)
  store$Lo <- getLowprice(store$Lo,newRowList,series,store$iter)
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)

  return(store)
}


