#This strategy is about Momentum strategy with MACD and Profit target


library(TTR)
library(xts)


maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

# MACD strategy
getOrders <- function(store, newRowList, currentPos, info, params){
  
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- -currentPos; pos <- allzero
  
  #Declare constants
  #nf = 12
  #ns = 26
  #m1 = 9
  count <- vector(mode = "numeric")
  buyMACD <- vector(mode="numeric")
  sellMACD <- vector(mode="numeric")
  
  
  #Extract close price for the series
  #Calculate DIFF and DEA
  for (i in 1:length(params$series)) {
    print(newRowList[[params$series[i]]])
    print(newRowList[[params$series[i]]]$Index)
    stock_use <- xts( newRowList[[params$series[i]]]$Close) 
    macd_data <- MACD(data=stock_use, nFast= 4, nSlow = 4,
                      nSig =9, maType = "EMA",percent= F)
    
  }



#Counter to assess when MACD line is less than signal line to identify crossover
j=0
for (i in 1:length(macd_data[,"macd"])) {
  j=j+1
  if(macd_data[i, "macd"]<macd_data[i,"singal"])
    
    count[j]= 1
  else
    
    count[j]=-1
}
print(count)

#Loop starts from 1 as the signal line 
#the first spike as a buy/sell signal

#buyMACD & sellMACD are all matrix
#buyMACD stores buy money, sellMACD stores sell money

#Enter the makret

for (a in 1:((length(count)-1))) {
  if((count[a]==-1)&(count[a+1]==1)) { # MACD line below to above across the signal line -> buy
    buyMACD <-cbind(buyMACD,a)
  }
  else if((count[a]==1)&(count[a+1]=-1)) { # MACD line above to below across the signal line -> sell
    sellMACD <-cbind(sellMACD,a)
  }
}

#Exit the market (profit traget & stop loss)--- 存疑



#MACD Returns Analysis
P = 200000 #initial amount

buyMACD <- cbind(buyMACD,length(cl))
sellMACD <- cbind(sellMACD, length(cl))
weightl <- vector(mode = "numeric")
weightsh <- vector(mode = "numeric")
weightl <- floor(P/cl[buyMACD])
weightsh <- floor(P/cl[sellMACD])


longAmt <- as.numeric(na.omit(cl[buyMACD]*weightl))
shortAmt <- as.numeric(na.omit(cl[sellMACD]*weightsh))

profitsh <- vector(mode="numeric")
profitl <- vector(mode="numeric")

#Profit = sell price – buy price
#Long position profit
for (i in 1:length(shortAmt)-1) {
  profitl[i] <- as.numeric((weightl[i]*cl[sellMACD[i+1]])-longAmt[i]) #Two transactions --open and close
}

#Short position profit
for (i in 1:length(shortAmt)) {
  profitsh[i] <- as.numeric(-(weightsh[i]*cl[buyMACD[i]])+shortAmt[i])
}

#Position sizing calculation 


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

