#RSI probabilities
library(TTR)
library(xts)
library(quantmod)
source(file.path("framework","data.R"))

dataList <- getData(directory="PART1") 
d3 <- dataList[[10]][1:1100,]

condPr_rsi <- matrix(0,nrow=nrow(d3)-lookback-1,ncol=3)
colnames(condPr_rsi) <- c("P(up)","P(down)","p(flat)")

lookback = 10
threshold = 10
tradDays <-  0

for (i in (lookback+2):nrow(d3)){
  start_Index <- i- lookback-1 #need 2 period
  rsi <- last(RSI(d3[start_Index:i, 4], n=lookback))
  
  if (rsi > (50 + 10)){
    condPr_rsi[start_Index,1] <- 1 # short
    tradDays <- tradDays+1
  }

  else if (rsi < (50 - 10)){
    condPr_rsi[start_Index,2] <- 1
    tradDays <- tradDays+1
    
  }else{
    condPr_rsi[start_Index,3] <- 1

  }
}
print(tradDays)

len <-  nrow(condPr_rsi)

up <- as.numeric(condPr_rsi[,1])
down <- as.numeric(condPr_rsi[,2])
mavg <- as.numeric(condPr_rsi[,3])

ProUp <- sum(up)/len
ProDown <- sum(down)/len
ProMavg <- sum(mavg)/len

print(ProUp)
print(ProDown)
print(ProMavg)
