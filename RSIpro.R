#RSI probabilities
library(TTR)
library(xts)
library(quantmod)
source(file.path("framework","data.R"))

dataList <- getData(directory="PART1") 
d3 <- dataList[[10]][1:500,]

condPr_rsi <- matrix(0,nrow=nrow(d3)-21,ncol=3)
colnames(condPr_rsi) <- c("P(up)","P(down)","p(flat)")

lookback = 20
threshold = 10

for (i in 22:nrow(d3)){
  start_Index <- i- lookback-1 #need 2 period
  rsi <- last(RSI(d3[start_Index:i, 4], n=20))
  
  if (rsi > (50 + 10)){
    condPr_rsi[start_Index,1] <- 1 # short
  }

  else if (rsi < (50 - 10)){
    condPr_rsi[start_Index,2] <- 1
    
  }else{
    condPr_rsi[start_Index,3] <- 1
  }
}

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
