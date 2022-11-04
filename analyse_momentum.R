library(TTR)
library(xts)
library(quantmod)
source(file.path("framework","data.R"))

dataList <- getData(directory="PART1") 
d <- dataList[[2]][1:500,]

condPr <- matrix(0,nrow=nrow(d),ncol=2)
colnames(condPr) <- c("P(short)","P(long)")

ret = list(short = 0, medium = 0, long = 0)
lookback <- c("short" = 10, "medium" = 60, "long" = 150)

# sma_short <- last(SMA(d[,4], n = 10))
# #print(sma_short)
# sma_med<- last(SMA(d[,4], n = 60))
# #print(sma_med)
# sma_long<- SMA(d[,4], n = 150)
# #print(nrow(sma_long))

for (i in 11: nrow(condPr)) {
  index <- i-10
  sma_short[index] <- last(SMA(d[index:i,4], n = 10))
  
  #print(sma_short)
  if(i >=61){
    startIndex <- i - 60
    sma_med[startIndex] <- as.numeric(last(SMA(d[startIndex:i,4], n = 60)))
    #print(sma_med)
  }
  if(i >=151){
    Index <- i - 150
    sma_long[Index] <- as.numeric(last(SMA(d[Index:i,4], n = 150)))
    #print(sma_long)
    
    if (sma_short[i] >sma_med[i] && sma_med[i] > sma_long[i]){ #long
      condPr[i,1] <- 1

     }else if (sma_short[i,1]<sma_med[i,1] && sma_med[i,1] < sma_long[i,1]){ #short
       condPr[i,2] <- 1
     }
  }
  
}
print(sma_short)
print(length(sma_med))
print(length(sma_long))


