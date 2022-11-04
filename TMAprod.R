library(TTR)
library(xts)
library(quantmod)
source(file.path("framework","data.R"))

dataList <- getData(directory="PART1") 
d2 <- dataList[[10]][1:500,]

condPr_TMA <- matrix(0,nrow=nrow(d2),ncol=2)
colnames(condPr_TMA) <- c("P(long)","P(short)")

sma_short <- rep(0,nrow(condPr_TMA))
sma_med <- rep(0,nrow(condPr_TMA))
sma_long <- rep(0,nrow(condPr_TMA))

#ret = list(short = 0, medium = 0, long = 0)
#lookback <- c("short" = 10, "medium" = 60, "long" = 150)


for (i in 6: nrow(condPr_TMA)) {
  index <- i-5
  sma_short[i] <- last(SMA(d2[index:i,4], n = 5))
  
  if(i >=11){
    startIndex <- i - 10
    sma_med[i]<- as.numeric(last(SMA(d2[startIndex:i,4], n = 10)))
    #print(sma_med)
  }
  if(i >=21){
    Index <- i - 20
    sma_long[i]<- as.numeric(last(SMA(d2[Index:i,4], n = 20)))
    #print(sma_long)
  
    if (sma_short[i] >sma_med[i] && sma_med[i] > sma_long[i]){ #long
      condPr_TMA[i,1] <- 1 #long
    
     }else if (sma_short[i]<sma_med[i] && sma_med[i] < sma_long[i]){ #short
       condPr_TMA[i,2] <- 1 #short
     }
  }
    
}

len <-  nrow(condPr_TMA)-20

long <- as.numeric(condPr_TMA[,1])
short <- as.numeric(condPr_TMA[,2])

Prolong <- sum(long)/len
Proshort <- sum(short)/len
  
print(Prolong)
print(Proshort)

