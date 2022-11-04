source(file.path("framework","data.R"))

dataList <- getData(directory="PART1") 
d <- dataList[[2]][1:500,]

lookback = 20

condPr <- matrix(0,nrow=nrow(d)-lookback,ncol=3)
colnames(condPr) <- c("P(up)","P(dowm)","P(flat)")

condPr2 <- matrix(0,nrow=nrow(d)-lookback,ncol=3)
colnames(condPr2) <- c("P(uu)","P(ud)","P(dd)","p(du)")

# lookback=20, short
# up up > lost
# down down > profit
for (i in (lookback+1): nrow(d)) {
  startIndex <- (i - lookback)
  bbands <- last(BBands(d[startIndex:i,4],n=lookback,sd=0.5))
  
  if (d[i,4] < bbands[,"dn"]) {
    # if close is relatively low go long (i.e., contrarian type)
    condPr[startIndex,1] <- 1
  }
  else if (d[i,4] > bbands[,"up"]) {
    
    # if close is relatively high go short (again, contrarian type)
    condPr[startIndex,2] <- 1
    
  }else{
    
    condPr[startIndex,3] <- 1
  }
  
  if (condPr[startIndex,1]== 1) {
    if (as.numeric(d[startIndex+1,4] >= d[startIndex,4])){ #1 True
      
      condPr2[startIndex,1] <- 1 # up up
  }else{
    condPr2[startIndex,2] <- 1 # up down
  }
  
  }
  
  if (condPr[startIndex,2]== 1){
    if (as.numeric(d[startIndex+1,4] > d[startIndex,4])){ #1 True
      
      condPr2[startIndex,4] <- 1 # down up
    }else{
      condPr2[startIndex,3] <- 1 # down down
    }
  }
}


len <-  nrow(condPr2)

uu <- as.numeric(condPr2[,1])
ud <- as.numeric(condPr2[,2])
dd <- as.numeric(condPr2[,3])
du <- as.numeric(condPr2[,4])

ProUU <- sum(uu)/len
ProUD <- sum(ud)/len
ProDD <- sum(dd)/len
ProDU <- sum(du)/len

print(ud)


