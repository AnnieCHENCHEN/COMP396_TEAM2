source(file.path("framework","data.R"))

dataList <- getData(directory="PART1") 
d <- dataList[[1]][1:500,]

lookback <- 20
sdParams <- 0.5

condPr <- matrix(0,nrow=nrow(d)-lookback,ncol=2)
colnames(condPr) <- c("P(up)","P(dowm)")

condPr2 <- matrix(0,nrow=nrow(d)-lookback,ncol=4)
colnames(condPr2) <- c("P(uu)","P(ud)","P(dd)","p(du)")

# lookback=20, short
for (i in (lookback+1): nrow(d)) {
  startIndex <- (i - lookback)
  bbands <- last(BBands(d[startIndex:i,4],n=lookback,sd=sdParams))
  
  if (d[i,4] < bbands[,"dn"]) {
    # if close is relatively low go long (i.e., contrarian type)
    condPr[startIndex,1] <- 1
  }
  else if (d[i,4] > bbands[,"up"]) {
    # if close is relatively high go short (again, contrarian type)
    condPr[startIndex,2] <- 1
  }
  
  if(condPr[startIndex,1] == 1 && i < nrow(d)-1){ # P(up)
    if(as.numeric(d[i+1,4]) >= as.numeric(d[i,4])){
      
      condPr2[startIndex,1] <- 1 #up up
    }else{
      
      condPr2[startIndex,2] <- 1
    }
  }
  if(condPr[startIndex,2]== 1 && i <= nrow(d)-1){ # P(down)
    if (as.numeric(d[i+1,4]) >= as.numeric(d[i,4])){ #1 True

      condPr2[startIndex,4] <- 1 # down up
    }else{

      condPr2[startIndex,3] <- 1 # down down
    }
  }
}
#write.table(condPr, file="analyse_meanReversion.txt")

#print(condPr2)

up <- as.numeric(condPr[,1])
down <- as.numeric(condPr[,2])

uu <- as.numeric(condPr2[,1])
ud <- as.numeric(condPr2[,2])
dd <- as.numeric(condPr2[,3])
du <- as.numeric(condPr2[,4])

ProUU <- sum(uu)/sum(up)  # sum(uu)/sum(up)
ProUD <- sum(ud)/sum(up)   # sum(ud)/sum(up)  #    the probability of making money
ProDD <- sum(dd)/sum(down)   # sum(dd)/sum(down)
ProDU <- sum(du)/sum(down)   # sum(du)/sum(down)   #the probability of making money

print(ProUU)
print(ProUD)
print(ProDD)
print(ProDU)






