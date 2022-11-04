source(file.path("framework","data.R"))

dataList <- getData(directory="PART1") 
d <- dataList[[2]][1:500,]

condPr <- matrix(0,nrow=nrow(d)-20,ncol=3)
colnames(condPr) <- c("P(up)","P(dowm)","P(flat)")

# lookback=20, short
for (i in 21: nrow(d)) {
  startIndex <- (i - 20)
  bbands <- last(BBands(d[startIndex:i,4],n=20,sd=0.5))
  
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
}
condPr <- round(condPr,2)
write.table(condPr, file="analyse_meanReversion.txt")
len <-  nrow(condPr)
up <- as.numeric(condPr[,1])
down <- as.numeric(condPr[,2])
mavg <- as.numeric(condPr[,3])
ProUp <- sum(up)/len
ProDown <- sum(down)/len
ProMavg <- sum(mavg)/len
print(ProUp)
print(ProDown)
print(ProMavg)

print(sum(up))
print(sum(down))
print(sum(mavg))



